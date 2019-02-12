package infer

import botkop.numsca
import funcdiff._
import funcdiff.SimpleMath.Extensions._
import gtype.GType
import infer.IR.{IRType, Id}
import infer.PredicateGraph._

import scala.collection.mutable

object GraphEmbedding {

  case class EmbeddingCtx(
    idTypeMap: Map[IR.Id, IRType],
    concreteTypeMap: GType => CompNode,
    predicates: Seq[TyVarPredicate],
    labelMap: Symbol => CompNode
  )

  case class Embedding(nodeMap: Map[IR.Id, CompNode])

  case class DecodingCtx(
    concreteTypes: IS[GType],
    projectTypes: IS[IR.Id]
  )
}

import GraphEmbedding._
import API._

case class GraphEmbedding(ctx: EmbeddingCtx, pc: ParamCollection, dimMessage: Int) {
  import ctx._
  val layerFactory: LayerFactory = LayerFactory('GraphEmbedding, pc)
  import layerFactory._

  require(dimMessage % 2 == 0, "dimMessage should be even")

  val initVec: CompNode =
    pc.getVar(nameSpace / 'initVec)(numsca.randn(1, dimMessage) * 0.01)

  /**
    * @return A prediction distribution matrix of shape (# of places) * (# of candidates)
    */
  def encodeAndDecode(
    iterations: Int,
    decodingCtx: DecodingCtx,
    placesToDecode: IS[Id]
  ): CompNode = {
    var embed = Embedding(idTypeMap.mapValuesNow(_ => initVec))
    for (_ <- 0 until iterations) {
      embed = iterate(embed)
    }
    decode(decodingCtx, placesToDecode, embed)
  }

  def iterate(
    embedding: Embedding
  ): Embedding = {
    import funcdiff.API._
    import embedding._

    /** Each message consists of a key-value pair */
    type Message = (CompNode, CompNode)

    val messages = mutable.HashMap[Id, mutable.ListBuffer[Message]]()
    ctx.idTypeMap.keys.foreach(id => messages(id) = mutable.ListBuffer())

    def messageModel(name: SymbolPath, vec: CompNode): Message = {
      linear(name / 'header, dimMessage)(vec) -> linear(name / 'content, dimMessage)(vec)
    }

    def messageModel2(name: SymbolPath, key: CompNode, value: CompNode): Message = {
      linear(name / 'header, dimMessage)(key) -> linear(name / 'content, dimMessage)(
        value
      )
    }

    def positionalEncoding(pos: Int): CompNode = {
      assert(pos >= 0)
      pc.getConst('position / Symbol(pos.toString)) {
        val phases = (0 until dimMessage / 2).map { dim =>
          pos / math.pow(1000, 2.0 * dim / dimMessage)
        }
        numsca.Tensor(phases.map(math.sin) ++ phases.map(math.cos): _*).reshape(1, -1)
      }
    }

    /* for each kind of predicate, generate one or more messages */
    def sendPredicateMessages(predicate: TyVarPredicate): Unit = predicate match {
      case EqualityRel(v1, v2) =>
        messages(v1.id) += messageModel('Equality, nodeMap(v2.id))
        messages(v2.id) += messageModel('Equality, nodeMap(v1.id))
      case FreezeType(v, ty) =>
        messages(v.id) += messageModel('FreezeType, concreteTypeMap(ty))
      case HasName(v, name) =>
        messages(v.id) += messageModel('HasName, labelMap(name))
      case SubtypeRel(sub, sup) =>
        messages(sub.id) += messageModel('HasSupertype, nodeMap(sup.id))
        messages(sup.id) += messageModel('HasSubtype, nodeMap(sub.id))
      case AssignRel(lhs, rhs) =>
        messages(lhs.id) += messageModel('AssignedFrom, nodeMap(rhs.id))
        messages(rhs.id) += messageModel('AssignedTo, nodeMap(lhs.id))
      case UsedAsBoolean(tyVar) =>
        messages(tyVar.id) += messageModel('UsedAsBoolean, nodeMap(tyVar.id))
      case InheritanceRel(child, parent) =>
        // information flow from parent to child only
        messages(child.id) += messageModel('DeclaredAsSubtype, nodeMap(parent.id))
        messages(parent.id) += messageModel('DeclaredAsSupertype, nodeMap(child.id))
      case DefineRel(v, expr) =>
        def decodeField(tId: Id, fieldLabel: Symbol): CompNode = {
          val input = nodeMap(tId).concat(labelMap(fieldLabel), axis = 1)
          linear('decodeField, dimMessage)(input)
        }

        def decodeArg(fEmbed: CompNode, argId: Int): CompNode = {
          val input = fEmbed.concat(positionalEncoding(argId), axis = 1)
          linear('decodeArg, dimMessage)(input)
        }

        def hasFieldMessage(name: SymbolPath, label: Symbol, tId: Id): Message = {
          messageModel2(name, labelMap(label), nodeMap(tId))
        }

        expr match {
          case FuncTypeExpr(argTypes, returnType) =>
            val ids = (argTypes.toIndexedSeq :+ returnType).map(_.id)
            ids.zipWithIndex.foreach {
              case (tId, argId) =>
                messages(v.id) += messageModel2(
                  'FuncTypeExpr,
                  positionalEncoding(argId),
                  nodeMap(tId)
                )
                messages(tId) += messageModel('Equality, decodeArg(nodeMap(v.id), argId))
            }
          case CallTypeExpr(f, args) =>
            /* Use attention-based weighted sum to compute the argument-aware function
               embedding (to perform generics-like reasoning) */
            val fVec = nodeMap(f.id)
            val fKey = linear('CallTypeExpr / 'fKey, dimMessage)(fVec)

            val (argKeys, argVecs) = args.toIndexedSeq.zipWithIndex.map {
              case (argT, argIdx) =>
                messageModel2(
                  'CallTypeExpr / 'arg,
                  positionalEncoding(argIdx),
                  nodeMap(argT.id)
                )
            }.unzip

            val fEmbed = attentionLayer('decodeFunc, dimMessage)(
              fKey -> fVec,
              (fKey -> fVec) +: (argKeys zip argVecs)
            )
            messages(v.id) += messageModel('CallTypeExpr / 'toV, fEmbed)
            args.zipWithIndex.foreach {
              case (arg, argId) =>
                messages(arg.id) += messageModel('Equality, decodeArg(fEmbed, argId))
            }
          case ObjLiteralTypeExpr(fields) =>
            fields.foreach {
              case (label, tv) =>
                messages(v.id) += messageModel2(
                  'ObjLiteralTypeExpr,
                  labelMap(label),
                  nodeMap(tv.id)
                )
                messages(tv.id) += messageModel(
                  'FieldAccess / 'toV,
                  decodeField(v.id, label)
                )
            }
          case FieldAccessTypeExpr(objType, label) =>
            messages(v.id) += messageModel(
              'FieldAccess / 'toV,
              decodeField(objType.id, label)
            )
            messages(objType.id) += hasFieldMessage('FieldAccess / 'toObject, label, v.id)
        }
    }

    ctx.predicates.foreach(sendPredicateMessages)

    val newNodeMap = messages.keys.map { id =>
      val nodeVec = nodeMap(id)
      val nodeKey = linear('MessageAggregate / 'nodeKey, dimMessage)(nodeVec)
      val newEmbed = attentionLayer('MessageAggregate, dimMessage)(
        nodeKey -> nodeVec,
        messages(id).toIndexedSeq
      ) //todo: try if using an RNN here can stabilize training
      id -> newEmbed
    }.toMap
    Embedding(newNodeMap)
  }

  /**
    * @return A prediction distribution matrix of shape (# of places) * (# of candidates)
    */
  def decode(
    decodingCtx: DecodingCtx,
    placesToDecode: IS[Id],
    embedding: Embedding
  ): CompNode = {
    def mlp(rows: IS[CompNode], layerName: String, layers: Int): CompNode = {
      var input = concatN(rows, axis = 0)
      for (i <- 0 until layers) {
        input = input ~> linear(Symbol(s"$layerName$i"), dimMessage) ~> relu
      }
      input
    }

    import embedding._

    val candidateEmbeddings = {
      val c =
        mlp(decodingCtx.concreteTypes.map(concreteTypeMap), "decode:concreteType", 2)
      val p = mlp(decodingCtx.projectTypes.map(nodeMap), "decode:projectType", 2)
      c.concat(p, axis = 0)
    }

    val transformedNodes = mlp(placesToDecode.map(embedding.nodeMap), "decode:nodes", 2)
    softmax(transformedNodes.dot(candidateEmbeddings.t)) //todo: try multi-head attention
  }

}
