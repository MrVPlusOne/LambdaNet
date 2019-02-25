package infer

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff._
import funcdiff.SimpleMath.Extensions._
import gtype.{AnyType, GType, TyVar}
import infer.IR.{IRType, IRTypeId}
import infer.PredicateGraph._

import scala.collection.mutable

object GraphEmbedding {

  val unknownTypeSymbol = 'UNKNOWN

  case class EmbeddingCtx(
    idTypeMap: Map[IR.IRTypeId, IRType],
    libraryTypeMap: Symbol => CompNode,
    predicates: Seq[TyVarPredicate],
    labelMap: Symbol => CompNode,
    fieldKnowledge: Map[Symbol, (CompNode, CompNode)]
  )

  case class Embedding(nodeMap: Map[IR.IRTypeId, CompNode])

  case class DecodingCtx(
    concreteTypes: IS[GType],
    projectTypes: IS[IRType]
  ) {
    private val indexMap =
      (concreteTypes
        ++ projectTypes.map { t =>
          assert(t.name.nonEmpty, s"project type has no name: $t")
          TyVar(t.name.get)
        }).zipWithIndex.toMap

    val maxIndex: Int = indexMap.size

    def indexOfType(t: GType): Int = {
      indexMap.getOrElse(t, indexMap(TyVar(unknownTypeSymbol)))
    }

    def typeOfIndex(i: Int): Either[GType, IRType] = {
      if (i < concreteTypes.length)
        Left(concreteTypes(i))
      else Right(projectTypes(i - concreteTypes.length))
    }
  }
}

import GraphEmbedding._
import API._

case class GraphEmbedding(
  ctx: EmbeddingCtx,
  layerFactory: LayerFactory,
  dimMessage: Int,
  forwardInParallel: Boolean
) {
  import ctx._
  import layerFactory._

  require(dimMessage % 2 == 0, "dimMessage should be even")

  val nodeInitVec: CompNode =
    getVar('nodeInitVec)(TensorExtension.randomUnitVec(dimMessage)) // randomVec()

  /**
    * @return A prediction distribution matrix of shape (# of places) * (# of candidates)
    */
  def encodeAndDecode(
    iterations: Int,
    decodingCtx: DecodingCtx,
    placesToDecode: IS[IRTypeId],
    iterateLogger: IS[Embedding] => Unit = _ => ()
  ): CompNode = {
    val init = Embedding(idTypeMap.mapValuesNow(_ => nodeInitVec))
    val embeddings = IS.iterate(init, iterations)(iterate)
    iterateLogger(embeddings)
    decode(decodingCtx, placesToDecode, embeddings.last)
  }

  /** Each message consists of a key-value pair */
  type Message = (CompNode, CompNode)

  def messageModel(name: SymbolPath, vec: CompNode): Message = {
    relu(linear(name / 'header, dimMessage)(vec)) -> relu(
      linear(name / 'content, dimMessage)(vec)
    )
  }

  def singleLayer(name: SymbolPath, input: CompNode): CompNode = {
    linear(name / 'linear, dimMessage)(input) ~> relu
  }

  def binaryMessage(name: SymbolPath, v1: CompNode, v2: CompNode): Message = {
    singleLayer(name / 'header, v1.concat(v2, axis = 1)) ->
      singleLayer(name / 'content, v1.concat(v2, axis = 1))
  }

  def fieldAccessMessage(
    name: SymbolPath,
    objEmbed: CompNode,
    fieldLabel: Symbol
  ): Message = {
    val input = objEmbed.concat(labelMap(fieldLabel), axis = 1)
    messageModel(name, singleLayer(name / 'compress, input))
  }

  def argAccessMessage(name: SymbolPath, fEmbed: CompNode, argId: Int): Message = {
    val input = fEmbed.concat(positionalEncoding(argId), axis = 1)
    messageModel(name, singleLayer(name / 'compress, input))
  }

//  def messageModel2(name: SymbolPath, key: CompNode, value: CompNode): Message = {
//    linear(name / 'header, dimMessage)(key) -> linear(name / 'content, dimMessage)(
//      value
//    )
//  }

  def randomVec(): Tensor = {
    numsca.randn(1, dimMessage) * 0.01
  }

  def positionalEncoding(pos: Int): CompNode = {
    assert(pos >= -1)
    if (pos == -1)
      getVar('position / 'head) { randomVec() } else {
      getConst('position / Symbol(pos.toString)) {
        val phases = (0 until dimMessage / 2).map { dim =>
          pos / math.pow(1000, 2.0 * dim / dimMessage)
        }
        numsca.Tensor(phases.map(math.sin) ++ phases.map(math.cos): _*).reshape(1, -1)
      }
    }
  }

  def iterate(
    embedding: Embedding
  ): Embedding = {
    import funcdiff.API._
    import embedding._

    val _messages = mutable.HashMap[IRTypeId, mutable.ListBuffer[Message]]()
    ctx.idTypeMap.keys.foreach(id => _messages(id) = mutable.ListBuffer())

    def messages(id: IRTypeId): MessageChannel = MessageChannel(_messages(id))

    case class MessageChannel(receiver: mutable.ListBuffer[Message]) {
      def +=(msg: Message): Unit = receiver.synchronized {
        receiver.append(msg)
      }
    }

    type ObjType = IRType
    val fieldDefs = mutable.HashMap[Symbol, IS[(ObjType, IRType)]]()
    val fieldUsages = mutable.HashMap[Symbol, IS[(ObjType, IRType)]]()
    ctx.predicates.collect {
      case DefineRel(v, ObjLiteralTypeExpr(fields)) =>
        fields.foreach {
          case (l, t) =>
            fieldDefs(l) = fieldDefs.getOrElse(l, IS()) :+ (v, t)
        }
      case DefineRel(v, FieldAccessTypeExpr(objType, l)) =>
        fieldUsages(l) = fieldUsages.getOrElse(l, IS()) :+ (objType, v)
    }

    /* for each kind of predicate, generate one or more messages */
    def sendPredicateMessages(predicate: TyVarPredicate): Unit = predicate match {
      case EqualityRel(v1, v2) =>
        messages(v1.id) += messageModel('Equality, nodeMap(v2.id))
        messages(v2.id) += messageModel('Equality, nodeMap(v1.id))
      case FreezeType(v, ty) =>
        messages(v.id) += messageModel('FreezeType, encodeGType(ty))
      case HasName(v, name) =>
//        messages(v.id) += messageModel('HasName, labelMap(name)) //todo: properly handle name info
      case SubtypeRel(sub, sup) =>
//        messages(sub.id) += messageModel('HasSupertype, nodeMap(sup.id))
//        messages(sup.id) += messageModel('HasSubtype, nodeMap(sub.id))
        messages(sub.id) += binaryMessage('SubtypeRel, nodeMap(sub.id), nodeMap(sup.id))
        messages(sup.id) += binaryMessage('SubtypeRel, nodeMap(sup.id), nodeMap(sub.id))
      case AssignRel(lhs, rhs) =>
//        messages(lhs.id) += messageModel('AssignedFrom, nodeMap(rhs.id))
//        messages(rhs.id) += messageModel('AssignedTo, nodeMap(lhs.id))
        messages(lhs.id) += binaryMessage('AssignRel, nodeMap(lhs.id), nodeMap(rhs.id))
        messages(rhs.id) += binaryMessage('AssignRel, nodeMap(rhs.id), nodeMap(lhs.id))
      case UsedAsBoolean(tyVar) =>
        messages(tyVar.id) += messageModel('UsedAsBoolean, nodeMap(tyVar.id))
      case InheritanceRel(child, parent) =>
        messages(child.id) += messageModel('DeclaredAsSubtype, nodeMap(parent.id))
        messages(parent.id) += messageModel('DeclaredAsSupertype, nodeMap(child.id))
      case DefineRel(v, expr) =>
        expr match {
          case FuncTypeExpr(argTypes, returnType) =>
            val ids = (returnType +: argTypes.toIndexedSeq).map(_.id)
            ids.zipWithIndex.foreach {
              case (tId, i) =>
                val argId = i - 1
                messages(v.id) += argAccessMessage(
                  'FuncTypeExpr / 'toF,
                  nodeMap(tId),
                  argId
                )
                messages(tId) += argAccessMessage(
                  'FuncTypeExpr / 'toArg,
                  nodeMap(v.id),
                  argId
                )
            }
          case CallTypeExpr(f, args) =>
            val fVec = nodeMap(f.id)
            val fKey = singleLayer('CallTypeExpr / 'fKey, fVec)
            val fPair = messageModel('CallTypeExpr / 'fPair, fVec)

            val argPairs = args.toIndexedSeq.zipWithIndex.map {
              case (argT, argId) =>
                argAccessMessage(
                  'CallTypeExpr / 'toArg,
                  nodeMap(argT.id),
                  argId
                )
            }

            val fEmbed = attentionLayer('CallTypeExpr / 'fEmbed, dimMessage)(
              fKey,
              fPair +: argPairs
            ) //todo: improve this
            messages(v.id) += messageModel('CallTypeExpr / 'toV, fEmbed)
            args.zipWithIndex.foreach {
              case (arg, argId) =>
                messages(arg.id) += argAccessMessage(
                  'CallTypeExpr / 'toArg,
                  fEmbed,
                  argId
                )
            }
          case ObjLiteralTypeExpr(fields) =>
            fields.foreach {
              case (label, tv) =>
                messages(v.id) += fieldAccessMessage(
                  'ObjLiteralTypeExpr / 'toV,
                  nodeMap(tv.id),
                  label
                )
                messages(tv.id) += fieldAccessMessage(
                  'ObjLiteralTypeExpr / 'toField,
                  nodeMap(v.id),
                  label
                )
                if (fieldUsages.contains(label)) {
                  messages(tv.id) += {
                    val att =
                      attentionLayer('ObjLiteralTypeExpr / 'fieldUsage, dimMessage)(
                        nodeMap(v.id),
                        fieldUsages(label).map {
                          case (k, n) => nodeMap(k.id) -> nodeMap(n.id)
                        }
                      )
                    messageModel('ObjLiteralTypeExpr / 'usageMessage, att)
                  }
                }
            }
          case FieldAccessTypeExpr(objType, label) =>
            messages(v.id) += fieldAccessMessage(
              'FieldAccess / 'toV,
              nodeMap(objType.id),
              label
            )
            messages(objType.id) += fieldAccessMessage(
              'FieldAccess / 'toObj,
              nodeMap(v.id),
              label
            )
            if (fieldDefs.contains(label)) {
              messages(v.id) += {
                val att = attentionLayer('FieldAccess / 'defs, dimMessage)(
                  nodeMap(objType.id),
                  fieldDefs(label)
                    .map { case (k, n) => nodeMap(k.id) -> nodeMap(n.id) } ++
                    fieldKnowledge.get(label).toIndexedSeq
                )
                messageModel('FieldAccess / 'defsMessage, att)
              }
            }
        }
    }

    ctx.predicates.par.foreach(sendPredicateMessages)

    val newNodeMap = _messages.keys.par
      .map { id =>
        val nodeKey1 = singleLayer('MessageAggregate / 'nodeKey1, nodeMap(id))
        val nodeKey2 = singleLayer('MessageAggregate / 'nodeKey2, nodeMap(id))
//        val nodePair = messageModel('MessageAggregate / 'nodeMessage, nodeMap(id))
        val out = attentionLayer('MessageAggregate, dimMessage)(
          nodeKey1,
          _messages(id).toIndexedSeq :+ (nodeKey2, nodeMap(id))
        )
        val newEmbed = out / sqrt(sum(square(out)))
//      val newEmbed = gru('MessageAggregate / 'updateGru)(nodeVec, change)
        id -> newEmbed
      }
      .seq
      .toMap
    Embedding(newNodeMap)
  }

  /**
    * @return A prediction distribution logits matrix of shape (# of places) * (# of candidates)
    */
  def decode(
    decodingCtx: DecodingCtx,
    placesToDecode: IS[IRTypeId],
    embedding: Embedding
  ): CompNode = {

    val attentionHeads = 16
    val concretes = decodingCtx.concreteTypes.map(encodeGType)

    val certainty = getVar(Symbol("decode:certainty"))(Tensor(10.0))
    val logits = for (head <- (0 until attentionHeads).par) yield {
      val shrinkFactor = 2
      def mlp(rows: IS[CompNode], layerName: String, layers: Int): CompNode = {
        var input = concatN(rows, axis = 0)
        var dim = dimMessage
        for (i <- 0 until layers) {
          dim /= shrinkFactor
          input = input ~>
            //          batchNorm(Symbol(s"$layerName-BN$i"), inTraining = true) ~>
            linear(Symbol(s"$layerName$i") / Symbol(s"head$head"), dim) ~>
            relu
        }
        input
      }

      import embedding._

      val candidateEmbeddings = {
        val c =
          mlp(concretes, "decode:concreteType", 2)
        val p =
          mlp(decodingCtx.projectTypes.map(t => nodeMap(t.id)), "decode:projectType", 2)
        c.concat(p, axis = 0)
      }

      val transformedNodes = mlp(placesToDecode.map(embedding.nodeMap), "decode:nodes", 2)
      transformedNodes.dot(candidateEmbeddings.t) * (certainty * 6.0 / dimMessage)
    }
    total(logits.seq.toVector) ~> relu
  }

  private val gTypeEmbeddingMap = mutable.HashMap[GType, CompNode]()
  def encodeGType(t: GType): CompNode =
    gTypeEmbeddingMap.getOrElseUpdate(
      t, {
        import gtype._
        import API._

        val funcInitKey = getVar('funcType / 'initKey)(randomVec())
        val funcInitValue = getVar('funcType / 'initValue)(randomVec())

        val objectInitKey = getVar('objType / 'initKey)(randomVec())
        val objectInitValue = getVar('objType / 'initValue)(randomVec())

        def rec(t: GType): CompNode = t match { //todo: need better ways
          case AnyType     => getVar('anyType)(numsca.randn(1, dimMessage) * 0.01)
          case TyVar(name) => libraryTypeMap(name)
          case FuncType(args, to) =>
            val messages = (to +: args).zipWithIndex.map {
              case (t, i) =>
                argAccessMessage('funcType / 'arg, rec(t), i - 1)
            }.toIndexedSeq
            attentionLayer('funcType / 'aggregate, dimMessage)(
              funcInitKey,
              messages
            )
          case ObjectType(fields) =>
            val messages = fields.toIndexedSeq.map {
              case (label, t) =>
                fieldAccessMessage('objectType / 'field, rec(t), label)
            }
            attentionLayer('objectType / 'aggregate, dimMessage)(
              objectInitKey,
              messages
            )
        }

        rec(t)
      }
    )

}
