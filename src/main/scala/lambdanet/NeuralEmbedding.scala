package lambdanet

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff._
import lambdanet.NeuralEmbedding.VarEmbedding
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._

import scala.collection.{GenSeq, mutable}
import scala.collection.parallel.ForkJoinTaskSupport

object NeuralEmbedding {
  case class VarEmbedding(nodeMap: Map[PNode, CompNode])

}

class NeuralEmbedding(
    pc: ParamCollection,
    dimMessage: Int,
    taskSupport: Option[ForkJoinTaskSupport]
) {
  private val layerFactory = LayerFactory(
    SymbolPath.empty / 'NeuralEmbedding,
    pc
  )
  import layerFactory._

  require(dimMessage % 2 == 0, "dimMessage should be even")

  def embedNodes(
      graph: PredicateGraph,
      iterations: Int
  ): Vector[VarEmbedding] = {
    import cats.implicits._

    type ObjType = PNode

    //todo: encode this as predicates
    /** which label is defined in which class as what */
    var fieldDefs = Map[Symbol, Set[(ObjType, PNode)]]()

    /** which label is accessed on which variable as what */
    var fieldUsages = Map[Symbol, Set[(ObjType, PNode)]]()
    graph.predicates.collect {
      case DefineRel(v, PObject(fields)) =>
        fields.foreach {
          case (l, t) =>
            fieldDefs |+|= Map(l -> Set(v -> t))
        }
      case DefineRel(v, PAccess(objType, l)) =>
        fieldUsages |+|= Map(l -> Set(objType -> v))
    }

    val labelMap: Symbol => CompNode = ???

    def fieldAccessMessage(
        name: SymbolPath,
        objEmbed: CompNode,
        fieldLabel: Symbol
    ): Message = {
      val input = objEmbed.concat(labelMap(fieldLabel), axis = 1)
      messageModel(name, singleLayer(name / 'compress, input))
    }

    def iterate(embedding: VarEmbedding): VarEmbedding = {
      case class MessageChannel(node: PNode) {
        val receiver: mutable.Buffer[Message] = mutable.Buffer()
        def +=(msg: => Message): Unit =
          if (!node.fromLib) // only send message for var nodes
            receiver.synchronized {
              receiver.append(msg)
            }
      }

      def embed(node: PNode): CompNode = {
        if (node.fromLib) encodeLibType(PTyVar(node))
        else embedding.nodeMap(node)
      }

      val messages =
        graph.nodes
          .map(n => n -> MessageChannel(n))
          .toMap

      def messageMutually(n1: PNode, n2: PNode, name: Symbol): Unit = {
        messages(n1) += messageModel(name / 'n1, embed(n2))
        messages(n2) += messageModel(name / 'n2, embed(n1))
      }

      def sendMessages(predicate: TyPredicate): Unit = predicate match {
        case HasLibType(v, ty) =>
          messages(v) += messageModel('HasLibType, encodeLibType(ty))
        case SubtypeRel(sub, sup) =>
          messageMutually(sub, sup, 'SubtypeRel)
        case AssignRel(lhs, rhs) =>
          messageMutually(lhs, rhs, 'AssignRel)
        case UsedAsBool(n) =>
          messages(n) += messageModel('UsedAsBool, embed(n))
        case InheritanceRel(child, parent) =>
          messageMutually(child, parent, 'InheritanceRel)
        case DefineRel(v, expr) =>
          expr match {
            case n: PNode =>
              messageMutually(v, n, 'DefineRel)
            case PFunc(args, ret) =>
              //todo: improve
              val nodes = ret +: args
              nodes.zipWithIndex.foreach {
                case (tId, i) =>
                  val argId = i - 1
                  messages(v) += argAccessMessage(
                    'FuncTypeExpr_toF,
                    embed(tId),
                    argId
                  )
                  messages(tId) += argAccessMessage(
                    'FuncTypeExpr_toArg,
                    embed(v),
                    argId
                  )
              }
            case PCall(f, args) =>
              //todo: improve
              val fVec = embed(f)
              val fKey = singleLayer('CallTypeExpr / 'fKey, fVec)
              val fPair = messageModel('CallTypeExpr / 'fPair, fVec)

              val argPairs = args.zipWithIndex.map {
                case (argT, argId) =>
                  argAccessMessage(
                    'CallTypeExpr / 'embedArg,
                    embed(argT),
                    argId
                  )
              }

              val fEmbed = attentionLayer('CallTypeExpr / 'fEmbed)(
                fKey,
                fPair +: argPairs
              )
              messages(v) += messageModel('CallTypeExpr / 'toV, fEmbed)
              args.zipWithIndex.foreach {
                case (arg, argId) =>
                  messages(arg) += argAccessMessage(
                    'CallTypeExpr / 'toArg,
                    fEmbed,
                    argId
                  )
              }
            case PObject(fields) =>
              //todo: improve
              fields.foreach {
                case (label, tv) =>
                  messages(v) += fieldAccessMessage(
                    'ObjLiteralTypeExpr / 'toV,
                    embed(tv),
                    label
                  )
                  messages(tv) += fieldAccessMessage(
                    'ObjLiteralTypeExpr / 'toField,
                    embed(v),
                    label
                  )
                  if (fieldUsages.contains(label)) {
                    messages(tv) += {
                      val att =
                        attentionLayer(
                          'ObjLiteralTypeExpr / 'fieldUsage,
                          transformKey = true
                        )(
                          embed(v),
                          fieldUsages(label).toVector.map {
                            case (k, n) => embed(k) -> embed(n)
                          }
                        )
                      messageModel('ObjLiteralTypeExpr / 'usageMessage, att)
                    }
                  }
              }
            case PAccess(obj, label) => ???
          }
      }

      par(graph.predicates.toSeq).foreach(sendMessages)
      val newNodeMap =
        embedding.nodeMap.keys
          .map { t =>
            val node = embed(t)
            //        val nodePair = messageModel('MessageAggregate / 'nodeMessage, nodeMap(id))
            val out = attentionLayer('MessageAggregate, transformKey = true)(
              node,
              //todo: mix the messages with the old embedding
              messages(t).receiver.toIndexedSeq :+ (node, node)
            )
            val outLen = sqrt(sum(square(out)))
            val newEmbed = out / outLen
            //      val newEmbed = gru('MessageAggregate / 'updateGru)(nodeVec, change)
            t -> newEmbed
          }
          .seq
          .toMap
      VarEmbedding(newNodeMap)
    }

    val nodeInitVec: CompNode =
      getVar('nodeInitVec)(TensorExtension.randomUnitVec(dimMessage))
    val init = VarEmbedding(
      graph.nodes
        .filterNot(_.fromLib)
        .map(n => n -> nodeInitVec)
        .toMap
    )
    Vector.iterate(init, iterations + 1)(iterate)
  }

  private def par[T](xs: Seq[T]): GenSeq[T] = {
    taskSupport match {
      case None => xs
      case Some(ts) =>
        val r = xs.par
        r.tasksupport = ts
        r
    }
  }

  private def encodeLibType(pType: PType): CompNode = ???

  /** Each message consists of a key-value pair */
  type Message = (CompNode, CompNode)

  private def singleLayer(name: SymbolPath, input: CompNode): CompNode = {
    linear(name / 'linear, dimMessage)(input) ~> relu
  }

  private def messageModel(name: SymbolPath, vec: CompNode): Message = {
    relu(linear(name / 'header, dimMessage)(vec)) -> relu(
      linear(name / 'content, dimMessage)(vec)
    )
  }

//  private def binaryMessage(
//      name: SymbolPath,
//      v1: CompNode,
//      v2: CompNode
//  ): Message = {
//    singleLayer(name / 'header, v1.concat(v2, axis = 1)) ->
//      singleLayer(name / 'content, v1.concat(v2, axis = 1))
//  }

  private def argAccessMessage(
      name: SymbolPath,
      fEmbed: CompNode,
      argId: Int
  ): Message = {
    val input = fEmbed.concat(positionalEncoding(argId), axis = 1)
    messageModel(name, singleLayer(name / 'compress, input))
  }

  private def randomVec(): Tensor = {
    numsca.randn(1, dimMessage) * 0.01
  }

  private def positionalEncoding(pos: Int): CompNode = {
    assert(pos >= -1)
    if (pos == -1)
      getVar('position / 'head) { randomVec() } else {
      getConst('position / Symbol(pos.toString)) {
        val phases = (0 until dimMessage / 2).map { dim =>
          pos / math.pow(1000, 2.0 * dim / dimMessage)
        }
        numsca
          .Tensor(phases.map(math.sin) ++ phases.map(math.cos): _*)
          .reshape(1, -1)
      }
    }
  }
}
