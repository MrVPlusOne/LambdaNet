package lambdanet

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff._
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.{PAnnot, PredicateGraph}
import lambdanet.translation.PredicateGraph._

import scala.collection.{GenSeq, mutable}
import scala.collection.parallel.ForkJoinTaskSupport

object NeuralInference {}

class NeuralInference(
    graph: PredicateGraphWithCtx,
    layerFactory: LayerFactory,
    dimMessage: Int,
    taskSupport: Option[ForkJoinTaskSupport],
) {
  import layerFactory._

  require(dimMessage % 2 == 0, "dimMessage should be even")

  /** returns a matrix of shape [ nodes number, prediction space ] */
  def predictTypes(
      nodesToPredict: Vector[PNode],
      predictionSpace: PredictionSpace,
      iterations: Int,
  ): CompNode = {

    val labelMap: Symbol => CompNode = {
      val allLabels = graph.fieldDefs.keySet ++ graph.fieldUsages.keySet
      allLabels.map(_ -> (randomVec(): CompNode)).toMap
    }

    // todo: Implement batching
    def fieldAccessMessage(
        name: SymbolPath,
        objEmbed: CompNode,
        fieldLabel: Symbol,
    ): Message = {
      val input = objEmbed.concat(labelMap(fieldLabel), axis = 1)
      messageModel(name, singleLayer(name / 'compress, input))
    }

    case class VarEmbedding(nodeMap: Map[PNode, CompNode])

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
        if (node.fromLib) encodeLibType {
          if (node.isType) PTyVar(node)
          else graph.libraryTerms(node)
        } else embedding.nodeMap(node)
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
        case UsedAsBool(n) =>
          messages(n) += messageModel('UsedAsBool, embed(n))
        case FixedToType(n, ty) =>
          ???
        case SubtypeRel(sub, sup) =>
          messageMutually(sub, sup, 'SubtypeRel)
        case AssignRel(lhs, rhs) =>
          messageMutually(lhs, rhs, 'AssignRel)
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
                    argId,
                  )
                  messages(tId) += argAccessMessage(
                    'FuncTypeExpr_toArg,
                    embed(v),
                    argId,
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
                    argId,
                  )
              }

              val fEmbed = attentionLayer('CallTypeExpr / 'fEmbed)(
                fKey,
                fPair +: argPairs,
              )
              messages(v) += messageModel('CallTypeExpr / 'toV, fEmbed)
              args.zipWithIndex.foreach {
                case (arg, argId) =>
                  messages(arg) += argAccessMessage(
                    'CallTypeExpr / 'toArg,
                    fEmbed,
                    argId,
                  )
              }
            case PObject(fields) =>
              //todo: improve
              fields.foreach {
                case (label, tv) =>
                  messages(v) += fieldAccessMessage(
                    'ObjLiteralTypeExpr / 'toV,
                    embed(tv),
                    label,
                  )
                  messages(tv) += fieldAccessMessage(
                    'ObjLiteralTypeExpr / 'toField,
                    embed(v),
                    label,
                  )
                  import graph.fieldUsages
                  if (fieldUsages.contains(label)) {
                    messages(tv) += {
                      val att =
                        attentionLayer(
                          'ObjLiteralTypeExpr / 'fieldUsage,
                          transformKey = true,
                        )(
                          embed(v),
                          fieldUsages(label).toVector.map {
                            case (k, n) => embed(k) -> embed(n)
                          },
                        )
                      messageModel('ObjLiteralTypeExpr / 'usageMessage, att)
                    }
                  }
              }
            case PAccess(objType, label) =>
              messages(v) += fieldAccessMessage(
                'FieldAccess / 'toV,
                embed(objType),
                label,
              )
              messages(objType) += fieldAccessMessage(
                'FieldAccess / 'toObj,
                embed(v),
                label,
              )
              import graph.fieldDefs
              if (fieldDefs.contains(label)) {
                 messages(v) += {
                  val att =
                    attentionLayer('FieldAccess / 'defs, transformKey = true)(
                      embed(objType),
                      fieldDefs(label).toVector.map {
                        case (k, Left(pt)) => embed(k) -> encodeLibType(pt)
                        case (k, Right(t)) => embed(k) -> embed(t)
                      },
                    )
                  messageModel('FieldAccess / 'defsMessage, att)
                }
              }
          }
      }

      par(graph.predicates.toSeq).foreach(sendMessages)
      val newNodeMap =
        embedding.nodeMap.keys
          .map { t =>
            val node = embed(t)
            //        val nodePair = messageModel('MessageAggregate / 'nodeMessage, nodeMap(id))

            val vs = messages(t).receiver.toVector
              .map {
                case (k, v) =>
                  mix('keyMix, node, k) -> mix('valueMix, node, v) //todo: batching
              }
            val out = attentionLayer('MessageAggregate, transformKey = true)(
              node,
              vs :+ (node, node),
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

    def encodeLibType(pType: PType): CompNode = ???

    def encodePType(ty: PType): CompNode = ???

    def similarity(inputMatrix: CompNode, candidateMatrix: CompNode) = {
      inputMatrix.dot(candidateMatrix.t) / dimMessage
    }

    def decode(embedding: VarEmbedding): CompNode = {
      val candidates =
        concatN(axis = 0)(
          par(predictionSpace.typeVector)
            .map(encodePType)
            .toVector,
        )
      val inputs =
        concatN(axis = 0)(nodesToPredict.map { embedding.nodeMap })

      similarity(inputs, candidates)
    }

    val nodeInitVec: CompNode =
      getVar('nodeInitVec)(TensorExtension.randomUnitVec(dimMessage))
    val embedding0 = VarEmbedding(
      graph.projectNodes
        .map(n => n -> nodeInitVec)
        .toMap,
    )
    val embeddings = Vector.iterate(embedding0, iterations + 1)(iterate)

    decode(embeddings.last)
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

  /** Each message consists of a key-value pair */
  type Message = (CompNode, CompNode)

  private def singleLayer(name: SymbolPath, input: CompNode): CompNode = {
    linear(name / 'linear, dimMessage)(input) ~> relu
  }

  private def messageModel(name: SymbolPath, vec: CompNode): Message = {
    relu(linear(name / 'header, dimMessage)(vec)) -> relu(
      linear(name / 'content, dimMessage)(vec),
    )
  }

  private def mix(name: SymbolPath, x1: CompNode, x2: CompNode): CompNode = {
    singleLayer(name, x1.concat(x2, axis = 1))
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
      argId: Int,
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

case class PredicateGraphWithCtx(
    nodes: Set[PNode],
    predicates: Set[TyPredicate],
    libraryTypes: Map[PNode, PType],
    libraryTerms: Map[PNode, PType],
    userAnnotations: Map[PNode, PAnnot],
) {
  import cats.instances.all._
  import cats.Monoid
  import cats.syntax.either._
  type ObjNode = PNode

  lazy val projectNodes: Set[PNode] = nodes.filterNot(_.fromLib)

  /** which label is accessed on which variable as what */
  val fieldUsages: Map[Symbol, Set[(ObjNode, PNode)]] =
    Monoid.combineAll(predicates.collect {
      case DefineRel(v, PAccess(objType, l)) =>
        Map(l -> Set(objType -> v))
    })

  /** which label is defined in which class as what */
  val fieldDefs: Map[Symbol, Set[(ObjNode, Either[PType, PNode])]] =
    Monoid.combineAll(
      predicates.toVector.collect {
        case DefineRel(v, PObject(fields)) =>
          fields.flatMap {
            case (l, t) => Map(l -> Set(v -> t.asRight[PType]))
          }
      } ++ libraryTypes.collect {
        case (node, PObjectType(fields)) =>
          fields.flatMap {
            case (l, t) => Map(l -> Set(node -> t.asLeft[PNode]))
          }
      },
    )

  def printStat(): Unit = {
    val nodeNum = nodes.size
    val predicatesNum = predicates.size
    println(s"Stats{nodeNum: $nodeNum, predicates: $predicatesNum}")
  }
}

object PredicateGraphWithCtx {
  def fromGraph(
      graph: PredicateGraph,
      nodeMapping: Map[PNode, PAnnot],
  ): PredicateGraphWithCtx = {
    val libTypes = for {
      (n, annot) <- nodeMapping if n.isType
      ty <- annot.typeOpt
    } yield {
      assert(n.fromLib)
      n -> ty
    }
    PredicateGraphWithCtx(graph.nodes, graph.predicates, libTypes, ???, ???)
  }
}
