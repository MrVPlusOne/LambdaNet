package lambdanet

import botkop.numsca
import botkop.numsca.Tensor

object NewInference {
  import funcdiff._
  import translation.PredicateGraph
  import PredicateGraph._
  import PredicateGraph.PNode
  import TensorExtension.randomUnitVec
  import scala.collection.GenSeq
  import scala.collection.parallel.ForkJoinTaskSupport
  import cats.data.Chain

  type ProjNode = PNode
  type LibNode = PNode
  type LibTypeNode = PNode
  type LibTermNode = PNode

  /** [[Predictor]] pre-computes a neural network sketch for the
    * given [[PredicateGraph]] and can be reused across multiple
    * training steps. The actual forward propagation only happens
    * in [[run]]. */
  case class Predictor(
      graph: PredicateGraph,
      libNodeType: LibNode => PType,
      predictionSpace: PredictionSpace,
      taskSupport: Option[ForkJoinTaskSupport],
  ) {

    case class run(
        dimMessage: Int,
        layerFactory: LayerFactory,
        nodesToPredict: Vector[PNode],
        iterations: Int,
    ) {
      import layerFactory._

      def result: CompNode = {
        val libSignatureEmbeddings = {
          val allLibSignatures: Set[PType] =
            libraryNodes.map(libNodeType)
          def encodeLeaf(n: PNode) =
            getVar('libTypeEmbedding / n.symbol) { randomVec() }
          signatureEmbeddingMap(encodeLeaf, allLibSignatures)
        }

        val initEmbedding: Embedding = {
          val vec = getVar('nodeInitVec)(randomUnitVec(dimMessage))
          projectNodes.map(_ -> vec).toMap
        }

        val embeddings = Vector
          .iterate(initEmbedding, iterations + 1)(
            updateEmbedding(libSignatureEmbeddings),
          )
          .last

        val allSignatureEmbeddings = {
          def encodeLeaf(n: PNode) =
            embeddings.getOrElse(n, {
              getVar('libTypeEmbedding / n.symbol) { randomVec() }
            })
          signatureEmbeddingMap(encodeLeaf, predictionSpace.allTypes)
        }
        decode(embeddings, allSignatureEmbeddings)
      }

      private def updateEmbedding(
          encodeSignature: PType => CompNode,
      )(embedding: Embedding): Embedding = {
        import cats.implicits._

        val messages = batchedMessages.toSeq
          .pipe(par)
          .map {
            case (kind, messageModels) =>
              architecture.calculateMessages(kind, messageModels, embedding)
          }
          .fold[Map[ProjNode, Chain[Message]]](Map())(_ |+| _)

        val merged: Map[ProjNode, CompNode] =
          architecture.mergeMessages(messages)
        replace(embedding, merged)
      }

      private def decode(
          embedding: Embedding,
          encodeSignature: Map[PType, CompNode],
      ): CompNode = {

        val candidates = predictionSpace.typeVector
          .pipe(par)
          .map(encodeSignature)
          .toVector
          .pipe(concatN(axis = 0))
        val inputs = nodesToPredict
          .map(embedding.apply)
          .pipe(concatN(axis = 0))

        similarity(inputs, candidates)
      }

      val architecture = NNArchitecture(dimMessage, layerFactory)

      private def similarity(
          inputMatrix: CompNode,
          candidateMatrix: CompNode,
      ) = {
        inputMatrix.dot(candidateMatrix.t) / dimMessage
      }

      type Embedding = Map[ProjNode, CompNode]

      private def signatureEmbeddingMap(
          leafEmbedding: PNode => CompNode,
          signatures: Set[PType],
      ): Map[PType, CompNode] = {
        //todo: parallelize

        import collection.mutable

        val signatureEmbeddings = mutable.HashMap[PType, CompNode]()
        def embedSignature(sig: PType): CompNode = {
          val r = sig match {
            case PTyVar(node) => leafEmbedding(node)
            case PFuncType(args, to) =>
              val args1 = args.map(embedSignature)
              val to1 = embedSignature(to)
              architecture.signatureNetwork(args1, to1)
            case _ => ???
          }
          signatureEmbeddings(sig) = r
          r
        }

        signatures.foreach(embedSignature)
        signatureEmbeddings.toMap
      }

      private def randomVec(): Tensor = {
        numsca.randn(1, dimMessage) * 0.01
      }

      def replace(
          embedding: Embedding,
          merged: Map[ProjNode, Message],
      ): Embedding = ???
    }

    val projectNodes: Set[ProjNode] = graph.nodes.filterNot(_.fromLib)
    val libraryNodes: Set[LibNode] = graph.nodes.filter(_.fromLib)

    type BatchedMessages = Map[MessageKind, Vector[MessageModel]]
    val batchedMessages: BatchedMessages = {
      import cats.implicits._
      import MessageModel._
      val K = MessageKind

      def toBatched(pred: TyPredicate): BatchedMessages = pred match {
        case SubtypeRel(sub, sup) =>
          Map(K.Mutual("subtype") -> Vector(Mutual(sub, sup)))
        case AssignRel(lhs, rhs) =>
          Map(K.Mutual("assign") -> Vector(Mutual(lhs, rhs)))
        case UsedAsBool(n) =>
          Map(K.Single("usedAsBool") -> Vector(Single(n)))
      }

      graph.predicates.par
        .map(toBatched)
        .fold[BatchedMessages](Map())(_ |+| _)
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
  }

  type Message = CompNode

  sealed trait MessageKind
  private object MessageKind {
    case class Single(name: String) extends MessageKind
    case class Mutual(name: String) extends MessageKind
  }

  sealed abstract class MessageModel

  private object MessageModel {
    case class Single(n: PNode) extends MessageModel

    case class Mutual(n1: PNode, n2: PNode) extends MessageModel
  }

  case class NNArchitecture(dimMessage: Int, layerFactory: LayerFactory) {
    def calculateMessages(
        kind: MessageKind,
        messageModels: Vector[MessageModel],
        embedding: PNode => CompNode,
    ): Map[ProjNode, Chain[Message]] = ???

    def signatureNetwork(args: Vector[CompNode], to: CompNode): CompNode = ???

    def mergeMessages(
        messages: Map[ProjNode, Chain[Message]],
    ): Map[ProjNode, Message] = ???
  }

}
