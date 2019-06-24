package lambdanet

object NewInference {
  import funcdiff._
  import translation.PredicateGraph
  import PredicateGraph._
  import PredicateGraph.PNode
  import TensorExtension.randomUnitVec
  import scala.collection.GenSeq
  import scala.collection.parallel.ForkJoinTaskSupport
  import cats.data.Chain


  case class Predictor(
      graph: PredicateGraph,
      predictionSpace: PredictionSpace,
      taskSupport: Option[ForkJoinTaskSupport]
  ) {

    case class run(layerFactory: LayerFactory, dimMessage: Int) {
      import layerFactory._

      case class run(nodesToPredict: Vector[PNode], iterations: Int) {
        def result: CompNode = {
          val initEmbedding: Embedding = {
            val vec = getVar('nodeInitVec)(randomUnitVec(dimMessage))
            projectNodes.map(_ -> vec).toMap
          }
          val libEmbeddings = libTypeEmbeddingMap()
          val embeddings =
            Vector.iterate(initEmbedding, iterations + 1)(
              updateEmbedding(libTypeEmbeddingMap())
            )
          decode(embeddings.last)
        }

        private def similarity(
            inputMatrix: CompNode,
            candidateMatrix: CompNode
        ) = {
          inputMatrix.dot(candidateMatrix.t) / dimMessage
        }

        private def encodePType(ty: PType): CompNode = ???

        private def decode(embedding: Embedding): CompNode = {
          val candidates = predictionSpace.typeVector
            .pipe(par)
            .map(encodePType)
            .toVector
            .pipe(concatN(axis = 0))
          val inputs = nodesToPredict
            .map(embedding.apply)
            .pipe(concatN(axis = 0))

          similarity(inputs, candidates)
        }
      }

      type Embedding = Map[ProjNode, CompNode]

      type Message = CompNode

      def updateEmbedding(
          encodeLibType: PType => CompNode
      )(embedding: Embedding): Embedding = {
        def featureMessages(
            embedding: Embedding
        ): Map[ProjNode, Chain[Message]] = { ??? }

        val messages: Map[ProjNode, Chain[Message]] =
          featureMessages(embedding)
        val merged: Map[ProjNode, CompNode] = mergeMessages(messages)
        replace(embedding, merged)
      }

      def replace(embedding: Embedding, merged: Map[ProjNode, Message]): Embedding = ???

      def mergeMessages(messages: Map[ProjNode, Chain[Message]]): Map[ProjNode, Message] = ???

      private def libTypeEmbeddingMap() = ???
    }

    type ProjNode = PNode
    sealed trait MessageKind
    sealed trait MessageModel

    val projectNodes: Set[ProjNode] = graph.nodes.filterNot(_.fromLib)
    val messageModels: Map[MessageKind, Vector[MessageModel]] = ???

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

}
