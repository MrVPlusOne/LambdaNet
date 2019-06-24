package lambdanet

import botkop.numsca
import botkop.numsca.Tensor

import scala.language.implicitConversions

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

        val messages = batchedMsgModels.toSeq
          .pipe(parallelize)
          .map {
            case (kind, messageModels) =>
              architecture.calculateMessages(kind, messageModels, embedding)
          }
          .fold(Map())(_ |+| _)

        val merged: Map[ProjNode, CompNode] =
          architecture.mergeMessages(messages, embedding)
        architecture.update(embedding, merged)
      }

      private def decode(
          embedding: Embedding,
          encodeSignature: Map[PType, CompNode],
      ): CompNode = {

        val candidates = predictionSpace.typeVector
          .pipe(parallelize)
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
        //todo: parallelize this

        import collection.mutable

        val vecForAny = getVar('anyType) { randomVec() }
        val signatureEmbeddings = mutable.HashMap[PType, CompNode]()
        def embedSignature(sig: PType): CompNode = {
          val r = sig match {
            case PTyVar(node) => leafEmbedding(node)
            case PAny         => vecForAny
            case PFuncType(args, to) =>
              val args1 = args.map(embedSignature)
              val to1 = embedSignature(to)
              architecture.encodeFunc(args1, to1)
            case PObjectType(fields) =>
              val fields1 = fields.toVector.map {
                case (label, ty) =>
                  encodeLabel(label) -> embedSignature(ty)
              }
              architecture.encodeObject(fields1)
          }
          signatureEmbeddings(sig) = r
          r
        }

        signatures.foreach(embedSignature)
        signatureEmbeddings.toMap
      }

      private val normalizeFactor = 0.1 / math.sqrt(dimMessage)
      private def randomVec(): Tensor = {
        numsca.randn(1, dimMessage) * normalizeFactor
      }

      private val encodeLabel =
        allLabels.map{ k =>
          k -> const(randomUnitVec(dimMessage))
        }.toMap

    }

    val projectNodes: Set[ProjNode] = graph.nodes.filterNot(_.fromLib)
    val libraryNodes: Set[LibNode] = graph.nodes.filter(_.fromLib)
    val allLibSignatures: Set[PType] = libraryNodes.map(libNodeType)
    val allLabels: Set[Symbol] = {
      val pTypeLabels =
        (allLibSignatures ++ predictionSpace.allTypes)
        .flatMap(_.allLabels)
      val predicateLabels =
        graph.predicates.flatMap{
          case DefineRel(_, expr) => expr.allLabels
        }
      pTypeLabels ++ predicateLabels
    }

    type BatchedMsgModels = Map[MessageKind, Vector[MessageModel]]
    val batchedMsgModels: BatchedMsgModels = {
      import cats.implicits._
      import MessageModel._
      import MessageKind._

      def toBatched(pred: TyPredicate): BatchedMsgModels = pred match {
        case SubtypeRel(sub, sup) =>
          Map(KindMutual("subtype") -> Vector(Mutual(sub, sup)))
        case AssignRel(lhs, rhs) =>
          Map(KindMutual("assign") -> Vector(Mutual(lhs, rhs)))
        case UsedAsBool(n) =>
          Map(KindSingle("usedAsBool") -> Vector(Single(n)))
      }

      graph.predicates.par
        .map(toBatched)
        .fold[BatchedMsgModels](Map())(_ |+| _)
    }

    private def parallelize[T](xs: Seq[T]): GenSeq[T] = {
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
  type LabelVector = CompNode

  sealed trait MessageKind
  private object MessageKind {
    case class KindSingle(name: String) extends MessageKind
    case class KindMutual(name: String) extends MessageKind
  }

  sealed abstract class MessageModel

  private object MessageModel {
    case class Single(n: PNode) extends MessageModel

    case class Mutual(n1: PNode, n2: PNode) extends MessageModel
  }

  case class NNArchitecture(dimMessage: Int, layerFactory: LayerFactory) {

    import layerFactory._

    def calculateMessages(
        kind: MessageKind,
        models: Vector[MessageModel],
        embedding: PNode => CompNode,
    ): Map[ProjNode, Chain[Message]] = {
      import MessageKind._
      import MessageModel._
      import cats.implicits._

      val result = kind match {
        case KindSingle(name) =>
          val paired = models
            .asInstanceOf[Vector[Single]]
            .map(s => s.n -> embedding(s.n))
          verticalBatching(paired, singleLayer(name, _))
        case KindMutual(name) =>
          val (n1s, n2s, inputs) = models
            .asInstanceOf[Vector[Mutual]]
            .map { s =>
              val merged = embedding(s.n1).concat(embedding(s.n2), axis = 1)
              (s.n1, s.n2, merged)
            }
            .unzip3
          verticalBatching(n1s.zip(inputs), singleLayer(name, _)) |+|
            verticalBatching(n2s.zip(inputs), singleLayer(name, _))
      }
      // should filter out (or avoid computing) all messages for lib nodes
      assert(result.keySet.forall(!_.fromLib))
      result
    }

    def encodeFunc(args: Vector[CompNode], to: CompNode): CompNode = ???

    def encodeObject(elements: Vector[(LabelVector, CompNode)]): CompNode = ???


    def mergeMessages(
        messages: Map[ProjNode, Chain[Message]],
        embedding: PNode => CompNode,
    ): Map[ProjNode, Message] = {
      messages.map {
        case (n, ms) =>
          val n1 = embedding(n)
          val values = concatN(axis = 0)(ms.toVector)
          val keys = singleLayer('mergeMsgs / 'transKey, values)

          //todo: check other kinds of attentions
          val attention = softmax(keys.dot(n1.t).t / dimMessage)
          n -> attention.dot(values)
      }
    }

    def update(
        embedding: Map[ProjNode, CompNode],
        messages: Map[ProjNode, CompNode],
    ): Map[ProjNode, CompNode] = {
      import numsca._

      val inputs = embedding.toVector.map {
        case (k, v) =>
          k -> v.concat(messages(k), axis = 1)
      }
      verticalBatching(inputs, stacked => {
        val old = stacked.slice(:>, 0 :> dimMessage)
        val msg = stacked.slice(dimMessage :> 2 * dimMessage)
        gru('updateEmbedding)(old, msg)
      }).mapValuesNow { chain =>
        val Vector(x) = chain.toVector
        x
      }
    }

    /** stack inputs vertically (axis=0) for batching */
    private def verticalBatching(
        inputs: Vector[(PNode, CompNode)],
        transformation: CompNode => CompNode,
    ): Map[PNode, Chain[CompNode]] = {
      import numsca.:>
      import cats.implicits._

      val (nodes, vectors) = inputs.unzip

      val output = transformation(concatN(axis = 0)(vectors))
      nodes.zipWithIndex.map {
        case (n, i) =>
          Map(n -> Chain(output.slice(i, :>)))
      }.combineAll
    }

    private def singleLayer(path: SymbolPath, input: CompNode): CompNode = {
      linear(path / 'linear, dimMessage)(input) ~> relu
    }

  }

  private implicit def toPath(str: String): SymbolPath = {
    SymbolPath.empty / Symbol(str)
  }
}
