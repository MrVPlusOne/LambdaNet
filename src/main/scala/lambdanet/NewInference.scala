package lambdanet

import scala.language.implicitConversions
import cats.data.Chain

object NewInference {
  import funcdiff._
  import translation.PredicateGraph
  import PredicateGraph._
  import PredicateGraph.PNode
  import TensorExtension.randomUnitVec
  import scala.collection.GenSeq
  import scala.collection.parallel.ForkJoinTaskSupport
  import translation.ImportsResolution.NameDef.unknownDef
  import DebugTime.logTime

  /** Pre-computes a (batched) neural network sketch reusable
    * across multiple training steps for the given [[PredicateGraph]].
    * The actual forward propagation only happens in [[run]]. */
  case class Predictor(
      graph: PredicateGraph,
      libraryTypeNodes: Set[LibTypeNode],
      libNodeType: LibNode => PType,
      taskSupport: Option[ForkJoinTaskSupport],
  ) {

    case class run(
        dimMessage: Int,
        layerFactory: LayerFactory,
        nodesToPredict: Vector[ProjNode],
        iterations: Int,
    ) {
      import layerFactory._

      val architecture = NNArchitecture(dimMessage, layerFactory)
      import architecture.{Embedding, randomVec}

      /** returns softmax logits */
      def result: CompNode = {

        val initEmbedding: Embedding =
          architecture.initialEmbedding(projectNodes)

        val encodeLibNode = logTime("encodeLibNode") {
          computeLibNodeEncoding()
        }

        val embedding = logTime("iterate") {
          Vector
            .iterate(initEmbedding, iterations + 1)(
              updateEmbedding(encodeLibNode),
            )
            .last
        }

        val allSignatureEmbeddings = logTime("allSignatureEmbeddings") {
          def encodeLeaf(n: PNode) =
            if (n.fromProject) embedding(ProjNode(n))
            else getVar('libTypeEmbedding / n.symbol) { randomVec() }
          // encode all types from the prediction space
          signatureEmbeddingMap(encodeLeaf, predictionSpace.allTypes)
        }
        logTime("decode") {
          decode(embedding, allSignatureEmbeddings)
        }
      }

      private def computeLibNodeEncoding(): LibNode => CompNode = {
        val libTypeEmbedding = {
          // todo: better encoding? (like using object label set)
          def encodeLeaf(n: PNode) =
            getVar('libType / n.symbol) { randomVec() }

          signatureEmbeddingMap(encodeLeaf, allLibSignatures) ++
            libraryNodes
              .filter(_.n.isType)
              .map(n => PTyVar(n.n) -> encodeLeaf(n.n))
        }
        val libTermEmbedding = {
          libraryNodes
            .filter(_.n.isTerm)
            .toSeq
            .pipe(parallelize)
            .map { n =>
              val v1 = getVar('libNode / n.n.symbol) { randomVec() }
              val v2 = libTypeEmbedding(libNodeType(n))
              LibTermNode(n) -> architecture.encodeLibTerm(v1, v2)
            }
            .toMap
        }

        n: LibNode => {
          assert(graph.nodes.contains(n.n))
          if (n.n.isType) libTypeEmbedding(PTyVar(n.n))
          else libTermEmbedding(LibTermNode(n))
        }
      }

      private def updateEmbedding(
          encodeLibNode: LibNode => CompNode,
      )(embedding: Embedding): Embedding = {
        import cats.implicits._

        // leaves can be project nodes, which change between iterations
        val encodeFixedTypes = logTime("encodeFixedTypes") {
          def encodeLeaf(n: PNode) =
            if (n.fromProject) embedding(ProjNode(n))
            else getVar('libTypeEmbedding / n.symbol) { randomVec() }
          signatureEmbeddingMap(encodeLeaf, allFixedTypes)
        }

        val messages: Map[ProjNode, Chain[Message]] =
          logTime("compute messages") {
            def encodeNode(n: PNode): CompNode =
              if (n.fromProject) embedding(ProjNode(n))
              else encodeLibNode(LibNode(n))

            batchedMsgModels.toSeq
              .pipe(parallelize)
              .map {
                case (kind, messageModels) =>
                  architecture.calculateMessages(
                    kind,
                    messageModels,
                    encodeNode,
                    encodeLabel,
                    encodeFixedTypes,
                  )
              }
              .fold(Map())(_ |+| _)
          }

        val merged: Map[ProjNode, CompNode] = logTime("merge messages") {
          architecture.mergeMessages(messages, embedding)
        }

        logTime("update embedding") { architecture.update(embedding, merged) }
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

        architecture.similarity(inputs, candidates)
      }

      private def signatureEmbeddingMap(
          leafEmbedding: PNode => CompNode,
          signatures: Set[PType],
      ): Map[PType, CompNode] = {
        import collection.mutable

        val vecForAny = getVar('anyType) { randomVec() }
        val signatureEmbeddings = mutable.HashMap[PType, CompNode]()
        def embedSignature(sig: PType): CompNode =
          SM.withErrorMessage(s"in sig: $sig") {
            val r = sig match {
              case PTyVar(node) => leafEmbedding(node)
              case PAny         => vecForAny
              case PFuncType(args, to) =>
                val args1 = args.map(embedSignature)
                val to1 = embedSignature(to)
                architecture.encodeFunction(args1, to1)
              case PObjectType(fields) =>
                val fields1 = fields.toVector.map {
                  case (label, ty) =>
                    encodeLabel(label) -> embedSignature(ty)
                }
                architecture.encodeObject(fields1)
            }
            signatureEmbeddings.synchronized {
              signatureEmbeddings(sig) = r
            }
            r
          }

        parallelize(signatures.toSeq).foreach(embedSignature)
        signatureEmbeddings.toMap
      }

      private val encodeLabel =
        allLabels.map { k =>
          k -> const(randomUnitVec(dimMessage))
        }.toMap

    }

    val projectNodes: Set[ProjNode] =
      graph.nodes.filter(_.fromProject).map(ProjNode)
    val libraryNodes: Set[LibNode] =
      graph.nodes.filter(_.fromLib).map(LibNode) ++ unknownNodes
    val allLibSignatures: Set[PType] = libraryNodes.map(libNodeType)
    val allFixedTypes: Set[PType] = graph.predicates.flatMap {
      case FixedToType(_, ty) => Set(ty)
      case _                  => Set[PType]()
    }

    val projectTypes: Set[PTyVar] =
      projectNodes.filter(n => n.n.isType).map(n => PTyVar(n.n))
    val libraryTypes: Set[PTyVar] = libraryTypeNodes.map(_.n.n.pipe(PTyVar))
    val predictionSpace = PredictionSpace(libraryTypes ++ projectTypes)

    val allLabels: Set[Symbol] = {
      val pTypeLabels =
        (predictionSpace.allTypes ++ allLibSignatures ++ allFixedTypes)
          .flatMap(_.allLabels)
      val predicateLabels =
        graph.predicates.flatMap {
          case DefineRel(_, expr) => expr.allLabels
          case _                  => Set[Symbol]()
        }
      pTypeLabels ++ predicateLabels
    }

    type BatchedMsgModels = Map[MessageKind, Vector[MessageModel]]
    val batchedMsgModels: BatchedMsgModels = {
      import cats.implicits._
      import MessageModel._
      import MessageKind._

      def mutual(name: String, p1: PNode, p2: PNode): BatchedMsgModels =
        Map(KindBinary(name) -> Vector(Binary(p1, p2)))

      def toBatched(pred: TyPredicate): BatchedMsgModels = pred match {
        case UsedAsBool(n) =>
          Map(KindSingle("usedAsBool") -> Vector(Single(n)))
        case FixedToType(n, ty) =>
          Map(KindWithType("fixedToType") -> Vector(WithType(n, ty)))
        case SubtypeRel(sub, sup) =>
          mutual("subtypeRel", sub, sup)
        case AssignRel(lhs, rhs) =>
          mutual("assignRel", lhs, rhs)
        case InheritanceRel(child, parent) =>
          mutual("inheritanceRel", child, parent)
        case DefineRel(defined, expr) =>
          expr match {
            case n: PNode =>
              mutual("defineEqual", defined, n)
            case PFunc(args, to) =>
              val msgs = (to +: args).zipWithIndex.map {
                case (a, i) => Labeled(defined, a, Label.Position(i - 1))
              }
              Map(KindLabeled("defineFunc", LabelType.Position) -> msgs)
            case PCall(f, args) =>
              // todo: reason about generics
              val msgs = (defined +: args).zipWithIndex.map {
                case (a, i) => Labeled(f, a, Label.Position(i - 1))
              }
              Map(KindLabeled("defineCall", LabelType.Position) -> msgs)
            case PObject(fields) =>
              //todo: use usage information
              val msgs = fields.toVector.map {
                case (l, v) => Labeled(defined, v, Label.Field(l))
              }
              Map(KindLabeled("defineObject", LabelType.Field) -> msgs)
            case PAccess(obj, l) =>
              val msg = Vector(Labeled(defined, obj, Label.Field(l)))
              Map(KindLabeled("defineAccess", LabelType.Field) -> msg)
          }
      }

      graph.predicates.par
        .map(toBatched)
        .fold[BatchedMsgModels](Map())(_ |+| _)
        .mapValuesNow(_.filterNot(_.allNodesFromLib))
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
  object MessageKind {

    case class KindSingle(name: String) extends MessageKind

    case class KindWithType(name: String) extends MessageKind

    case class KindBinary(name: String) extends MessageKind

    case class KindLabeled(name: String, labelType: LabelType.Value)
        extends MessageKind

    object LabelType extends Enumeration {
      val Position, Field = Value
    }
  }

  sealed abstract class MessageModel {
    import MessageModel._

    /** If this is true, then this message model can be discarded */
    def allNodesFromLib: Boolean = this match {
      case Single(n)          => n.fromLib
      case Binary(n1, n2)     => n1.fromLib && n2.fromLib
      case WithType(n, _)     => n.fromLib
      case Labeled(n1, n2, _) => n1.fromLib && n2.fromLib
    }
  }

  object MessageModel {

    case class Single(n: PNode) extends MessageModel

    case class Binary(n1: PNode, n2: PNode) extends MessageModel

    case class WithType(n: PNode, ty: PType) extends MessageModel

    case class Labeled(n1: PNode, n2: PNode, label: Label) extends MessageModel

    sealed trait Label
    object Label {
      case class Position(get: Int) extends Label
      case class Field(get: Symbol) extends Label
    }
  }

  val unknownNodes: Set[LibNode] =
    Set(LibNode(unknownDef.term.get), LibNode(unknownDef.ty.get))
}
