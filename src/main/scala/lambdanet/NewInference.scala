package lambdanet

import botkop.numsca
import botkop.numsca.Tensor
import lambdanet.translation.ImportsResolution.NameDef
import lambdanet.translation.ImportsResolution.NameDef.unknownDef

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
        nodesToPredict: Vector[ProjNode],
        iterations: Int,
    ) {
      import layerFactory._
      import architecture.randomVec

      def result: CompNode = {
        val libTypeEmbedding = {
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
            .map { n =>
              val v1 = getVar("libNode" / n.n.symbol) { randomVec() }
              val v2 = libTypeEmbedding(libNodeType(n))
              LibTermNode(n) -> architecture.encodeLibTerm(v1, v2)
            }
            .toMap
        }

        val initEmbedding: Embedding = {
          val vec = getVar('nodeInitVec)(randomUnitVec(dimMessage))
          projectNodes.map(_ -> vec).toMap
        }

        def encodeLibNode(n: LibNode): CompNode = {
          assert(graph.nodes.contains(n.n))
          if (n.n.isType) libTypeEmbedding(PTyVar(n.n))
          else libTermEmbedding(LibTermNode(n))
        }

        val embedding = Vector
          .iterate(initEmbedding, iterations + 1)(
            updateEmbedding(encodeLibNode),
          )
          .last

        val allSignatureEmbeddings = {
          def encodeLeaf(n: PNode) =
            if (n.fromProject) embedding(ProjNode(n))
            else getVar('libTypeEmbedding / n.symbol) { randomVec() }

          signatureEmbeddingMap(encodeLeaf, predictionSpace.allTypes)
        }
        decode(embedding, allSignatureEmbeddings)
      }

      val architecture = NNArchitecture(dimMessage, layerFactory)

      private def updateEmbedding(
          encodeLibNode: LibNode => CompNode,
      )(embedding: Embedding): Embedding = {
        import cats.implicits._

        val encodeFixedTypes: Map[PType, CompNode] = {
          def encodeLeaf(n: PNode) =
            if (n.fromProject) embedding(ProjNode(n))
            else getVar('libTypeEmbedding / n.symbol) { randomVec() }
          signatureEmbeddingMap(encodeLeaf, allFixedTypes)
        }

        def encodeNode(n: PNode): CompNode =
          if (n.fromProject) embedding(ProjNode(n))
          else encodeLibNode(LibNode(n))

        val messages = batchedMsgModels.toSeq
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
    val allLabels: Set[Symbol] = {
      val pTypeLabels =
        (allLibSignatures ++ predictionSpace.allTypes ++ allFixedTypes)
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
              val nodes = to +: args
              val msgs = nodes.zipWithIndex.map {
                case (a, i) => Labeled(defined, a, Label.Position(i - 1))
              }
              Map(KindLabeled("defineFunc", LabelType.Position) -> msgs)
            case PCall(f, args) =>
              assert(graph.nodes.contains(f))
              assert(args.forall(graph.nodes.contains))
              // todo: reason about generics
              val fArgMsgs = args.zipWithIndex.map {
                case (a, i) => Labeled(f, a, Label.Position(i))
              }
              val fDefinedMsg = Vector(Labeled(f, defined, Label.Position(-1)))
              Map(
                KindLabeled("defineCall1", LabelType.Position) -> fArgMsgs,
                KindLabeled("defineCall2", LabelType.Position) -> fDefinedMsg,
              )
            //todo: use usage information
            case PObject(fields) =>
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
  private object MessageKind {

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

  private object MessageModel {

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

  case class NNArchitecture(dimMessage: Int, layerFactory: LayerFactory) {
    import layerFactory._

    private val normalizeFactor = 0.1 / math.sqrt(dimMessage)
    def randomVec(): Tensor = {
      numsca.randn(1, dimMessage) * normalizeFactor
    }

    def calculateMessages(
        kind: MessageKind,
        models: Vector[MessageModel],
        encodeNode: PNode => CompNode,
        encodeLabel: Symbol => CompNode,
        encodeFixedType: PType => CompNode,
    ): Map[ProjNode, Chain[Message]] = {
      import MessageKind._
      import MessageModel._
      import cats.implicits._

      val result0 = kind match {
        case KindSingle(name) =>
          val paired = models
            .asInstanceOf[Vector[Single]]
            .map(s => s.n -> encodeNode(s.n))
          verticalBatching(paired, singleLayer(name, _))
        case KindBinary(name) =>
          val (n1s, n2s, inputs) = models
            .asInstanceOf[Vector[Binary]]
            .map { s =>
              val merged = encodeNode(s.n1).concat(encodeNode(s.n2), axis = 1)
              (s.n1, s.n2, merged)
            }
            .unzip3
          verticalBatching(n1s.zip(inputs), singleLayer(name / 'left, _)) |+|
            verticalBatching(n2s.zip(inputs), singleLayer(name / 'right, _))
        case KindLabeled(name, labelType) =>
          val (n1s, n2s, inputs) = models
            .asInstanceOf[Vector[Labeled]]
            .map {
              case Labeled(n1, n2, label) =>
                val lEmbed = labelType match {
                  case LabelType.Field =>
                    val l = label.asInstanceOf[Label.Field].get
                    encodeLabel(l)
                  case LabelType.Position =>
                    val pos = label.asInstanceOf[Label.Position].get
                    encodePosition(pos)
                }
                val input = concatN(axis = 1)(
                  Vector(encodeNode(n1), encodeNode(n2), lEmbed),
                )
                (n1, n2, input)
            }
            .unzip3
          verticalBatching(n1s.zip(inputs), singleLayer(name / 'left, _)) |+|
            verticalBatching(n2s.zip(inputs), singleLayer(name / 'right, _))
        case KindWithType(name) =>
          // limitation: information flows in only one direction
          val inputs = models
            .asInstanceOf[Vector[WithType]]
            .map {
              case WithType(n, ty) =>
                n -> encodeFixedType(ty)
            }
          verticalBatching(inputs, singleLayer(name, _))
      }

      // filter out all messages for lib nodes
      result0.collect {
        case (k, v) if k.fromProject => ProjNode(k) -> v
      }
    }

    def encodeFunction(args: Vector[CompNode], to: CompNode): CompNode = {
      (to +: args).zipWithIndex
        .map {
          case (a, i) =>
            a.concat(encodePosition(i), axis = 1)
        }
        .pipe(concatN(axis = 0))
        .pipe(singleLayer('encodeFunction, _))
        .pipe(sum(_, axis = 0))
    }

    def encodeObject(elements: Vector[(LabelVector, CompNode)]): CompNode = {
      if (elements.isEmpty) {
        getVar('emptyObject) { randomVec() }
      } else {
        elements
          .map {
            case (v1, v2) => v1.concat(v2, axis = 1)
          }
          .pipe(concatN(axis = 0))
          .pipe(singleLayer('encodeObject, _))
          .pipe(sum(_, axis = 0))
      }
    }

    def encodeLibTerm(experience: CompNode, signature: CompNode): CompNode = {
      singleLayer('encodeLibTerm, experience.concat(signature, axis = 1))
    }

    def mergeMessages(
        messages: Map[ProjNode, Chain[Message]],
        embedding: ProjNode => CompNode,
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
          k -> v.concat(messages.getOrElse(k, emptyMessage), axis = 1)
      }
      verticalBatching(inputs, stacked => {
        val old = stacked.slice(:>, 0 :> dimMessage)
        val msg = stacked.slice(:>, dimMessage :> 2 * dimMessage)
        gru('updateEmbedding)(old, msg)
      }).mapValuesNow { chain =>
        val Vector(x) = chain.toVector
        x
      }
    }

    private val emptyMessage = getVar('emptyMessage) { randomVec() }

    /** stack inputs vertically (axis=0) for batching */
    private def verticalBatching[K](
        inputs: Vector[(K, CompNode)],
        transformation: CompNode => CompNode,
    ): Map[K, Chain[CompNode]] = {
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

    private val encodePosition = {
      def encodePosition(pos: Int): CompNode = {
        assert(pos >= -1)
        if (pos == -1) {
          getVar('position / 'head) { randomVec() }
        } else {
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
      val maxFunctionArity = 50
      (-1 to maxFunctionArity).map(i => i -> encodePosition(i)).toMap
    }

  }

  val unknownNodes: Set[LibNode] =
    Set(LibNode(unknownDef.term.get), LibNode(unknownDef.ty.get))

  private implicit def toPath(str: String): SymbolPath = {
    SymbolPath.empty / Symbol(str)
  }
}
