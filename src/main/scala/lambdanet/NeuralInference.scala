package lambdanet

import lambdanet.architecture.Embedding
import lambdanet.architecture.{LabelEncoder, NNArchitecture}
import lambdanet.train.DecodingResult

import scala.collection.mutable

object NeuralInference {
  import funcdiff._
  import translation.PredicateGraph
  import PredicateGraph._
  import PredicateGraph.PNode
  import scala.collection.GenSeq
  import scala.collection.parallel.ForkJoinTaskSupport
  import translation.ImportsResolution.NameDef.unknownDef
  import DebugTime.logTime

  /** When set to false, each message passing has independent parameters */
  val fixBetweenIteration = false

  /** Pre-computes a (batched) neural network sketch reusable
    * across multiple training steps for the given [[PredicateGraph]].
    * The actual forward propagation only happens in [[run]]. */
  case class Predictor(
      projectName: ProjectPath,
      graph: PredicateGraph,
      libraryTypeNodes: Set[LibTypeNode],
      libDefs: LibDefs,
      taskSupport: Option[ForkJoinTaskSupport]
  ) {

    case class run(
        architecture: NNArchitecture,
        nodesToPredict: Vector[ProjNode],
        initEmbedding: Set[ProjNode] => Embedding,
        iterations: Int,
        nodeForAny: LibTypeNode,
        labelEncoder: LabelEncoder,
        isLibLabel: Symbol => Boolean,
        nameEncoder: LabelEncoder,
        labelDropout: Boolean,
        predictionDropout: Boolean,
        isLibOracle: Option[Vector[Boolean]]
    ) {
      import architecture.{randomVar}

      /** returns softmax logits */
      def result: Vector[DecodingResult] = {
        val encodeLibNode = computeLibNodeEncoding()

        val embeddings = logTime("iterate") {
          (0 until iterations)
            .scanLeft(initEmbedding(projectNodes)) { (embed, i) =>
              updateEmbedding(encodeLibNode)(
                embed,
                if (fixBetweenIteration) 0
                else i
              )
            }
            .toVector
        }

        embeddings.map { embed =>
          logTime("decode") {
            def encodeType(ty: PType) = ty match {
              case PTyVar(node) =>
                if (node.fromLib) encodeLibNode(LibNode(node))
                else embed.vars(ProjNode(node))
              case _ => throw new Error()
            }

            decodeSeparate(embed, encodeType, isLibOracle)
          }
        }
      }

      private def computeLibNodeEncoding(): LibNode => CompNode = {
        val libSignatureEmbedding =
          signatureEmbeddingMap(
            architecture.encodeLibType(_, encodeNames),
            encodeLabels
          )

        val libTermEmbeddingMap =
          collection.concurrent.TrieMap[LibTermNode, CompNode]()
        def libTermEmbedding(n0: LibTermNode): CompNode =
          libTermEmbeddingMap.getOrElseUpdate(
            n0, {
              val n = n0.n
              val v1 = randomVar('libNode / n.n.symbol)
              val v2 = libSignatureEmbedding(libDefs.libNodeType(n))
              val name = encodeNameOpt(n.n.nameOpt)
              architecture.encodeLibTerm(v1, v2, name)
            }
          )

        n: LibNode =>
          DebugTime.logTime("computeLibNodeEncoding") {
            if (n.n.isType) libSignatureEmbedding(PTyVar(n.n))
            else libTermEmbedding(LibTermNode(n))
          }
      }

      private def updateEmbedding(
          encodeLibNode: LibNode => CompNode
      )(embedding: Embedding, iteration: Int): Embedding = {

        val messages = logTime("compute messages") {
          def encodeNode(n: PNode): CompNode =
            if (n.fromProject) embedding.vars(ProjNode(n))
            else encodeLibNode(LibNode(n))

          architecture.calculateMessages(
            iteration,
            parallelize(batchedMsgModels.toSeq),
            encodeNode,
            encodeLabels,
            encodeNames,
            labelUsages,
            isLibLabel
          )
        }

        val merged = logTime("merge messages") {
          architecture.mergeMessages(
            'mergeMessages / Symbol(s"iter-$iteration"),
            parallelize(messages.toSeq),
            embedding.vars
          )
        }

        logTime("update embedding") {
          Embedding(
            architecture.update('vars, embedding.vars, merged)
          )
        }
      }

      private def decode(
          embedding: Embedding,
          encodeSignature: Map[PType, CompNode]
      ): DecodingResult = {

        val candidates = predictionSpace.typeVector
          .pipe(parallelize)
          .map(encodeSignature)
          .toVector
        val inputs = nodesToPredict
          .map(embedding.vars.apply)

        architecture.similarity(inputs, candidates, 'decode)
      }

      private def decodeSeparate(
          embedding: Embedding,
          encodeSignature: PType => CompNode,
          isLibOracle: Option[Vector[Boolean]]
      ): DecodingResult = {
        val inputs = nodesToPredict
          .map(embedding.vars.apply)

        val projCandidates = predictionSpace.projTypeVec
          .pipe(parallelize)
          .map(encodeSignature)
          .toVector

        val libCandidates = predictionSpace.libTypeVec
          .pipe(parallelize)
          .map(encodeSignature)
          .toVector

        architecture.twoStageSimilarity(
          inputs,
          libCandidates,
          projCandidates,
          isLibOracle
        )
      }

      private def signatureEmbeddingMap(
          leafEmbedding: PNode => CompNode,
          labelEncoding: Symbol => CompNode
      ): PType => CompNode = {

        val signatureEmbeddings =
          collection.concurrent.TrieMap[PType, CompNode]()
        def embedSignature(sig: PType): CompNode =
          SM.withErrorMessage(s"in sig: $sig") {
            signatureEmbeddings.getOrElseUpdate(
              sig,
              sig match {
                case PTyVar(node) => leafEmbedding(node)
                case PAny         => leafEmbedding(nodeForAny.n.n)
                case PFuncType(args, to) =>
                  val args1 = args.map(embedSignature)
                  val to1 = embedSignature(to)
                  architecture.encodeFunction(args1, to1)
                case PObjectType(fields) =>
                  val fields1 = fields.toVector.map {
                    case (label, ty) =>
                      labelEncoding(label) -> embedSignature(ty)
                  }
                  architecture.encodeObject(fields1)
              }
            )
          }

        embedSignature
      }

      private val encodeLabels = labelEncoder.newEncoder(labelDropout)

      private val encodeNames = nameEncoder.newEncoder(labelDropout)

      def encodeNameOpt(nameOpt: Option[Symbol]): CompNode = {
        nameOpt match {
          case Some(n) => encodeNames(n)
          case None    => randomVar("nameMissing")
        }
      }
    }

    val projectNodes: Set[ProjNode] =
      graph.nodes.filter(_.fromProject).map(ProjNode)
    val projectClasses: Set[PType] = {
      graph.predicates.collect {
        case DefineRel(c, _: PObject) if c.isType && c.nameOpt.nonEmpty =>
          // only collect named types to avoid intermediate type nodes generated by resolveType
          PTyVar(c)
      }
    }
    val projectTypes: Set[PType] = {
      graph.nodes
        .filter(n => n.fromProject && n.isType && n.nameOpt.nonEmpty)
        .map(PTyVar)
    }
    val libraryNodes: Set[LibNode] =
      graph.nodes.filter(_.fromLib).map(LibNode) ++ unknownNodes
    private val unknownType = PTyVar(unknownDef.ty.get)
    val predictionSpace = PredictionSpace(
      libraryTypeNodes
        .map(_.n.n.pipe(PTyVar)) ++ projectClasses - unknownType // ++ Set(PAny),
    )

    val labelUsages: LabelUsages = {
      import cats.implicits._

      val classesInvolvingLabel =
        (libDefs.libClassDefs ++ graph.predicates).toVector.collect {
          case DefineRel(c, PObject(fields)) =>
            fields.toVector.foldMap {
              case (label, field) =>
                Map(label -> Vector(ClassFieldUsage(c, field)))
            }
        }.combineAll

      val accessesInvolvingLabel =
        graph.predicates.toVector.collect {
          case DefineRel(r, PAccess(receiver, label)) =>
            Map(label -> Vector(AccessFieldUsage(receiver, r)))
        }.combineAll

      LabelUsages(classesInvolvingLabel, accessesInvolvingLabel)
    }

    type BatchedMsgModels = Map[MessageKind, Vector[MessageModel]]
    val batchedMsgModels: BatchedMsgModels = {
      import cats.implicits._
      import MessageModel._
      import MessageKind._

      def mutual(name: String, p1: PNode, p2: PNode): BatchedMsgModels =
        Map(KindBinary(name) -> Vector(Binary(p1, p2)))

      def positional(
          name: String,
          f: PNode,
          args: Vector[PNode],
          ret: PNode
      ): BatchedMsgModels = {
        val msgs = (ret +: args).zipWithIndex.map {
          case (a, i) => Labeled(f, a, Label.Position(i - 1))
        }
        Map(KindBinaryLabeled(name, LabelType.Position) -> msgs)
      }

      def toBatched(pred: TyPredicate): BatchedMsgModels = pred match {
        case HasName(n, name) =>
          Map(KindNaming("hasName") -> Vector(Naming(n, name)))
//          Map()
        case UsedAsBool(n) =>
          Map(KindSingle("usedAsBool") -> Vector(Single(n)))
        case BinaryRel(lhs, rhs, name) =>
          mutual(name.toString, lhs, rhs)
        case DefineRel(defined, expr) =>
          expr match {
            case n: PNode =>
              mutual("defineEqual", defined, n)
            case PFunc(args, to) =>
              positional("defineFunc", defined, args, to)
            case PCall(f, args) =>
              // todo: reason about generics
              positional("defineCall", f, args, defined)
            case PObject(fields) =>
              val msgs = fields.toVector.map {
                case (l, v) => Labeled(defined, v, Label.Field(l))
              }
              val fieldMessages = fields.toVector.foldMap {
                case (label, field) =>
                  Map(
                    (KindField(label): MessageKind) ->
                      Vector[MessageModel](ClassFieldUsage(defined, field))
                  )
              }
              fieldMessages |+|
                Map(KindBinaryLabeled("defineObject", LabelType.Field) -> msgs)
            case PAccess(receiver, label) =>
              val msg = Vector(Labeled(defined, receiver, Label.Field(label)))
              Map(
                KindBinaryLabeled("defineAccess", LabelType.Field) -> msg,
                KindAccess(label) -> Vector(
                  AccessFieldUsage(receiver, defined)
                )
              )
          }
      }

      graph.predicates.par
        .map(toBatched)
        .fold[BatchedMsgModels](Map())(_ |+| _)
        .mapValuesNow(_.filterNot(_.allNodesFromLib))
    }

    def visualizeNeuralGraph: String = {
      NeuralVisualization.toMamGraph(graph.nodes, batchedMsgModels)
    }

    private object NeuralVisualization {
      import lambdanet.utils.GraphVisualization
      import lambdanet.utils.GraphVisualization._
      import language.implicitConversions

      def toMamGraph(allNodes: Set[PNode], msgs: BatchedMsgModels): String = {
        val g = new GraphVisualization.LabeledGraph()

        val mapping = mutable.HashMap[Either[MessageModel, PNode], MamElement]()
        implicit def convert1(n: PNode): MamElement =
          mapping.getOrElseUpdate(Right(n), g.newId())
        implicit def convert2(n: MessageModel): MamElement =
          mapping.getOrElseUpdate(Left(n), g.newId())
        implicit def stringElement(s: String): MamElement =
          MamElement(s""""$s"""")

        allNodes.foreach { n =>
          val nodeColor = if (n.fromLib) "Orange" else "Green"
          val nameStr = n.toString
          g.addNode(n, nameStr, nameStr, nodeColor)
        }

        def name(kind: MessageKind): String =
          kind match {
            case MessageKind.KindSingle(name) =>
              s"Single:$name"
            case MessageKind.KindBinary(name) =>
              s"Binary:$name"
            case MessageKind.KindNaming(name) =>
              s"Naming:$name"
            case MessageKind.KindBinaryLabeled(name, labelType) =>
              s"Labeled($name, $labelType)"
            case MessageKind.KindAccess(label) =>
              s"Access($label)"
            case MessageKind.KindField(label) =>
              s"Field($label)"
          }

        msgs.foreach {
          case (kind, msgs) =>
            msgs.foreach { msg =>
              val nm = name(kind)
              g.addNode(msg, nm, s"$nm| $msg", "Blue")

              msg match {
                case MessageModel.Naming(n, name) =>
                  g.addEdge(msg, n, "named")
                case MessageModel.Single(n) =>
                  g.addEdge(msg, n, "")
                case MessageModel.Binary(n1, n2) =>
                  g.addEdge(msg, n1, "n1")
                  g.addEdge(msg, n2, "n2")
                case MessageModel.Labeled(n1, n2, label) =>
                  g.addEdge(msg, n1, "n1")
                  g.addEdge(msg, n2, "n2")
                case ClassFieldUsage(c, field) =>
                  g.addEdge(msg, c, "class")
                  g.addEdge(msg, field, "field")
                case AccessFieldUsage(receiver, result) =>
                  g.addEdge(msg, receiver, "receiver")
                  g.addEdge(msg, result, "result")
              }
            }
        }

        g.toMamFormat("\"SpringElectricalEmbedding\"", directed = true)
          .replace("\uD835\uDCDF", "P")
          .replace("\uD835\uDCDB", "L")
      }
    }

    def parallelize[T](xs: Seq[T]): GenSeq[T] = {
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

    case class KindBinary(name: String) extends MessageKind

    case class KindNaming(name: String) extends MessageKind

    case class KindBinaryLabeled(name: String, labelType: LabelType)
        extends MessageKind

    /** All field accesses involving `label` */
    case class KindAccess(label: Symbol) extends MessageKind

    /** All classes with a field of name `label` */
    case class KindField(label: Symbol) extends MessageKind

    sealed trait LabelType
    object LabelType extends Enumeration {
      case object Position extends LabelType
      case object Field extends LabelType
    }
  }

  sealed abstract class MessageModel {
    import MessageModel._

    /** If this is true, then this message model can be discarded */
    def allNodesFromLib: Boolean = this match {
      case Naming(n, _)             => n.fromLib
      case Single(n)                => n.fromLib
      case Binary(n1, n2)           => n1.fromLib && n2.fromLib
      case Labeled(n1, n2, _)       => n1.fromLib && n2.fromLib
      case AccessFieldUsage(n1, n2) => n1.fromLib && n2.fromLib
      case ClassFieldUsage(n1, n2)  => n1.fromLib && n2.fromLib
    }
  }

  object MessageModel {

    case class Naming(n: PNode, name: Symbol) extends MessageModel

    case class Single(n: PNode) extends MessageModel

    case class Binary(n1: PNode, n2: PNode) extends MessageModel

    case class Labeled(n1: PNode, n2: PNode, label: Label) extends MessageModel

    sealed trait Label
    object Label {
      case class Position(get: Int) extends Label
      case class Field(get: Symbol) extends Label
    }
  }

  case class ClassFieldUsage(`class`: PNode, field: PNode) extends MessageModel

  case class AccessFieldUsage(receiver: PNode, result: PNode)
      extends MessageModel

  case class LabelUsages(
      classesInvolvingLabel: Map[Symbol, Vector[ClassFieldUsage]],
      accessesInvolvingLabel: Map[Symbol, Vector[AccessFieldUsage]]
  )

  val unknownNodes: Set[LibNode] =
    Set(LibNode(unknownDef.term.get), LibNode(unknownDef.ty.get))
}
