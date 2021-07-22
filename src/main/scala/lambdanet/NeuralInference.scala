package lambdanet

import botkop.numsca.{Shape, Tensor}
import lambdanet.architecture.Embedding
import lambdanet.architecture.{LabelEncoder, NNArchitecture}
import lambdanet.train.{DecodingResult, NamingBaseline, ProjectStats}
import lambdanet.train.NamingBaseline.{nodeName, typeName}
import lambdanet.translation.ImportsResolution.NameDef
import org.nd4j.linalg.api.buffer.DataType
import translation.PredicateGraph

import scala.collection.mutable

/**
  * Utilities to build the graph neural network from a given [[PredicateGraph]].
  */
object NeuralInference {
  import funcdiff._
  import PredicateGraph._
  import PredicateGraph.PNode
  import scala.collection.GenSeq
  import scala.collection.parallel.ForkJoinTaskSupport
  import translation.ImportsResolution.NameDef.unknownDef
  import DebugTime.logTime

  /** When set to false, each message passing has independent parameters */
  val fixBetweenIteration = false
  val noAttentional: Boolean = false
  val noContextual: Boolean = false
  val noLogical: Boolean = false

  checkOMP()
  Tensor.floatingDataType = DataType.DOUBLE

  def checkOMP() = {
    if (!sys.env.get("OMP_NUM_THREADS").contains("1")) {
      throw new Error(
        "Environment variable OMP_NUM_THREADS needs to be set to 1 " +
          "to avoid unnecessarily large memory usage and performance penalty"
      )
    }
  }

  /** Pre-computes a (batched) neural network sketch reusable
    * across multiple training steps for the given [[PredicateGraph]].
    * The actual forward propagation only happens in [[run]]. */
  case class Predictor(
      graph: PredicateGraph,
      stats: ProjectStats,
      libDefs: LibDefs,
      taskSupport: Option[ForkJoinTaskSupport],
      onlyPredictLibType: Boolean,
      predictAny: Boolean,
  ) {
    import stats._
    private val parallelism =
      taskSupport.map(_.environment.getParallelism).getOrElse(1)

    case class run(
        architecture: NNArchitecture,
        nodesToPredict: Vector[ProjNode],
        iterations: Int,
        labelEncoder: LabelEncoder,
        isLibLabel: Symbol => Boolean,
        nameEncoder: LabelEncoder,
        labelDropout: Boolean,
        encodeLibSignature: Boolean,
    )(implicit mode: GraphMode) {
      import architecture.{randomVar}

      /** returns softmax logits */
      def result: DecodingResult = {
        val encodeLibNode = computeLibNodeEncoding()

        /**
          * Encodes either a [[PTyVar]] or a [[PAny]].
          */
        def encodeTypeNode(embed: Embedding)(ty: PType) = ty match {
          case PTyVar(node) =>
            if (node.fromLib) encodeLibNode(LibNode(node))
            else embed.vars(ProjNode(node))
          case PAny => encodeLibNode(LibNode(NameDef.anyType.node))
          case _    => throw new Error(s"Unable to encode PType Node: $ty")
        }

        val embed = logTime("iterate") {
          (0 until iterations)
            .scanLeft(architecture.initialEmbedding(projectNodes)) { (embed, i) =>
              updateEmbedding(encodeLibNode)(
                embed,
                if (fixBetweenIteration) 0
                else i
              )
            }
            .last
        }
        logTime("decode") {
          decode(embed, encodeTypeNode(embed))
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
              if (encodeLibSignature) {
                val v1 = randomVar('libNode / n.n.symbol)
                val v2 = libSignatureEmbedding(libDefs.libNodeType(n))
                val name = encodeNameOpt(n.n.nameOpt)
                architecture.encodeLibTerm(v1, v2, name)
              } else {
                val v1 = randomVar('libNode / n.n.symbol)
                val name = encodeNameOpt(n.n.nameOpt)
                architecture.encodeLibTerm(v1, name)
              }
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
      )(
          embedding: Embedding,
          iteration: Int
      ): Embedding = {

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
//            .pipe { embed =>
//            val candidates =
//              predictionSpace.projTypeVec.map(v => v -> encodeType(v))
//            architecture.attendPredictionSpaceByName(
//              projectNodes.toVector,
//              projectNodes.toVector.map{ embed.vars },
//              candidates,
//              similarityScores,
//              s"attendPredictionSpace$iteration"
//            )
//          }
        }
      }

      private def decode(
          embedding: Embedding,
          encodeSignature: PType => CompNode
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

        architecture.similarity(
          inputs,
          libCandidates ++ projCandidates,
          'decodingSimilarity,
          parallelism
        )
      }

      private val nodeForAny = NameDef.anyType.node

      /**
        * Computes the embedding of a (potentially structured) [[PType]], using a simple
        * recursive NN architecture on the type AST.
       **/
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
                case PAny         => leafEmbedding(nodeForAny)
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

    type BatchedMsgModels = Map[MessageKind, Vector[MessageModel]]
    val batchedMsgModels: BatchedMsgModels = {
      import cats.implicits._
      import MessageModel._
      import MessageKind._

      def mutual(name: String, p1: PNode, p2: PNode): BatchedMsgModels = {
        if (p1 == p2) Map() // get rid of self loops
        else Map(KindBinary(name) -> Vector(Binary(p1, p2)))
      }

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

      def unless(cond: Boolean)(msg: => BatchedMsgModels): BatchedMsgModels =
        unlessT(cond, Map(): BatchedMsgModels)(msg)

      def unlessT[T](cond: Boolean, default: => T)(msg: => T): T =
        if (cond) default else msg

      def toBatched(pred: TyPredicate): BatchedMsgModels = pred match {
        case HasName(n, name) =>
          unless(noContextual) {
            Map(KindNaming("hasName") -> Vector(Naming(n, name)))
          }
        case UsedAsBool(n) =>
          unless(noLogical) {
            Map(KindSingle("usedAsBool") -> Vector(Single(n)))
          }
        case BinaryRel(lhs, rhs, name) =>
          unless(noLogical) {
            mutual(name.toString, lhs, rhs)
          }
        case DefineRel(defined, expr) =>
          expr match {
            case n: PNode =>
              unless(noLogical) { mutual("defineEqual", defined, n) }
            case PFunc(args, to) =>
              unless(noLogical) { positional("defineFunc", defined, args, to) }
            case PCall(f, args) =>
              // todo: reason about generics
              unless(noLogical) { positional("defineCall", f, args, defined) }
            case PObject(fields) =>
              def logical: BatchedMsgModels = Map(
                KindBinaryLabeled("defineObject", LabelType.Field) ->
                  fields.toVector.map {
                    case (l, v) => Labeled(defined, v, Label.Field(l))
                  }
              )
              def attentional = fields.toVector.foldMap {
                case (label, field) =>
                  Map(
                    (KindField(label): MessageKind) ->
                      Vector[MessageModel](ClassFieldUsage(defined, field))
                  )
              }
              unless(noAttentional || noContextual)(attentional) |+|
                unless(noLogical)(logical)
            case PAccess(receiver, label) =>
              def logical: BatchedMsgModels = Map(
                KindBinaryLabeled("defineAccess", LabelType.Field) ->
                  Vector(Labeled(defined, receiver, Label.Field(label)))
              )
              def attentional: BatchedMsgModels =
                Map(
                  KindAccess(label) -> Vector(
                    AccessFieldUsage(receiver, defined)
                  )
                )

              unless(noLogical)(logical) |+|
                unless(noAttentional || noContextual)(attentional)
          }
      }

      val nodeWithNames = for {
        n <- projectNodes.toVector.par
        nm = NamingBaseline.nodeName(n.n) if n.n.nameOpt.nonEmpty
      } yield n.n -> nm

      val namedOptions = predictionSpace.typeVector.par.collect {
        case PTyVar(n1) if n1.nameOpt.nonEmpty =>
          n1 -> NamingBaseline.nodeName(n1)
      }

      val similarities = unlessT(noContextual, Set[BatchedMsgModels]()) {
        (for {
          (n, nName) <- nodeWithNames
          (n1, n1Name) <- namedOptions if n != n1
          sim = NamingBaseline.nameSimilarity(nName, n1Name)
          if sim > 0
        } yield mutual(s"nameSimilar$sim", n, n1)).toSet.seq
      }

      (similarities.toSet ++ graph.predicates.par.map(toBatched))
        .fold[BatchedMsgModels](Map())(_ |+| _)
        .mapValuesNow(_.filterNot(_.allNodesFromLib))
    }

    private val nodeOrdering = projectNodes.toVector.zipWithIndex.toMap
//    private lazy val similarityScores = DebugTime.logTime("similarityScores") {
//      if (projectNodes.isEmpty || predictionSpace.projTypeVec.isEmpty)
//        None
//      else {
//        val nodes = projectNodes.toVector.par
//        val candidates = predictionSpace.projTypeVec
//        val scores = for {
//          n <- nodes
//          ty <- candidates
//        } yield {
//          val n1 = nodeName(n.n).toSet
//          val n2 = typeName(ty).toSet
//          val s1 = n1.size
//          val s2 = n2.size
//          val s3 = n1.intersect(n2).size
//          s3.toDouble / (s1 + s2).pipe(x => if (x == 0) 1 else x)
//        }
//        Tensor(scores.toArray)
//          .reshape(Shape.make(nodes.length, candidates.length))
//          .pipe(Some.apply)
//      }
//    }

    private lazy val similarityScores = DebugTime.logTime("similarityScores") {
      val nodes = projectNodes.toVector.par.map { n =>
        nodeName(n.n)
      }
      val candidates = predictionSpace.typeVector.par.map { t =>
        typeName(t)
      }
      val scores = for {
        n1 <- nodes
        n2 <- candidates
      } yield {
        val sim = NamingBaseline.nameSimilarity(n1, n2)
        sim.toDouble
      }
      Tensor(scores.toArray)
        .reshape(Shape.make(nodes.length, candidates.length))
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

    case class KindBinaryLabeled(name: String, labelType: LabelType) extends MessageKind

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

  val unknownNodes: Set[LibNode] =
    Set(LibNode(unknownDef.term.get), LibNode(unknownDef.ty.get))

  /**
  a b=T1 f
  a <: b
  b = f(a)
  T1 l: B1
  T2 l: B2
  T3 l: B1
  P1 l: b1


  f: {T1, T2, T3, P1, Any}
  x: {B1, B2, b1, Any}

  a.f(x)

  a.f(x, y ,z)
  f(x)

  v1 = a.f
  v2 = v1(x)

  x1 <: arg1
  x2 <: arg2
  f <: ret
  g = (arg1, arg2) -> ret
  f = g(x1, x2)
  g <: Function

  x = f.l
  x == b1

  c := x
  c := x  --> Tx <: Tc

    */
  // These field usage code should probably be moved into PredicateGraph, but
  // let's keep them here for now to prevent breaking serialization
  case class ClassFieldUsage(`class`: PNode, field: PNode) extends MessageModel

  case class AccessFieldUsage(receiver: PNode, result: PNode) extends MessageModel

  case class LabelUsages(
      classesInvolvingLabel: Map[Symbol, Vector[ClassFieldUsage]],
      accessesInvolvingLabel: Map[Symbol, Vector[AccessFieldUsage]]
  )

  def computeLabelUsages(
      libDefs: LibDefs,
      graph: PredicateGraph
  ): LabelUsages = {
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
}
