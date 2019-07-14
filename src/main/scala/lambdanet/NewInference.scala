package lambdanet

import lambdanet.architecture.{NNArchitecture}

object NewInference {
  import funcdiff._
  import translation.PredicateGraph
  import PredicateGraph._
  import PredicateGraph.PNode
  import scala.collection.GenSeq
  import scala.collection.parallel.ForkJoinTaskSupport
  import translation.ImportsResolution.NameDef.unknownDef
  import DebugTime.logTime

  /** Pre-computes a (batched) neural network sketch reusable
    * across multiple training steps for the given [[PredicateGraph]].
    * The actual forward propagation only happens in [[run]]. */
  case class Predictor(
      nodeForAny: LibTypeNode,
      graph: PredicateGraph,
      libraryTypeNodes: Set[LibTypeNode],
      libNodeType: LibNode => PType,
      labelEncoder: GenSeq[Symbol] => Symbol => CompNode,
      nameEncoder: GenSeq[Symbol] => Symbol => CompNode,
      taskSupport: Option[ForkJoinTaskSupport],
  ) {

    case class run(
        architecture: NNArchitecture,
        nodesToPredict: Vector[ProjNode],
        iterations: Int,
    ) {
      import architecture.{Embedding, randomVar}

      /** returns softmax logits */
      def result: Vector[CompNode] = {

        val initEmbedding: Embedding =
          architecture.initialEmbedding(projectNodes, predicateLabels)

        val encodeLibNode = logTime("encodeLibNode") {
          computeLibNodeEncoding()
        }

        val embeddings = logTime("iterate") {
          Vector
            .iterate(initEmbedding, iterations + 1)(
              updateEmbedding(encodeLibNode),
            )
        }

        embeddings.map { embed =>
          val allSignatureEmbeddings = logTime("allSignatureEmbeddings") {
            def encodeLeaf(n: PNode) =
              if (n.fromProject) embed.vars(ProjNode(n))
              else randomVar('libTypeEmbedding / n.symbol)
            def encodeLabel(l: Symbol) =
              embed.labels.getOrElse(l, encodeLibLabels(l))

            // encode all types from the prediction space
            signatureEmbeddingMap(
              encodeLeaf,
              encodeLabel,
              predictionSpace.allTypes,
            )
          }
          logTime("decode") {
            decode(embed, allSignatureEmbeddings)
          }
        }
      }

      private def computeLibNodeEncoding(): LibNode => CompNode = {
        val libTypeEmbedding = {
          // todo: better encoding? (like using object label set)
          def encodeLeaf(n: PNode) = {
            val ex = randomVar('libType / n.symbol)
//            libNodeType(n) match {
//              case PObjectType(fields) =>
//                fields.keys.toVector.map(encodeLabel)
//            }
            val name = encodeNameOpt(n.nameOpt)
            architecture.encodeLibType(ex, name)
          }

          signatureEmbeddingMap(encodeLeaf, encodeLibLabels, allLibSignatures) ++
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
              val v1 = randomVar('libNode / n.n.symbol)
              val v2 = libTypeEmbedding(libNodeType(n))
              val name = encodeNameOpt(n.n.nameOpt)
              LibTermNode(n) -> architecture.encodeLibTerm(v1, v2, name)
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

        val messages =
          logTime("compute messages") {
            def encodeNode(n: PNode): CompNode =
              if (n.fromProject) embedding.vars(ProjNode(n))
              else encodeLibNode(LibNode(n))

            def encodeLabel(l: Symbol): CompNode = {
              embedding.labels.getOrElse(l, encodeLibLabels(l))
            }

            architecture.calculateMessages(
              parallelize(batchedMsgModels.toSeq),
              encodeNode,
              encodeLabel,
              encodeNames,
            )
          }

        val merged = logTime("merge messages") {
          val (m1, m2) = messages
          val r1 =
            architecture.mergeMessages(
              'vars,
              parallelize(m1.toSeq),
              embedding.vars,
            )
          val r2 =
            architecture.mergeMessages(
              'labels,
              parallelize(m2.toSeq),
              embedding.labels,
            )
          (r1, r2)
        }

        logTime("update embedding") {
          architecture.Embedding(
            architecture.update('vars, embedding.vars, merged._1),
            architecture.update('labels, embedding.labels, merged._2),
          )
        }
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
          .map(embedding.vars.apply)
          .pipe(concatN(axis = 0))

        architecture.similarity(inputs, candidates)
      }

      private def signatureEmbeddingMap(
          leafEmbedding: PNode => CompNode,
          labelEncoding: Symbol => CompNode,
          signatures: Set[PType],
      ): Map[PType, CompNode] = {

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
              },
            )
          }

        parallelize(signatures.toSeq).foreach(embedSignature)
        signatureEmbeddings.toMap
      }

      private val encodeLibLabels = labelEncoder(
        parallelize((allLabels ++ additionalNames).toSeq),
      )

      private val encodeNames = nameEncoder(
        parallelize((allLabels ++ additionalNames).toSeq),
      )

      def encodeNameOpt(nameOpt: Option[Symbol]): CompNode = {
        nameOpt match {
          case Some(n) => encodeLibLabels(n)
          case None    => randomVar("nameMissing")
        }
      }
    }

    val projectNodes: Set[ProjNode] =
      graph.nodes.filter(_.fromProject).map(ProjNode)
    val libraryNodes: Set[LibNode] =
      graph.nodes.filter(_.fromLib).map(LibNode) ++ unknownNodes
    val allLibSignatures: Set[PType] = libraryNodes.map(libNodeType)

    val projectTypes: Set[PTyVar] =
      projectNodes.filter(n => n.n.isType).map(n => PTyVar(n.n))
    val libraryTypes: Set[PTyVar] = libraryTypeNodes.map(_.n.n.pipe(PTyVar))
    val predictionSpace = PredictionSpace(
      Set(PAny) ++ libraryTypes ++ projectTypes,
    )
    val predicateLabels: Set[Symbol] =
      graph.predicates.flatMap {
        case DefineRel(_, expr) => expr.allLabels
        case _                  => Set[Symbol]()
      }
    val allLabels: Set[Symbol] = {
      val pTypeLabels =
        (predictionSpace.allTypes ++ allLibSignatures)
          .flatMap(_.allLabels)
      pTypeLabels ++ predicateLabels
    }

    val additionalNames: Set[Symbol] = {
      val nodes = graph.nodes ++ allLibSignatures.flatMap(_.allNodes)
      nodes.flatMap(_.nameOpt)
    }

    type BatchedMsgModels = Map[MessageKind, Vector[MessageModel]]
    val batchedMsgModels: BatchedMsgModels = {
      import cats.implicits._
      import MessageModel._
      import MessageKind._

      def mutual(name: String, p1: PNode, p2: PNode): BatchedMsgModels =
        Map(KindBinary(name) -> Vector(Binary(p1, p2)))

      def toBatched(pred: TyPredicate): BatchedMsgModels = pred match {
        case HasName(n, name) =>
          Map(KindNaming("hasName") -> Vector(Naming(n, name)))
        case UsedAsBool(n) =>
          Map(KindSingle("usedAsBool") -> Vector(Single(n)))
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
              Map(KindBinaryLabeled("defineFunc", LabelType.Position) -> msgs)
            case PCall(f, args) =>
              // todo: reason about generics
              val msgs = (defined +: args).zipWithIndex.map {
                case (a, i) => Labeled(f, a, Label.Position(i - 1))
              }
              Map(KindBinaryLabeled("defineCall", LabelType.Position) -> msgs)
            case PObject(fields) =>
              val msgs = fields.toVector.map {
                case (l, v) => Labeled(defined, v, Label.Field(l))
              }
              Map(KindBinaryLabeled("defineObject", LabelType.Field) -> msgs)
            case PAccess(obj, l) =>
              val msg = Vector(Labeled(defined, obj, Label.Field(l)))
              Map(KindBinaryLabeled("defineAccess", LabelType.Field) -> msg)
          }
      }

      graph.predicates.par
        .map(toBatched)
        .fold[BatchedMsgModels](Map())(_ |+| _)
        .mapValuesNow(_.filterNot(_.allNodesFromLib))
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
      case Naming(n, _)       => n.fromLib
      case Single(n)          => n.fromLib
      case Binary(n1, n2)     => n1.fromLib && n2.fromLib
      case Labeled(n1, n2, _) => n1.fromLib && n2.fromLib
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
}
