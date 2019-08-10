package lambdanet

import cats.data.Chain
import funcdiff._
import lambdanet.PrepareRepos.parseRepos
import lambdanet.architecture.{ArchitectureHelper, LabelEncoder}
import lambdanet.translation.IR._
import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PNode, PType}

import scala.collection.GenSeq
import scala.collection.parallel.ForkJoinTaskSupport
import scala.language.implicitConversions

/** fixme: some PNode type annotations are missing when tokenizing */
object SequenceModel {

  def main(args: Array[String]): Unit = {
    import ammonite.ops._
    val repos = parseRepos(pwd / RelPath("data/toy"), pwd / RelPath("data/toy"))
    val libDefs = repos.libDefs
    val nodeMapping =
      libDefs.nodeMapping ++ repos.trainSet.flatMap(
        _.irModules.flatMap(_.mapping),
      )
    val seqs = repos.trainSet.head.pipe { m =>
      m.irModules.map { m =>
        tokenizeModule(m, nodeMapping).tap { tks =>
          println(s"${m.path}: $tks")
          println(s"ir module:\n${m}")
        }
      }
    }
//    println("Batched")
//    batch(seqs).foreach(println)
  }

  case class SeqPredictor(
      modules: Vector[IRModule],
      libDefs: LibDefs,
      predSpace: PredictionSpace,
      taskSupport: Option[ForkJoinTaskSupport],
  ) {
    private val sentences = {

      val nodeMapping = libDefs.nodeMapping ++ modules.flatMap(_.mapping)
      modules.par
        .map(tokenizeModule(_, nodeMapping))
        .toVector
    }
    val leftBatched: BatchedSeq = batch(sentences)
    val rightBatched: BatchedSeq = batch(sentences.map(_.reverse))

    def run(
        architecture: SeqArchitecture,
        nameEncoder: LabelEncoder,
        nodesToPredict: Vector[PNode],
        nameDropout: Double
    ): CompNode = {
      val embedding = encode(architecture, nameEncoder, nameDropout)
      val missingEmbedding = architecture
        .randomVar('MissingEmbedding / 'left)
        .concat(architecture.randomVar('MissingEmbedding / 'right), axis = 1)
      val states = nodesToPredict.map { n =>
        embedding.getOrElse(n, missingEmbedding)
      }
      val input = concatN(axis = 0, fromRows = true)(states)
      architecture.predict(input, predSpace.size)
    }

    def encode(
        architecture: SeqArchitecture,
        nameEncoder: LabelEncoder,
        nameDropout: Double
    ): Map[PNode, CompNode] = {
      val encodeName = nameEncoder.newEncoder(nameDropout)
      architecture
        .aggregate(leftBatched, rightBatched, encodeName)
    }
  }

  type BatchedSeq = Vector[Vector[(Token, Option[PNode])]]

  private def batch(seqs: Vector[TokenSeq]): BatchedSeq = {
    require(seqs.nonEmpty)
    val sorted = seqs.sortBy(s => -s.length)
    val maxCols = sorted.head.length
    (0 until maxCols).toVector.map { c =>
      sorted
        .takeWhile(seq => seq.length > c)
        .map(_.apply(c).pipe {
          case (tk, target) => tk -> target.map(_._1)
        })
    }
  }

  case class SeqArchitecture(dimEmbedding: Int, pc: ParamCollection)
      extends ArchitectureHelper {
    val layerFactory = LayerFactory('SeqArchitecture, pc)
    import layerFactory._

    def aggregate(
        leftBatched: BatchedSeq,
        rightBatched: BatchedSeq,
        encodeName: Symbol => CompNode,
    ): Map[PNode, CompNode] = {
      import botkop.numsca._

      def encodeToken(token: Token): CompNode = {
        token match {
          case Name(name)   => encodeName(name)
          case Keyword(key) => randomVar('Keyword / Symbol(key.id.toString))
        }
      }

      /** run rnn from left to right */
      def pass(
          batched: BatchedSeq,
          name: SymbolPath,
      ): Map[PNode, CompNode] = {
        import cats.implicits._

        val initVec = randomVar(name / 'initVec)
        val initMat = Vector
          .fill(batched.head.length)(initVec)
          .pipe(concatN(axis = 0, fromRows = true))
        def iter(
            s0: CompNode,
            input: Vector[(Token, Option[PNode])],
        ): (CompNode, Map[PNode, Chain[CompNode]]) = {
          val n1 = input.length
          val s1 = s0.slice(0 :> n1, :>)
          val (tokens, targets) = input.unzip
          val input1 =
            concatN(axis = 0, fromRows = true)(tokens.map(encodeToken))
          val newState = gru(name / 'gru)(s1, input1)
          val preds: Map[PNode, Chain[CompNode]] = targets
            .zip(0 until n1)
            .map {
              case (Some(n), row) => Map(n -> Chain(newState.slice(row, :>)))
              case _              => Map[PNode, Chain[CompNode]]()
            }
            .combineAll
          newState -> preds
        }

        var predictions = Map[PNode, Chain[CompNode]]()
        batched.foldLeft(initMat) {
          case (s0, input) =>
            val (s1, preds) = iter(s0, input)
            predictions = predictions.combine(preds)
            s1
        }
        predictions.mapValuesNow(xs => meanN(xs.toVector))
      }

      val out1 = pass(leftBatched, 'aggregate / 'left)
      val out2 = pass(rightBatched, 'aggregate / 'right)
      out1.keySet.map { k =>
        k -> out1(k).concat(out2(k), axis = 1)
      }.toMap
    }

    def predict(states: CompNode, predSpaceSize: Int): CompNode = {
      states ~>
        linear('predict / 'L1, dimEmbedding) ~> relu ~>
        linear('predict / 'L2, dimEmbedding) ~> relu ~>
        linear('predict / 'L3, predSpaceSize)
    }
  }

  sealed trait Token
  case class Keyword(key: Keywords.Value) extends Token
  case class Name(symbol: Symbol) extends Token
  val missing = Symbol("<Missing>")

  type Target = Option[(PNode, PType)]

  //noinspection TypeAnnotation
  object Keywords extends Enumeration {
    val const, let = Value
    val beginVarDef, endVarDef = Value
    val beginCall, endCall = Value
    val beginObject, endObject = Value
    val beginAccess, endAccess = Value
    val beginIfExpr, endIfExpr = Value
    val beginCast, endCast = Value
    val beginAssign, endAssign = Value
    val returnToken = Value
    val beginIf, endIf = Value
    val beginWhile, endWhile = Value
    val beginBlock, endBlock = Value
    val beginFuncDef, endFuncDef, beforeReturn = Value
    val beginClassDef, endClassDef = Value
    val beginModule, endModule = Value
  }

  type TokenSeq = Vector[(Token, Target)]

  def tokenizeModule(
      module: IRModule,
      allNodeMapping: Map[PNode, PAnnot],
  ): TokenSeq = SM.withErrorMessage(s"In module: ${module.path}") {
    import Keywords.{beginModule, endModule}
    val tokenizer = Tokenizer(allNodeMapping)
    TK(beginModule) ++ module.stmts.flatMap(tokenizer.tokenize) ++ TK(endModule)
  }

  private def TK(pairs: (Token, Target)*): TokenSeq =
    pairs.toVector

  implicit private def keyword(key: Keywords.Value): (Token, Target) = {
    (Keyword(key), None)
  }

  private case class Tokenizer(mapping: Map[PNode, PAnnot]) {
    import Keywords._
    import scala.language.implicitConversions

    def tokenize(stmt: IRStmt): TokenSeq =
      SM.withErrorMessage(s"In stmt: $stmt") {
        stmt match {
          case VarDef(node, rhs, isConst) =>
            val s3s = rhs match {
              case Var(n) => Vector(tkNode(n))
              case FuncCall(f, args) =>
                TK(beginCall, f) ++ args.map(tkNode) ++ TK(endCall)
              case ObjLiteral(fields) =>
                keyword(beginObject) +:
                  fields.toVector.flatMap {
                    case (s, n) => TK(s, n)
                  } :+ keyword(endObject)
              case FieldAccess(receiver, label) =>
                TK(beginAccess, receiver, label, endAccess)
              case IfExpr(cond, e1, e2) =>
                TK(beginIfExpr, cond, e1, e2, endIfExpr)
              case Cast(expr, ty) =>
                TK(beginCast, expr, ty, endCast)
            }
            val s1 = if (isConst) const else let
            TK(beginVarDef, s1, node) ++ s3s ++ TK(endVarDef)
          case AssignStmt(lhs, rhs) =>
            TK(beginAssign, lhs, rhs, endAssign)
          case ReturnStmt(v, _) =>
            TK(returnToken, v)
          case IfStmt(cond, e1, e2) =>
            TK(beginIf, cond) ++ tokenize(e1) ++ tokenize(e2) ++ TK(endIf)
          case WhileStmt(cond, body) =>
            TK(beginWhile, cond) ++ tokenize(body) ++ TK(endWhile)
          case BlockStmt(stmts) =>
            keyword(beginBlock) +: stmts.flatMap(tokenize) :+ keyword(endBlock)
          case FuncDef(funcNode, args, returnType, body) =>
            TK(beginFuncDef, funcNode) ++
              args.map(tkNode) ++
              TK(beforeReturn, returnType) ++
              tokenize(body) ++ TK(endFuncDef)
          case ClassDef(classNode, superTypes, vars, funcDefs) =>
            TK(beginClassDef, classNode) ++
              superTypes.toVector.map(v => tkNode(v.node)) ++
              TK(beginObject) ++
              vars.toVector.flatMap { case (s, n) => TK(s, n) } ++
              funcDefs.toVector.flatMap(_._2.pipe(tokenize)) ++
              TK(endObject, endClassDef)
        }
      }

    implicit private def justToken(token: Symbol): (Token, Target) = {
      (Name(token), None)
    }

    implicit private def tkNode(node: PNode): (Token, Target) = {
      val target = mapping.get(node) match {
        case Some(Annot.User(t, _)) =>
          // could missing if this is an unresolved import
          Some(node -> t)
        case _ => None
      }
      Name(node.nameOpt.getOrElse(missing)) -> target
    }
  }

}
