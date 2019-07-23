package lambdanet

import funcdiff._
import lambdanet.PrepareRepos.parseRepos
import lambdanet.architecture.ArchitectureHelper
import lambdanet.translation.IR._
import lambdanet.translation.PAnnot
import lambdanet.translation.PredicateGraph.{PNode, PType}
import scala.collection.parallel.ForkJoinTaskSupport

object SequenceModel {

  def main(args: Array[String]): Unit = {
    import ammonite.ops._
    val repos = parseRepos(pwd / RelPath("data/toy"), pwd / RelPath("data/toy"))
    val libDefs = repos.libDefs
    val seqs = repos.trainSet.head.pipe { m =>
      m.irModules.map(tokenizeModule(_, libDefs).tap(println))
    }
    println("Batched")
    batch(seqs).foreach(println)
  }

  case class Predictor(
      modules: Vector[IRModule],
      libDefs: LibDefs,
      predSpace: PredictionSpace,
      trainableTokens: Set[Token],
      taskSupport: Option[ForkJoinTaskSupport],
  ) {
    private val sentences = modules.par
      .map(tokenizeModule(_, libDefs))
      .toVector
    val leftBatched: BatchedSeq = batch(sentences)
    val rightBatched: BatchedSeq = batch(sentences.map(_.reverse))

    def run(architecture: ModelArchitecture): (Vector[PNode], CompNode) = {
      val (nodes, states) =
        architecture.aggregate(leftBatched, rightBatched, trainableTokens)
      val input = concatN(axis = 0, fromRows = true)(states)
      nodes -> architecture.predict(input, predSpace.size)
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

  case class ModelArchitecture(dimEmbedding: Int, pc: ParamCollection)
      extends ArchitectureHelper {
    val layerFactory = LayerFactory('ModelArchitecture, pc)
    import layerFactory._

    def aggregate(
        leftBatched: BatchedSeq,
        rightBatched: BatchedSeq,
        trainableTokens: Set[Token],
    ): (Vector[PNode], Vector[CompNode]) = {
      import botkop.numsca._

      def encodeToken(token: Token): CompNode = {
        if (trainableTokens contains token) {
          val path = token match {
            case Name(name)   => 'Name / name
            case Keyword(key) => 'Keyword / Symbol(key.id.toString)
          }
          randomVar(path)
        } else randomVar('UnknownToken)
      }

      /** run rnn from left to right */
      def pass(
          batched: BatchedSeq,
          name: SymbolPath,
      ): Vector[(PNode, CompNode)] = {
        val initVec = randomVar(name / 'initVec)
        val initMat = Vector
          .fill(batched.head.length)(initVec)
          .pipe(concatN(axis = 0, fromRows = true))
        def iter(
            s0: CompNode,
            input: Vector[(Token, Option[PNode])],
        ): (CompNode, Vector[(PNode, CompNode)]) = {
          val n1 = input.length
          val s1 = s0.slice(0, n1, :>)
          val (tokens, targets) = input.unzip
          val input1 =
            concatN(axis = 0, fromRows = true)(tokens.map(encodeToken))
          val newState = gru(name / 'gru)(s1, input1)
          val preds = targets.zip(0 until n1).flatMap {
            case (Some(n), row) => Vector(n -> newState.slice(row, :>))
            case _              => Vector()
          }
          newState -> preds
        }

        var predictions = Vector[(PNode, CompNode)]()
        batched.foldLeft(initMat) {
          case (s0, input) =>
            val (s1, preds) = iter(s0, input)
            predictions ++= preds
            s1
        }
        predictions
      }

      val out1 = pass(leftBatched, 'aggregate / 'left)
      val out2 = pass(rightBatched, 'aggregate / 'right).reverse
      (out1 zip out2).map {
        case ((n1, s1), (n2, s2)) =>
          assert(n1 == n2)
          n1 -> s1.concat(s2, axis = 1)
      }.unzip
    }

    def predict(states: CompNode, predSpaceSize: Int): CompNode = {
      def singleLayer(name: Symbol, dim: Int)(input: CompNode) = {
        val path = 'predict / name
        input ~> linear(path, dimEmbedding) ~> relu
      }

      states ~> singleLayer('L1, dimEmbedding) ~> singleLayer(
        'L2,
        predSpaceSize,
      )
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
    val beginFuncDef, endFuncDef = Value
    val beginClassDef, endClassDef = Value
    val beginModule, endModule = Value
  }

  type TokenSeq = Vector[(Token, Target)]

  def tokenizeModule(module: IRModule, libDefs: LibDefs): TokenSeq = {
    import Keywords.{beginModule, endModule}
    val tokenizer = Tokenizer(libDefs.nodeMapping ++ module.mapping)
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

    def tokenize(stmt: IRStmt): TokenSeq = stmt match {
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
        TK(beginFuncDef, funcNode) ++ args.map(tkNode) ++ TK(returnType) ++
          tokenize(body) ++ TK(endFuncDef)
      case ClassDef(classNode, superTypes, vars, funcDefs) =>
        TK(beginClassDef, classNode) ++
          superTypes.toVector.map(v => tkNode(v.node)) ++
          TK(beginObject) ++
          vars.toVector.flatMap { case (s, n) => TK(s, n) } ++
          funcDefs.flatMap(_._2.pipe(tokenize)) ++
          TK(endObject, endClassDef)
    }

    implicit private def justToken(token: Symbol): (Token, Target) = {
      (Name(token), None)
    }

    implicit private def tkNode(node: PNode): (Token, Target) = {
      val target = mapping(node) match {
        case Annot.User(t, _) => Some(node -> t)
        case _                => None
      }
      Name(node.nameOpt.getOrElse(missing)) -> target
    }
  }

}
