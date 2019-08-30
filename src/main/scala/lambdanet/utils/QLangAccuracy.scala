package lambdanet.utils

import lambdanet._
import lambdanet.train.{Correct, Counted}
import lambdanet.{Annot, PredictionSpace, train}
import lambdanet.translation.PredicateGraph.{PNode, PType, ProjNode}
import lambdanet.translation.QLang._

object QLangAccuracy {
  import cats.implicits._

  type Stat = Map[PNode, Int]

  def occurrenceMap(stmts: Vector[QStmt]): Stat = {
    def rec(stmt: QStmt): Stat = stmt match {
      case VarDef(n, init, _) =>
        Map(n -> 1) |+| recE(init)
      case AssignStmt(lhs, rhs) =>
        recE(lhs) |+| recE(rhs)
      case ExprStmt(e)      => recE(e)
      case ReturnStmt(e, _) => recE(e)
      case IfStmt(cond, b1, b2) =>
        recE(cond) |+| rec(b1) |+| rec(b2)
      case WhileStmt(cond, body) =>
        recE(cond) |+| rec(body)
      case BlockStmt(stmts) =>
        stmts.foldMap(rec)
      case FuncDef(_, args, ret, body) =>
        args.map(_ -> 1).toMap |+| Map(ret -> 1) |+| rec(body)
      case ClassDef(_, _, vars, fs) =>
        vars.values.map(_ -> 1).toMap |+|
          fs.valuesIterator.toList.foldMap(rec)
    }

    def recE(expr: QExpr): Stat = expr match {
      case Var(n) => Map(n -> 1)
      case FuncCall(f, args) =>
        recE(f) |+| args.foldMap(recE)
      case Cast(e, _) =>
        recE(e)
      case ObjLiteral(fields) =>
        fields.values.toList.foldMap(recE)
      case Access(e, _) => recE(e)
      case IfExpr(c, e1, e2) =>
        recE(c) |+| recE(e1) |+| recE(e2)
    }

    stmts.foldMap(rec)
  }

  // fixme: the label set is much smaller than the one obtained from forward
  case class FseAccuracy(modules: Vector[QModule], predSpace: PredictionSpace) {
    private val occurrence = modules.foldMap { m =>
      occurrenceMap(m.stmts)
    }

    /** non-inferred project node annotations (only library types) */
    val annots = modules.flatMap {
      _.mapping.collect {
        case (k, Annot.User(t, false)) if k.fromProject => k -> t
      }
    }.toMap

    /** Count top-N correctness */
    def countTopNCorrect(
        n: Int,
        predictions: Map[PNode, Vector[PType]],
        onlyCountInSpaceTypes: Boolean
    ): (Counted[Correct], Set[PNode], Set[PNode]) = {
      val annots1 = if (onlyCountInSpaceTypes) annots.filter {
        case (k, t) => predictions.contains(k) && predSpace.allTypes.contains(t)
      } else annots
      QLangAccuracy.countTopNCorrect(
        n,
        annots1,
        predictions,
        occurrence.getOrElse(_, 0)
      )
    }
  }

  def countTopNCorrect(
      n: Int,
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, Vector[PType]],
      nodeWeight: PNode => Int,
      warnMissingPredictions: Boolean = false
  ): (Counted[Correct], Set[PNode], Set[PNode]) = {
    val d = nodesToPredict.keySet.diff(predictions.keySet)
    assert(d.isEmpty, s"Some nodes lack predictions: ${d}")

    val (rightSet, wrongSet) =
      nodesToPredict.foldLeft((Set[PNode](), Set[PNode]())) {
        case ((yes, no), (node, t)) =>
          val rightQ = predictions(node).take(n).contains(t)
          if (rightQ) (yes + node, no) else (yes, no + node)
      }
    val y1 = rightSet.toSeq.map(nodeWeight).sum
    val n1 = wrongSet.toSeq.map(nodeWeight).sum
    (Counted(y1 + n1, y1), rightSet, wrongSet)
  }

  def topNAccuracy(
      n: Int,
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, Vector[PType]],
      nodeWeight: PNode => Int
  ): (Double, Int, Int) = {
    val c = countTopNCorrect(n, nodesToPredict, predictions, nodeWeight)._1
    (train.toAccuracy(c), c.value, c.count)
  }

  def top1Accuracy(
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, PType],
      nodeWeight: PNode => Int
  ): (Double, Int, Int) = {
    val pred1 = predictions.mapValuesNow(Vector(_))
    val c = countTopNCorrect(1, nodesToPredict, pred1, nodeWeight)._1
    (train.toAccuracy(c), c.value, c.count)
  }
}
