package lambdanet.utils

import lambdanet.train.{Correct, Counted}
import lambdanet.{Annot, train}
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

  case class FseAccuracy(modules: Vector[QModule]){
    private val occurrence = modules.foldMap { m =>
      occurrenceMap(m.stmts)
    }

    private val annots = modules.flatMap {
      _.mapping.collect {
        case (k, Annot.User(t)) if t.madeFromLibTypes => k -> t
      }
    }.toMap

    def countCorrect(predictions: Map[ProjNode, PType]): Counted[Correct] = {
      val preds = predictions.map { case (k, v) => k.n -> v }
      QLangAccuracy.countCorrect(annots, preds, occurrence.getOrElse(_, 0))
    }
  }

  def countCorrect(
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, PType],
      nodeWeight: PNode => Int,
  ): Counted[Correct] = {
    val (y, n) = nodesToPredict.foldLeft((0, 0)) {
      case ((yes, no), (n, t)) =>
        val rightQ = predictions.get(n) match {
          case Some(t1) => t1 == t
          case None     => false
        }
        val w = nodeWeight(n)
        if (rightQ) (yes + w, no) else (yes, no + w)
    }
    Counted(y + n, y)
  }

  def measureAcc(
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, PType],
      nodeWeight: PNode => Int,
  ): (Double, Int, Int) = {
    val c = countCorrect(nodesToPredict, predictions, nodeWeight)
    (train.toAccuracy(c), c.value, c.count)
  }

}