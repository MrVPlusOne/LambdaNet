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

  case class FseAccuracy(modules: Vector[QModule]) {
    private val occurrence = modules.foldMap { m =>
      occurrenceMap(m.stmts)
    }

    /** non-inferred project node annotations (only library types) */
    val annots = modules.flatMap {
      _.mapping.collect {
        case (k, Annot.User(t, false)) if k.fromProject && t.madeFromLibTypes =>
          k -> t
      }
    }.toMap

    /** Count top-N correctness */
    def countTopNCorrect(
        n: Int,
        predictions: Map[PNode, Vector[PType]],
    ): Counted[Correct] = {
      QLangAccuracy.countTopNCorrect(
        n,
        annots,
        predictions,
        occurrence.getOrElse(_, 0),
      )
    }
  }

  def countTopNCorrect(
      n: Int,
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, Vector[PType]],
      nodeWeight: PNode => Int,
      warnMissingPredictions: Boolean = false
  ): Counted[Correct] = {
    val (y1, n1) = nodesToPredict.foldLeft((0, 0)) {
      case ((yes, no), (node, t)) =>
        val rightQ = predictions.get(node) match {
          case Some(t1) => t1.take(n).contains(t)
          case None =>
            if(warnMissingPredictions)
              lambdanet.printWarning(s"Prediction missing for $node of type $t")
            false
        }
        val w = nodeWeight(node)
        if (rightQ) (yes + w, no) else (yes, no + w)
    }
    Counted(y1 + n1, y1)
  }

  def topNAccuracy(
      n: Int,
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, Vector[PType]],
      nodeWeight: PNode => Int,
  ): (Double, Int, Int) = {
    val c = countTopNCorrect(n, nodesToPredict, predictions, nodeWeight)
    (train.toAccuracy(c), c.value, c.count)
  }

  def top1Accuracy(
      nodesToPredict: Map[PNode, PType],
      predictions: Map[PNode, PType],
      nodeWeight: PNode => Int,
  ): (Double, Int, Int) = {
    val pred1 = predictions.mapValuesNow(Vector(_))
    val c = countTopNCorrect(1, nodesToPredict, pred1, nodeWeight)
    (train.toAccuracy(c), c.value, c.count)
  }
}
