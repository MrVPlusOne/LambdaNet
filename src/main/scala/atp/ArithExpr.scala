package atp

import org.scalacheck.Gen

object ArithExpr{
  sealed trait Expr {
    def show: String = prettyPrint(this, 0)

    override def toString: String = show
  }

  case class Var(name: String) extends Expr

  case class Const(value: Int) extends Expr

  case class Add(x1: Expr, x2: Expr) extends Expr

  case class Mul(x1: Expr, x2: Expr) extends Expr

  def prettyPrint(expr: Expr, outerPrecedence: Int): String = {
    def wrap(prec: Int, s: String) = {
      if (outerPrecedence > prec) s"($s)" else s
    }
    expr match {
      case Var(name) => name
      case Const(n) => n.toString
      case Add(x1, x2) =>
        wrap(2, s"${prettyPrint(x1, 2)} + ${prettyPrint(x2, 3)}")
      case Mul(x1, x2) =>
        wrap(4, s"${prettyPrint(x1, 4)} * ${prettyPrint(x2, 5)}")
    }
  }


  def simplify1(expr: Expr): Expr = expr match {
    case Add(Const(m), Const(n)) => Const(m+n)
    case Mul(Const(m), Const(n)) => Const(m*n)
    case Add(Const(0),x) => x
    case Add(x,Const(0)) => x
    case Mul(Const(0),x) => Const(0)
    case Mul(x,Const(0)) => Const(0)
    case Mul(Const(1),x) => x
    case Mul(x,Const(1)) => x
    case _ => expr
  }

  /**
    * However, simplifying just once is not necessarily adequate; we would like instead to simplify repeatedly until no further progress is possible. To do this, let us apply the above function in a bottom-up sweep through an expres- sion tree, which will simplify in a cascaded manner.
    */
  def simplify(expr: Expr): Expr = expr match {
    case Add(e1,e2) => simplify1(Add(simplify(e1),simplify(e2)))
    case Mul(e1,e2) => simplify1(Mul(simplify(e1),simplify(e2)))
    case _ => simplify1(expr)
  }

  implicit class ArithStringBuild(val ctx: StringContext) extends AnyVal {
    def ae(args: Any*): Expr = {
      assert(ctx.parts.length == 1)
      ArithParser.parseExpr(ctx.parts.head)
    }
  }

  val varGen: Gen[Var] = {
    for (n <- Gen.oneOf(Gen.oneOf("x", "y", "z", "x1", "x2"),
      for(c <- Gen.alphaChar; s <- Gen.alphaNumStr) yield c +: s)) yield Var(n)
  }

  val constGen: Gen[Const] = Gen.choose(0,100).map(Const)

  def binaryGen(size: Int, bF: (Expr, Expr) => Expr): Gen[Expr] = {
    for(
      s1 <- Gen.choose(1,size);
      l <- arithGenWithSize(s1);
      r <- arithGenWithSize(size - s1)
    ) yield bF(l,r)
  }

  def arithGenWithSize(size: Int): Gen[Expr] = {
    if(size <= 1) Gen.oneOf(varGen, constGen)
    else {
      Gen.frequency(
        1 -> varGen,
        1 -> constGen,
        3 -> Gen.oneOf(Add, Mul).flatMap(bf => binaryGen(size, bf))
      )
    }
  }

  val arithGen: Gen[Expr] = Gen.sized(arithGenWithSize)

}

