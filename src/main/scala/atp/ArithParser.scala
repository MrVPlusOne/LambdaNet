package atp

import fastparse._, MultiLineWhitespace._
import ArithExpr._

//todo: unit test this parser
object ArithParser extends App {
  def parseVar[_:P] = P(CharIn("a-zA-Z").! ~ CharsWhileIn("a-zA-Z0-9").?.!).map{
    case (x,xs) => Var(x+xs)
  }

  def parseInt[_:P] = P( CharIn("0-9").rep(1).!.map(_.toInt) )

  def parseConst[_:P]: P[Const] = parseInt.map(Const)

  def parseSum[_:P]: P[Expr] = P(parseProduct.rep(min=1, sep="+")).map(xs => xs.reduce(Add))

  def parseProduct[_:P]: P[Expr] = P(parseAtom.rep(min=1, sep="*")).map(xs => xs.reduce(Mul))

  def parseAtom[_:P]: P[Expr] = P(("("~/ parseSum ~ ")") | parseConst | parseVar)

  def parseAll[_:P] = P(parseSum ~ End)

  def parseExpr(s: String): Expr = parse(s, parseAll(_)) match {
    case Parsed.Success(value, _) => value
    case f: Parsed.Failure => throw new Exception(f.trace().longMsg)
  }
}
