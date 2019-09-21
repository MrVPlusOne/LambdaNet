package lambdanet

import lambdanet.GType.API._
import lambdanet.Surface.{ExprContext, GExpr, Var}

//noinspection TypeAnnotation
object JSExamples {

  val number = 'number
  val string = 'string
  val boolean: Symbol = GType.boolType.id
  val void: Symbol = GType.voidType.id
  val anyArray = 'Array
  val generator = 'Array

  implicit class ExpressionSyntax[E](e: E)(implicit conv: E => GExpr) {
    val expr: GExpr = conv(e)

    def +(other: GExpr): GExpr = (expr m 'OP_Plus).call(other)

    def -(other: GExpr): GExpr = (expr m 'OP_Minus).call(other)

    def *(other: GExpr): GExpr = (expr m 'OP_Times).call(other)

    def /(other: GExpr): GExpr = (expr m 'OP_Divide).call(other)

    def <(other: GExpr): GExpr = (expr m 'OP_LessThan).call(other)

    def ===(other: GExpr): GExpr = Var('eq).call(expr, other)

    def &&(other: GExpr): GExpr = Var('OP_And).call(expr, other)

    def unary_! : GExpr = Var('OP_Not).call(expr)
  }

  def baseType(name: Symbol): (Symbol, CompoundType) = {
    name -> obj(Symbol(name.name + "_UNIQUE") -> name)
  }

  val operatorTokens = Seq(
    'LessThanToken, 'LessThanSlashToken, 'GreaterThanToken,
    'LessThanEqualsToken, 'GreaterThanEqualsToken, 'EqualsEqualsToken,
    'ExclamationEqualsToken, 'EqualsEqualsEqualsToken,
    'ExclamationEqualsEqualsToken, 'EqualsGreaterThanToken, 'PlusToken,
    'MinusToken, 'AsteriskToken, 'AsteriskAsteriskToken, 'SlashToken,
    'PercentToken, 'PlusPlusToken, 'MinusMinusToken, 'LessThanLessThanToken,
    'GreaterThanGreaterThanToken, 'GreaterThanGreaterThanGreaterThanToken,
    'AmpersandToken, 'BarToken, 'CaretToken, 'ExclamationToken, 'TildeToken,
    'AmpersandAmpersandToken, 'BarBarToken, 'QuestionToken, 'ColonToken,
    'AtToken, 'EqualsToken, 'PlusEqualsToken, 'MinusEqualsToken,
    'AsteriskEqualsToken, 'AsteriskAsteriskEqualsToken, 'SlashEqualsToken,
    'PercentEqualsToken, 'LessThanLessThanEqualsToken,
    'GreaterThanGreaterThanEqualsToken,
    'GreaterThanGreaterThanGreaterThanEqualsToken, 'AmpersandEqualsToken,
    'BarEqualsToken, 'CaretEqualsToken, 'FirstCompoundAssignment
  )

  val specialVars = Map[Symbol, GType](
    undefinedSymbol -> any,
    '$TypeOf -> (List(any) -: string),
    '$Spread -> (List(anyArray) -: any),
    '$Case -> (List(number) -: void),
    '$Switch -> (List(number) -: void),
    '$Delete -> (List(any) -: void),
    '$ArrayAccess -> (List(anyArray) -: any),
    '$Yield -> (List(any) -: generator),
    '$Await -> (List('Promise) -: void),
    '$Template -> (List(any) -: string),
    thisSymbol -> any
  ) ++ operatorTokens.map { s =>
    s -> any
  } ++ operatorTokens.map { s =>
    Symbol("POST_" + s.name) -> any
  }

  def treatAsAny(name: String): (Symbol, AnyType.type) = {
    Symbol(name) -> any
  }
}
