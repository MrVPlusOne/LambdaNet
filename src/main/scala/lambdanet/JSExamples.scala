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
    // operators
    'MinusToken -> (List(number) -: number),
    'PlusToken -> (List(number) -: number),
    'PlusPlusToken -> (List(number) -: number),
    'MinusMinusToken -> (List(number) -: number),
    'POST_PlusPlusToken -> (List(number) -: number),
    'POST_MinusMinusToken -> (List(number) -: number),
    'TildeToken -> (List(number) -: number),
    'ExclamationToken -> (List(any) -: boolean),
    thisSymbol -> any,
  )

  def treatAsAny(name: String): (Symbol, AnyType.type) = {
    Symbol(name) -> any
  }
}
