package lambdanet.surface

import lambdanet.surface.GStmt.API.{any, obj}
import lambdanet._
import GStmt.API._
import lambdanet.types.{
  AnyType,
  CompoundType,
  GType,
  GroundType,
  ObjectType,
  TyVar,
  TypeContext
}

object JSExamples {

  val number = 'number
  val string = 'string
  val boolean: Symbol = GType.boolType.id
  val void: Symbol = GType.voidType.id
  val anyArray = 'Array
  val function = 'Function //fixme: this should not be treated as object type
  val generator = 'Generator

  def mkArrayType(baseType: GroundType): (Symbol, ObjectType) = {
    val arrayType = if (baseType == any) 'Array else Symbol(s"${baseType.id.name}Array")
    arrayType -> obj(
      'length -> number,
      'pop -> (List() -: baseType),
      'push -> (List(baseType) -: number),
      'forEach -> (List(List(baseType) -: any) -: void),
      'slice -> (List(number, number) -: arrayType),
      'access -> (List(number) -: baseType)
    )
  }

  implicit class ExpressionSyntax[E](e: E)(implicit conv: E => GExpr) {
    val expr: GExpr = conv(e)

    def +(other: GExpr): GExpr = (expr m 'OP_Plus).call(other)

    def -(other: GExpr): GExpr = (expr m 'OP_Minus).call(other)

    def *(other: GExpr): GExpr = (expr m 'OP_Times).call(other)

    def /(other: GExpr): GExpr = (expr m 'OP_Divide).call(other)

    def <(other: GExpr): GExpr = (expr m 'OP_LessThan).call(other)

    def ===(other: GExpr): GExpr = 'eq.call(expr, other)

    def &&(other: GExpr): GExpr = 'OP_And.call(expr, other)

    def unary_! : GExpr = 'OP_Not.call(expr)
  }

  def baseType(name: Symbol): (Symbol, CompoundType) = {
    name -> obj(Symbol(name.name + "_UNIQUE") -> name)
  }

  val specialVars = Map[Symbol, GType](
    'undefined -> any,
    '$TypeOf -> (List(any) -: string),
    '$Spread -> (List(anyArray) -: any),
    '$Case -> (List(number) -: void),
    '$Switch -> (List(number) -: void),
    '$Delete -> (List(any) -: void),
    '$ArrayAccess -> (List(anyArray) -: any),
    '$Yield -> (List(any) -: generator),
    // operators
    'MinusToken -> (List(number) -: number),
    'PlusToken -> (List(number) -: number),
    'PlusPlusToken -> (List(number) -: number),
    'MinusMinusToken -> (List(number) -: number),
    'POST_PlusPlusToken -> (List(number) -: number),
    'POST_MinusMinusToken -> (List(number) -: number),
    'TildeToken -> (List(number) -: number),
    'ExclamationToken -> (List(any) -: boolean),
    thisSymbol -> any
  )

  @deprecated
  val exprContext: ExprContext = {
    var typeUnfold = Map(
      baseType(function), //fixme: not an object
      baseType('Map),
      baseType(void),
      baseType(boolean),
      number -> obj(
        'OP_Plus -> (List(number) -: number),
        'OP_Minus -> (List(number) -: number),
        'OP_Times -> (List(number) -: number),
        'OP_Divide -> (List(number) -: number),
        'OP_LessThan -> (List(number) -: boolean)
      ),
      string -> obj(
        'OP_Plus -> (List(any) -: string),
        'charAt -> (List(number) -: string),
        'length -> number
      ),
      'Comparator -> obj(
        'equal -> (List(any, any) -: boolean)
      )
    )

    var varAssign = Map[Symbol, GType](
      thisSymbol -> any,
      'eq -> (List(any, any) -: boolean),
      'toBool -> (List(any) -: boolean),
      'emptyArray -> anyArray,
      'Math -> obj(
        'floor -> (List(number) -: number),
        'abs -> (List(number) -: number)
      ),
      'isFinite -> (List(number) -: boolean),
      'Infinity -> number,
      'parseInt -> (List(string, number) -: number),
      'isNaN -> (List(number) -: boolean),
      'parseFloat -> (List(string) -: number)
    ) ++ specialVars

    def addType(name: Symbol): Unit = {
      typeUnfold += baseType(name)
      varAssign += (name -> any)
    }

    //todo: properly handle these: (query compiler for type definitions)
    Seq(
      'String,
      'Object,
      'Number,
      'Function,
      'Array,
      'Float64Array,
      'Uint32Array,
      'Error,
      'RangeError,
      'Window,
      'HTMLElement,
      generator,
      'Event,
      'CSSStyleDeclaration,
      'TransitionEvent,
      'Injector,
      'ReflectiveInjector,
      'ReflectiveInjector_,
      'Map,
      'Node,
      'RegExp,
      'WeakMap,
      'undefined,
      'Element,
      'Text,
      'Comment
    ).foreach(addType)

    Seq('super, 'window, 'global, 'self, 'document, 'setTimeout, 'getComputedStyle, 'JSON,
      'NaN, 'console)
      .foreach(s => {
        varAssign += (s -> any)
      })

    val typeContext = TypeContext(Set(), typeUnfold, Set())
    ExprContext(varAssign, typeContext)
  }

  @deprecated
  val typeContext: TypeContext = exprContext.typeContext

  @deprecated
  val libraryTypes: Set[Symbol] = typeContext.typeUnfold.keySet

  def treatAsAny(name: String): (Symbol, AnyType.type) = {
    Symbol(name) -> any
  }

  // @formatter:on
  @deprecated
  val realWorldExamples = {
    val binaryTreeNodeObj = obj(
      'leftHeight -> number,
      'height -> number,
      'balanceFactor -> number,
      'uncle -> 'BinaryTreeNode,
      'setValue -> (List(any) -: 'BinaryTreeNode),
      'setLeft -> (List('BinaryTreeNode) -: 'BinaryTreeNode),
      'removeChild -> (List('BinaryTreeNode) -: boolean),
      'replaceChild -> (List('BinaryTreeNode, 'BinaryTreeNode) -: boolean),
      'traverseInOrder -> (List() -: anyArray),
      'toString -> (List() -: string)
    )

    val heapObj = obj(
      'getLeftChildIndex -> (List(number) -: number),
      'hasParent -> (List(number) -: boolean),
      'hasRightChild -> (List(number) -: boolean),
      'leftChild -> (List(number) -: any),
      'swap -> (List(number, number) -: void),
      'poll -> (List() -: void),
      'add -> (List(any) -: 'Heap),
      'remove -> (List(any, TyVar('Comparator)) -: 'Heap),
      'find -> (List(any, TyVar('Comparator)) -: 'numberArray),
      'isEmpty -> (List() -: boolean),
      'toString -> (List() -: string)
    )

    val newUnfold = typeContext.typeUnfold ++ Map[Symbol, CompoundType](
      'BinarySearchTree -> obj(
        'insert -> (List(any) -: 'BinarySearchTreeNode),
        'contains -> (List(any) -: boolean),
        'toString -> (List() -: string)
      ),
      'BinaryTreeNode -> binaryTreeNodeObj,
      'BinarySearchTreeNode -> binaryTreeNodeObj.extended(
        'insert -> (List(any) -: 'BinarySearchTreeNode),
        'contains -> (List(any) -: boolean),
        'findMin -> (List() -: 'BinarySearchTreeNode)
      ),
      'RedBlackTree -> obj(
        'insert -> (List(any) -: 'BinarySearchTreeNode),
        'remove -> (List(any) -: boolean),
        'balance -> (List('BinarySearchTreeNode) -: void),
        'leftLeftRotation -> (List('BinarySearchTreeNode) -: 'BinarySearchTreeNode),
        'isNodeRed -> (List('BinarySearchTreeNode) -: boolean)
      ),
      'FenwickTree -> obj(
        'increase -> (List(number, number) -: 'FenwickTree),
        'query -> (List(number) -: number),
        'queryRange -> (List(number, number) -: number)
      ),
      'AvlTree -> obj(
        'insert -> (List(number) -: void),
        'remove -> (List(any) -: boolean),
        'balance -> (List('BinarySearchTreeNode) -: void)
      ),
      'Heap -> heapObj
    )

    typeContext.copy(typeUnfold = newUnfold)
  }
}
