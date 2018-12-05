package gtype

import gtype.GExpr.boolType
import gtype.GStmt.API._
import SamplePrograms.Example

import scala.collection.mutable.ListBuffer

object JSExamples {

  val number = 'number
  val string = 'string
  val boolean: Symbol = boolType.id
  val void = 'void
  val anyArray = 'anyArray
  val numArray = 'numberArray

  def mkArrayType(baseType: GroundType): (Symbol, ObjectType) = {
    val arrayType = Symbol(s"${baseType.id.name}Array")
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
  }

  def I(i: Int): Const = Const(i, number)

  val THIS: Symbol = ClassDef.thisSymbol

  val SUPER: Symbol = ClassDef.superSymbol

  val typeContext = TypeContext(
    baseTypes = Set(boolean, void),
    typeUnfold = Map(
      number -> obj(
        'OP_Plus -> (List(number) -: number),
        'OP_Minus -> (List(number) -: number),
        'OP_Times -> (List(number) -: number),
        'OP_Divide -> (List(number) -: number),
        'OP_LessThan -> (List(number) -: boolean),
      ),
      string -> obj(
        'OP_Plus -> (List(any) -: string),
        'charAt -> (List(number) -: string),
        'length -> number
      ),
      'Comparator -> obj(
        'equal -> (List(any, any) -: boolType)
      )
    ) ++ Seq[GroundType](boolean, void, number, string, any).map(mkArrayType).toMap // create array types for basic types
    , subRel = Set()
  )

  val exprContext: ExprContext = {
    val varAssign = Map[Symbol, GType](
      'eq -> (List(any, any) -: boolType),
      'not -> (List(any) -: boolType),
      'OP_And -> (List(any, any) -: boolType),
      'toBool -> (List(any) -: boolType),
      'emptyArray -> anyArray,
      'Math -> obj(
        'floor -> (List(number) -: number),
        'abs -> (List(number) -: number)
      )
    )

    ExprContext(varAssign, typeContext)
  }

  object Collection {

    import collection.mutable

    val all: ListBuffer[(String, Example)] = mutable.ListBuffer()

    def wellFormed(name: String)(stmts: GStmt*): Example = {
      val ex = Example(BLOCK(stmts: _*), Set())
      all += (name -> ex)
      ex
    }


    val factExample: Example = wellFormed("fact") {
      FUNC('fact, number)('x -> number) {
        IF('x < I(1))(
          RETURN(I(1))
        ).ELSE(
          RETURN('x * 'fact.call('x - I(1)))
        )
      }
    }

    val mergeSortExample: Example = wellFormed("mergeSort")(
      FUNC('mergeSort, numArray)('array -> numArray)(
        IF('array.m('length) < I(2))(
          RETURN('array)
        ).NoElse,
        VAR('middle)(('Math m 'floor).call(
          'array.m('length) / I(2))),
        VAR('left, numArray)('array.m('slice).call(I(0), 'middle)),
        VAR('right, numArray)('array.m('slice).call('middle, undefined)),
        RETURN('merge.call('mergeSort.call('left), 'mergeSort.call('right)))
      ),

      FUNC('merge, numArray)(('left, numArray), ('right, numArray))(
        VAR('array, numArray)('emptyArray),
        VAR('lIndex)(I(0)),
        VAR('rIndex)(I(0)),
        WHILE((Var('lIndex) + Var('rIndex)) < ('left.m('length) + 'right.m('length)))(
          VAR('lItem)('left.m('access).call('lIndex)),
          VAR('rItem)('right.m('access).call('rIndex)),
          IF('eq.call('lItem, undefined))(
            'array.m('push).call('lItem),
            'lIndex := ('lIndex + I(1))
          ).EIF('rItem === undefined)(
            'array.m('push).call('lItem),
            'rIndex := ('rIndex + I(1))
          ).EIF('lItem < 'rItem)(
            // array.push(lItem); lIndex++;
            ('array m 'push).call('lItem),
            'lIndex := ('lIndex + I(1))
          ).ELSE(
            ('array m 'push).call('rItem),
            'rIndex := ('rIndex + I(1))
          )
        ),
        RETURN('array)
      )
    )

    val linkedListExample: Example = {
      val linkedList = 'LinkedList
      val linkedListNode = 'LinkedListNode

      val currentNode = 'currentNode

      wellFormed("LinkedList")(
        CLASS(linkedList)(
          'head -> linkedListNode,
          'tail -> linkedListNode,
          'compare -> 'Comparator
        )(
          FUNC(linkedList, any)('compare -> 'Comparator)(
            THIS.m('head) := undefined,
            THIS.m('tail) := undefined,
            THIS.m('compare) := 'compare
          ),

          FUNC('prepend, linkedList)(('value, any))(
            VAR('newNode)(linkedListNode.call('value, THIS.m('head))),
            THIS.m('head) := 'newNode,
            IF('not.call(THIS.m('tail)))(
              THIS.m('tail) := 'newNode
            ).NoElse,
            RETURN(THIS)
          ),

          FUNC('append, linkedList)(('value, any))(
            VAR('newNode)(linkedListNode.call('value)),
            IF('not.call(THIS.m('head)))(
              THIS.m('head) := 'newNode,
              THIS.m('tail) := 'newNode,
              RETURN(THIS)
            ).NoElse,
            THIS.m('tail).m('next) := 'newNode,
            THIS.m('tail) := 'newNode,
            RETURN(THIS)
          ),

          FUNC('delete, linkedListNode)(('value, any))(
            IF('not.call(THIS.m('head)))(
              RETURN(undefined)
            ).NoElse,

            VAR('deletedNode)(undefined),
            WHILE(THIS.m('head) && THIS.m('compare).m('equal).call(
              THIS m 'head m 'value, 'value
            ))(
              'deletedNode := (THIS m 'head),
              (THIS m 'head) := (THIS m 'head m 'next)
            ),
            VAR(currentNode)(THIS m 'head),
            IF('toBool.call(currentNode))(
              WHILE('toBool.call(currentNode m 'next))(
                IF(('this m 'compare m 'equal).call(currentNode m 'next m 'value, 'value))(
                  'deletedNode := (currentNode m 'next),
                  (currentNode m 'next) := (currentNode m 'next m 'next)
                ).ELSE(
                  currentNode := (currentNode m 'next)
                )
              )
            ).NoElse,
            IF((THIS m 'compare m 'equal).call(THIS m 'tail m 'value, 'value))(
              (THIS m 'tail) := currentNode
            ).NoElse,
            RETURN('deletedNode)
          ),

          FUNC('toArray, anyArray)()(
            VAR('nodes)('emptyArray),
            VAR(currentNode)(THIS m 'head),
            WHILE('toBool.call(currentNode))(
              ('nodes m 'push).call(currentNode),
              currentNode := (currentNode m 'next)
            ),
            RETURN('nodes)
          ),

          FUNC('fromArray, linkedList)(('values, anyArray))(
            FUNC('$local0, void)('value -> any)(
              (THIS m 'append).call('value)
            ),
            ('values m 'forEach).call('$local0),
            RETURN(THIS)
          )
        ),

        CLASS(linkedListNode)(
          'value -> any,
          'next -> linkedListNode
        )(
          FUNC(linkedListNode, any)(('value, any), ('next, linkedListNode))(
            THIS.m('value) := 'value,
            THIS.m('next) := 'next
          ),
          FUNC('toString, any)('callback -> (List(any) -: any))(
            RETURN(IfExpr('not.call('callback),
              THIS.m('value),
              'callback.call(THIS.m('value)),
              any))
          )
        )
      )
    }

    val mutualRecursionExample: Example = {
      val XY = obj(('x, 'number), ('y, 'number))

      wellFormed("mutual recursion")(
        FUNC('f1, number)(('p, XY))(
          IF(('p m 'x) === I(0))(
            RETURN('p m 'y)
          ).ELSE(
            ('p m 'x) := ('p m 'x) - I(1),
            RETURN('f2.call('p))
          )
        ),
        FUNC('f2, number)(('p, XY))(
          ('p m 'y) := (('p m 'y) + I(1)),
          RETURN('f1 call 'p)
        )
      )
    }
  }

}
