package gtype

import gtype.GExpr.boolType
import gtype.GType.API._
import gtype.GExpr.API._

object SamplePrograms {
  /*
      * let Point = { x: int, moveX: int => Point }
      *
      * let mkPoint: int -> Point =
      *   (x: int) => { x: x0, moveX: (dx: int) => mkPoint (x0 + dx) }
      *
      * let Point2D = { x: int, moveX: int => Point2D, y: int, moveY: int => Point2D }
      * */

  val point = 0
  val point2D = 1
  val numArray = 2

  val typeContext = TypeContext(
    subRel = Set(
      ('int: GType) -> 'number
    ),
    typeUnfold = Map(
      point -> obj('x -> "int", 'moveX -> "int".arrow(point)),
      point2D -> obj(
        'x -> "int", 'moveX -> "int".arrow(point2D),
        'y -> "int", 'moveY -> "int".arrow(point2D)),
      numArray -> obj(
        'length -> 'int,
        'slice -> ('int -: 'int -: numArray),
        'access -> ('int -: 'number),
        'push -> ('number -: numArray)
      )
    ))

  val exprContext: ExprContext = {

    val varAssign = Map[Symbol, GType](
      'lt -> ('int -: 'int -: boolType),
      'plus -> ('int -: 'int -: 'int),
      'minus -> ('int -: 'int -: 'int),
      'times -> ('int -: 'int -: 'int),
      'divide -> ('number -: 'number -: 'number),
      'floor -> ('number -: 'int)
    )

    ExprContext(varAssign, typeContext)
  }

  object WellFormed {
    val ltExample: GExpr = {
      'lt.call(C(1, 'int), C(2, 'int))
    }

    val factExample: GExpr = {
      /*
    * let fact: int -> int =
    *   (x: int) => if x < 0 then 1 else x * fact (x-1)
    * in fact 5
    **/

      LET('fact, 'int -: 'int) {
        FUN('x, 'int) {
          IF('lt.call('x, C(0, 'int)), 'int) {
            C(1, 'int)
          } ELSE {
            'times.call('x, 'fact.call('minus.call('x, C(1, 'int))))
          }
        }
      } IN 'fact.call(C(5, 'int))
    }

    val mkPointExample: GExpr = {
      /*
    * let mkPoint: int -> Point =
    *   (x: int) => { x: x, moveX: (dx: int) => mkPoint (x + dx) }
    * in mkPoint 5
    *   */

      LET('mkPoint, 'int -: point){
        FUN('x, 'int) {
          mkObj('x -> 'x, 'moveX -> FUN('dx, 'int) { 'mkPoint.call('plus.call('x, 'dx)) })
        }
      } IN 'mkPoint.call(C(5, 'int))
    }

    val arrayMerge: GExpr = {
      FUN('left, numArray)(FUN('right, numArray){
        LET('array, numArray)(C("[]", numArray)) IN {
          LET('lIndex, 'int)(C(0, 'int)) IN {
            LET('whileCond, boolType)('lt.call('lIndex, 'left.m('length))) IN {
              LET('lItem, 'number)('left.m('access).call('lIndex)) IN {
                'array.m('push).call('lItem)
              }
            }
          }
        }
      })
    }

    val all: Seq[(String, GExpr, GType)] = Seq(
      ("ltExample", ltExample, boolType),
      ("factExample", factExample, 'int),
      ("mkPointExample", mkPointExample, point),
      ("merge array simplified", arrayMerge, numArray -: numArray -: numArray)
    )
  }
}
