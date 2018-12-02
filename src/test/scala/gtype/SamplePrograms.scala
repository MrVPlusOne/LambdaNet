package gtype

import gtype.GExpr.boolType
import gtype.GStmt.API._

object SamplePrograms {
  /*
      * let Point = { x: int, moveX: int => Point }
      *
      * let mkPoint: int -> Point =
      *   (x: int) => { x: x0, moveX: (dx: int) => mkPoint (x0 + dx) }
      *
      * let Point2D = { x: int, moveX: int => Point2D, y: int, moveY: int => Point2D }
      * */

  val point = 'Point
  val point2D = 'Point2D
  val numArray = 'NumArray

  val typeContext = TypeContext(
    baseTypes = Set('int, 'number, 'string, GExpr.boolType.id),
    typeUnfold = Map(
      point -> obj('x -> 'int, 'moveX -> (List('int) -: point)),
      point2D -> obj(
        'x -> 'int, 'moveX -> (List('int) -: point2D),
        'y -> 'int, 'moveY -> (List('int) -: point2D)),
      numArray -> obj(
        'length -> 'int,
        'slice -> (List('int, 'int) -: numArray),
        'access -> (List('int) -: 'number),
        'push -> (List('number) -: numArray)
      )
    ), subRel = Set(
      ('int: GType) -> 'number
    ))

  val exprContext: ExprContext = {
    val varAssign = Map[Symbol, GType](
      'lt -> (List('int, 'int) -: boolType),
      'plus -> (List('int, 'int) -: 'int),
      'minus -> (List('int, 'int) -: 'int),
      'times -> (List('int, 'int) -: 'int),
      'divide -> (List('number, 'number) -: 'number),
      'floor -> (List('number) -: 'int)
    )

    ExprContext(varAssign, typeContext)
  }

  object WellFormed {

    val ltExample: GStmt = {
      'lt.call(C(1, 'int), C(2, 'int))
    }

    /**
      *  function fact(x: int): int {
      *    if (x < 0) return 1 else return x * fact (x-1)
      *  }
      */
    val factExample: GStmt = {
      FUNC('fact, 'int)('x -> 'int) {
        IF('lt.call('x, C(0, 'int)))(
          RETURN(C(1, 'int))
        ).ELSE(
          RETURN('times.call('x, 'fact.call('minus.call('x, C(1, 'int)))))
        )
      }
    }

    /**
      *  type Point = { x: int, moveX: (int) -> Point }
      *
      *  function mkPoint(x: int): Point {
      *    function moveX(dx: int): Point {
      *      return mkPoint(x + dx)
      *    };
      *    return { x: x, moveX: moveX }
      *  }
      */
    val mkPointExample: GStmt = {
      FUNC('mkPoint, point)('x -> 'int)(
        FUNC('moveX, point)('dx -> 'int)(
          RETURN('mkPoint.call('plus.call('x, 'dx)))
        ),
        RETURN(mkObj('x -> 'x, 'moveX -> 'moveX))
      )
    }


    val all: Seq[(String, GStmt)] = Seq(
      ("ltExample", ltExample),
      ("factExample", factExample),
      ("mkPointExample", mkPointExample),
      //      ("merge array simplified", arrayMerge, numArray -: numArray -: numArray)
    )
  }

}
