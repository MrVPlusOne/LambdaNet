package gtype

import gtype.GType.boolType
import gtype.GStmt.API._
import gtype.GStmt.TypeHoleContext

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
    baseTypes = Set('int, 'number, 'string, boolType.id),
    typeUnfold = Map(
      point -> obj('x -> 'int, 'moveX -> (List('int) -: point)),
      'PointAlias -> obj('x -> 'int, 'moveX -> (List('int) -: 'PointAlias)),
      point2D -> obj(
        'x -> 'int,
        'moveX -> (List('int) -: point2D),
        'y -> 'int,
        'moveY -> (List('int) -: point2D)
      ),
      numArray -> obj(
        'length -> 'int,
        'slice -> (List('int, 'int) -: numArray),
        'access -> (List('int) -: 'number),
        'push -> (List('number) -: numArray)
      ),
      'Comparator -> obj(
        'equal -> (List(any, any) -: boolType)
      )
    ),
    subRel = Set(
      ('int: GType) -> 'number
    )
  )

  val exprContext: ExprContext = {
    val varAssign = Map[Symbol, GType](
      'eq -> (List(any, any) -: boolType),
      'not -> (List(any) -: boolType),
      'lt -> (List('int, 'int) -: boolType),
      'plus -> (List('int, 'int) -: 'int),
      'minus -> (List('int, 'int) -: 'int),
      'times -> (List('int, 'int) -: 'int),
      'divide -> (List('number, 'number) -: 'number),
      'floor -> (List('number) -: 'int),
      'emptyArray -> numArray
    )

    ExprContext(varAssign, typeContext)
  }

  case class Example(program: BlockStmt, holeTypeMap: Map[GTHole, GType])
  // @formatter:on
}
