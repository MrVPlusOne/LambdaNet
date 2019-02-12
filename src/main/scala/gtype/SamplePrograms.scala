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



  // @formatter:off
  //noinspection TypeAnnotation
  object Collection {

    def wellFormed(stmt: => GStmt): Example = {
      typeHoleContext.reset()
      val b = TryBLOCK(stmt)
      val e = Example(b, typeHoleContext.holeTypeMap.toMap)
      e
    }

    val ltExample: Example = wellFormed(
      'lt.call(C(1, 'int), C(2, 'int))
    )

    /*
     *  function fact(x: int): int {
     *    if (x < 0) return 1 else return x * fact (x-1)
     *  }
     */
    val factExample: Example = wellFormed {
      FUNC('fact, 'int)('x -> 'int) {
        IF('lt.call('x, C(0, 'int)))(
          RETURN(C(1, 'int))
        ).ELSE(
          RETURN('times.call('x, 'fact.call('minus.call('x, C(1, 'int)))))
        )
      }
    }

    /*
     *  type Point = { x: int, moveX: (int) -> Point }
     *
     *  function mkPoint(x: int): Point {
     *    function moveX(dx: int): Point {
     *      return mkPoint(x + dx)
     *    };
     *    return { x: x, moveX: moveX }
     *  }
     */
    val mkPointExample: Example = wellFormed {
      FUNC('mkPoint, point)('x -> 'int)(
        FUNC('moveX, point)('dx -> 'int)(
          RETURN('mkPoint.call('plus.call('x, 'dx)))
        ),
        RETURN(mkObj('x -> 'x, 'moveX -> 'moveX))
      )
    }

    /*
     *  function mkPoint2D(x: int, y: int): Point2D {
     *    function moveX(dx: int): Point2D {
     *      return mkPoint2D(x + dx, y)
     *    };
     *
     *    // forward reference
     *    function moveY(dy: int): Point2D {
     *      return moveBoth(0, dy)
     *    };
     *
     *    function moveBoth(dx: int, dy: int): Point2D {
     *      return mkPoint2D(x + dx, y + dy)
     *    }
     *
     *    return { x: x, moveX: moveX, y: y, moveY: moveY, moveBoth: moveBoth }
     *  }
     */
    val mkPoint2DExample: Example = wellFormed {
      FUNC('mkPoint2D, point2D)('x -> 'int, 'y -> 'int)(
        FUNC('moveX, point2D)('dx -> 'int)(
          RETURN('mkPoint2D.call('plus.call('x, 'dx), 'y))
        ),
        FUNC('moveY, point2D)('dy -> 'int)(
          RETURN('moveBoth.call(C(0, 'int), 'plus.call('y, 'dy)))
        ),
        FUNC('moveBoth, point2D)('dx -> 'int, 'dy -> 'int)(
          RETURN('mkPoint2D.call('plus.call('x, 'dx), 'plus.call('y, 'dy)))
        ),
        RETURN(mkObj('x -> 'x, 'moveX -> 'moveX, 'y -> 'y, 'moveY -> 'moveY, 'moveBoth -> 'moveBoth))
      )
    }

    val mergeSortExample: Example = wellFormed(BLOCK(
      //    function mergeSort(array: number[]): number[] {
      //      if (array.length < 2) {
      //      return array;
      //    }
      //      const middle = Math.floor(array.length / 2);
      //      const left = array.slice(0, middle);
      //      const right = array.slice(middle);
      //
      //      return merge(mergeSort(left), mergeSort(right));
      //    }
      FUNC('mergeSort, numArray)('array -> numArray)(
        IF('lt.call('array.m('length), I(2)))(
          RETURN('array)
        ).NoElse,
        VAR('middle, 'int)('floor.call('divide.call('array.m('length), I(2)))),
        VAR('left, numArray)('array.m('slice).call(I(0), 'middle)),
        VAR('right, numArray)('array.m('slice).call('middle, undefined)),
        RETURN('merge.call('mergeSort.call('left), 'mergeSort.call('right)))
      ),
      //    function merge(left: number[], right: number[]): number[] {
      //      const array: number[] = [];
      //      let lIndex = 0;
      //      let rIndex = 0;
      //      while (lIndex + rIndex < left.length + right.length) {
      //        const lItem = left[lIndex];
      //        const rItem = right[rIndex];
      //        if (lItem == null) {
      //          array.push(rItem); rIndex++;
      //        } else if (rItem == null) {
      //          array.push(lItem); lIndex++;
      //        } else if (lItem < rItem) {
      //          array.push(lItem); lIndex++;
      //        } else {
      //          array.push(rItem); rIndex++;
      //        }
      //      }
      //      return array;
      //    }
      FUNC('merge, numArray)(('left, numArray), ('right, numArray))(
        VAR('array, numArray)('emptyArray),
        VAR('lIndex)(I(0)),
        VAR('rIndex)(I(0)),
        WHILE('lt.call('plus.call('lIndex, 'rIndex), 'plus.call('left.m('length), 'right.m('length))))(
          VAR('lItem)('left.m('access).call('lIndex)),
          VAR('rItem)('right.m('access).call('rIndex)),
          IF('eq.call('lItem, undefined))(
            'array.m('push).call('lItem),
            'lIndex := 'plus.call('lIndex, I(1))
          ).ELSE(
            IF('eq.call('rItem, undefined))(
              'array.m('push).call('lItem),
              'rIndex := 'plus.call('rIndex, I(1))
            ).NoElse // ...
          )
        ),
        RETURN(I(0))
      )
    ))

    val polygonExample = wellFormed(BLOCK(
      CLASS('Polygon)(
        'name -> 'string,
        'height -> 'int,
        'width -> 'int
      )(
        FUNC('Polygon, any)(('height, 'int), ('width, 'int))(
          'this.m('name) := Const("Polygon", 'string),
          'this.m('width) := 'width,
          'this.m('height) := 'height
        )
      ),
      CLASS('Square, superType = Some('Polygon))()(
        FUNC('Square, any)('length -> 'int)(
          'Polygon.call('length, 'length),
          'this.m('name) := Const("Square", 'string)
        )
      )
    ))

    val linkedListExample = {
      val linkedList = 'LinkedList
      val linkedListNode = 'LinkedListNode
      wellFormed(BLOCK(
        CLASS(linkedList)(
          'head -> linkedListNode,
          'tail -> linkedListNode,
          'compare -> 'Comparator
        )(
          FUNC(linkedList, any)('compare -> 'Comparator)(
            'this.m('head) := undefined,
            'this.m('tail) := undefined,
            'this.m('compare) := 'compare
          ),
          // /**
          //  * @param {*} value
          //  * @return {LinkedList}
          //  */
          //  prepend(value) {
          //    // Make new node to be a head.
          //    const newNode = new LinkedListNode(value, this.head);
          //    this.head = newNode;
          //
          //    // If there is no tail yet let's make new node a tail.
          //    if (!this.tail) {
          //      this.tail = newNode;
          //    }
          //
          //    return this;
          //  }
          FUNC('prepend, linkedList)('value -> any)(
            VAR('newNode)(linkedListNode.call('value, 'this.m('head))),
            'this.m('head) := 'newNode,
            IF('not.call('this.m('tail)))(
              'this.m('tail) := 'newNode
            ).NoElse,
            RETURN('this)
          )
        ),

        //  export default class LinkedListNode {
        //    constructor(value, next = null) {
        //      this.value = value;
        //      this.next = next;
        //    }
        //
        //    toString(callback) {
        //      return callback ? callback(this.value) : `${this.value}`;
        //    }
        //  }
        CLASS(linkedListNode)(
          'value -> any,
          'next -> linkedListNode
        )(
          FUNC(linkedListNode, any)(('value, any), ('next, linkedListNode))(
            'this.m('value) := 'value,
            'this.m('next) := 'next
          ),
          FUNC('toString, any)('callback -> (List(any) -: any))(
            RETURN(IfExpr('not.call('callback),
              'this.m('value),
              'callback.call('this.m('value)),
              any))
          )
        )
      ))
    }

    val all: Seq[(String, Example)] = Seq(
      ("ltExample", ltExample),
      ("factExample", factExample),
      ("mkPointExample", mkPointExample),
      ("mkPoint2DExample", mkPoint2DExample),
      ("mergeSortExample", mergeSortExample),
      ("polygonExample", polygonExample),
      ("linkedListExample", linkedListExample)
    )
  }
  // @formatter:on
}
