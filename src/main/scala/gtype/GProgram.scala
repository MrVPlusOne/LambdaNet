package gtype

case class GProgram(typeBindings: Map[String, GType]) {

}


/*
* Example programs:
*
* let fact: int -> int =
*   (x: int) => if x < 0 then 1 else x * fact (x-1)
*
* let Point = { x: int, moveX: int => Point }
*
* let mkPoint: int -> Point =
*   (x: int) => { x: x0, moveX: (dx: int) => mkPoint (x0 + dx) }
*
* let Point2D = { x: int, moveX: int => Point2D, y: int, moveY: int => Point2D }
*
*
*
* */