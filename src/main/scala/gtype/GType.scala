package gtype

import org.scalacheck.Gen
import scala.language.implicitConversions

/**
  * T :=
  *   | Star
  *   | Base
  *   | T => T
  *   | [l: T, ...]
  */
sealed trait GType {
  override def toString: String = prettyPrint

  def prettyPrint: String = pPrint(0)

  def pPrint(envPriority: Int): String = {
    def wrap(priority: Int)(content: String) = {
      if(priority < envPriority) s"($content)" else content
    }

    this match {
      case BaseType(name) => name.name
      case AnyType => "?"
      case FuncType(from, to) => wrap(0)(from.pPrint(1) + "->" + to.pPrint(0))
      case ObjectType(fields) => fields.map{ case (l, t) => s"$l: ${t.pPrint(0)}"}.mkString("[", ", ", "]")
    }
  }

  def arrow(t: GType): FuncType = FuncType(this, t)
}

/** A [[BaseType]] or [[AnyType]] */
sealed trait GroundType extends GType

case class BaseType(name: Symbol) extends GroundType

/** The any type */
case object AnyType extends GroundType

case class FuncType(from: GType, to: GType) extends GType

case class ObjectType(fields: Set[(String, GType)]) extends GType


object GType {
  object API {
    implicit def symbol2base(symbol: Symbol): BaseType = BaseType(symbol)

    implicit def string2base(name: String): BaseType = BaseType(Symbol(name))

    def any: AnyType.type = AnyType

    def obj(fields: (String, GType)*): ObjectType = ObjectType(fields.toSet)
  }

  val simpleGroundGen: Gen[GroundType] = {
    Gen.frequency(
      1 -> Gen.const(AnyType),
      2 -> Gen.oneOf("x", "y", "z", "x1", "x2").map(API.string2base),
      1 -> (for (c <- Gen.alphaChar; s <- Gen.alphaNumStr.filter(_.length < 6))
              yield API.string2base(c +: s)))
  }

  val simpleNameGen: Gen[String] = {
    Gen.oneOf(
      Gen.oneOf("f", "g", "h", "field1"),
      for (c <- Gen.alphaChar; s <- Gen.alphaNumStr.filter(_.length < 6)) yield c +: s
    )
  }

  def funcGen(size: Int, groundGen: Gen[GroundType], fNameGen: Gen[String]): Gen[FuncType] = {
    val s0 = size - 1
    for (
      s1 <- Gen.choose(1, s0);
      l <- gTypeGen(s1, groundGen, fNameGen);
      r <- gTypeGen(s0 - s1, groundGen, fNameGen)
    ) yield l.arrow(r)
  }

  def objGen(size: Int, groundGen: Gen[GroundType], fNameGen: Gen[String]): Gen[ObjectType] = {
    Gen.choose(0,math.min(size, 3)).flatMap{ fNum =>
      var s0 = size - 1
      val fieldGen = for(fieldName <- fNameGen;
                         s1 <- Gen.choose(1, math.max(1,s0));
                         t <- gTypeGen(s1, groundGen, fNameGen)) yield {
        s0 -= s1
        (fieldName, t)
      }


      Gen.listOfN(fNum, fieldGen).map(API.obj)
    }
  }

  def gTypeGen(size: Int, groundGen: Gen[GroundType], fNameGen: Gen[String]): Gen[GType] = {
    if (size <= 1) groundGen
    else {
      Gen.frequency(
        2 -> groundGen,
        3 -> funcGen(size, groundGen, fNameGen),
        1 -> objGen(size, groundGen, fNameGen)
      )
    }
  }

  def simpleGTypeGen(size: Int = 40): Gen[GType] = {
    gTypeGen(size, simpleGroundGen, simpleNameGen)
  }

  def consistent(t1: GType, t2: GType): Boolean = ???
}