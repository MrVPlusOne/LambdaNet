package gtype

import org.scalacheck.Gen
import scala.language.implicitConversions

/**
  * T :=
  * | Star
  * | Base
  * | T => T
  * | [l: T, ...]
  */
sealed trait GType {
  override def toString: String = prettyPrint

  def prettyPrint: String = pPrint(0)

  def pPrint(envPriority: Int): String = {
    def wrap(priority: Int)(content: String) = {
      if (priority < envPriority) s"($content)" else content
    }

    this match {
      case BaseType(name) => name.name
      case AnyType => "?"
      case FuncType(from, to) => wrap(0)(from.pPrint(1) + "->" + to.pPrint(0))
      case ObjectType(fields) => fields.map { case (l, t) => s"$l: ${t.pPrint(0)}" }.mkString("[", ", ", "]")
    }
  }

  def astSize: Int = this match {
    case _: GroundType => 1
    case FuncType(from, to) => from.astSize + to.astSize + 1
    case ObjectType(fields) => fields.map(_._2.astSize).sum + 1
  }

  def arrow(t: GType): FuncType = FuncType(this, t)
}

/** A [[BaseType]] or [[AnyType]] */
sealed trait GroundType extends GType

case class BaseType(name: Symbol) extends GroundType

/** The any type */
case object AnyType extends GroundType

case class FuncType(from: GType, to: GType) extends GType

case class ObjectType(fields: Map[String, GType]) extends GType


object GType {

  object API {
    implicit def symbol2base(symbol: Symbol): BaseType = BaseType(symbol)

    implicit def string2base(name: String): BaseType = BaseType(Symbol(name))

    def any: AnyType.type = AnyType

    def obj(fields: (String, GType)*): ObjectType = ObjectType(fields.toMap)
  }

  /** structural subtyping check */
  def isSubtype(child: GType, parent: GType,
                subRel: (BaseType, BaseType) => Boolean): Boolean = (child, parent) match {
    case (g1: GroundType, g2: GroundType) if g1 == g2 => true // reflexivity
    case (b1: BaseType, b2: BaseType) => subRel(b1, b2)
    case (FuncType(c1, c2), FuncType(p1, p2)) =>
      isSubtype(p1, c1, subRel) && isSubtype(c2, p2, subRel)
    case (ObjectType(fields1), ObjectType(fields2)) =>
      fields2.forall { case (l, lt) =>
        fields1.get(l) match {
          case None => false // filed missing, not subtype
          case Some(lt1) => isSubtype(lt1, lt, subRel)
        }
      }
    case _ => false
  }

  /** mask out a type using another type */
  def restrict(t: GType, mask: GType): GType = (t, mask) match {
    case (_, AnyType) => AnyType
    case (ObjectType(fields1), ObjectType(fields2)) =>
      ObjectType(fields1.map { case (l, lt) =>
        l -> fields2.get(l).map { m => restrict(lt, m) }.getOrElse(lt)
      })
    case (FuncType(t1, t2), FuncType(m1, m2)) =>
      FuncType(restrict(t1, m1), restrict(t2, m2))
    case _ => t
  }


  def consistent(t1: GType, t2: GType): Boolean = {
    restrict(t1, t2) == restrict(t2, t1)
  }


  // === GType random sampling ===

  val shortNameGen: Gen[String] = {
    for(
      c <- Gen.alphaChar;
      suffixLen <- Gen.choose(0,4);
      s <- Gen.listOfN(suffixLen, Gen.alphaNumChar)) yield (c +: s).mkString
  }

  val simpleGroundGen: Gen[GroundType] = {
    Gen.frequency(
      1 -> Gen.const(AnyType),
      2 -> Gen.oneOf("x", "y", "z", "x1", "x2").map(API.string2base),
      1 -> shortNameGen.map(API.string2base))
  }

  val simpleNameGen: Gen[String] = {
    Gen.oneOf(
      Gen.oneOf("f", "g", "h"),
      shortNameGen
    )
  }

  def funcGen(size: Int, groundGen: Gen[GroundType], fNameGen: Gen[String]): Gen[FuncType] = {
    if(size < 1){
      println(s"funcGen size: $size")
    }
    val s0 = size - 3
    for (
      s1 <- Gen.choose(1, math.max(1,s0));
      l <- gTypeGen(s1, groundGen, fNameGen);
      r <- gTypeGen(s0 - s1, groundGen, fNameGen)
    ) yield l.arrow(r)
  }

  def objGen(size: Int, groundGen: Gen[GroundType], fNameGen: Gen[String]): Gen[ObjectType] = {
    if(size < 1){
      println(s"objGen size: $size")
    }
    Gen.choose(1, math.min(size, 3)).flatMap { fNum =>
      var s0 = size - fNum - 1
      val fieldGen =
        for (fieldName <- fNameGen;
             s1 <- Gen.choose(1, math.max(1, s0));
             t <- gTypeGen(s1, groundGen, fNameGen)) yield {
          s0 -= s1
          (fieldName, t)
        }


      Gen.listOfN(fNum, fieldGen).map(API.obj)
    }
  }

  def gTypeGen(size: Int, groundGen: Gen[GroundType], fNameGen: Gen[String]): Gen[GType] = {
    if (size <= 1) {
      Gen.frequency(
        5 -> groundGen,
        1 -> Gen.const(API.obj())
      )
    }
    else if(size <= 10) Gen.frequency(
      1 -> groundGen,
      4 -> funcGen(size, groundGen, fNameGen),
      2 -> objGen(size, groundGen, fNameGen)
    ) else {
      Gen.frequency(
        1 -> funcGen(size, groundGen, fNameGen),
        1 -> objGen(size, groundGen, fNameGen)
      )
    }
  }

  val simpleGTypeGen: Gen[GType] = {
    Gen.sized{ size => gTypeGen(size, simpleGroundGen, simpleNameGen) }
  }

  // === End of GType random sampling ===
}