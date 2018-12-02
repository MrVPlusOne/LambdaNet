package gtype

import org.scalacheck.Gen
import scala.language.implicitConversions

/** A gradual type annotation, either a [[GType]] or a [[GTHole]] */
sealed trait GTMark

/** An annotation hole that needs to be inferred. */
case class GTHole(id: Int) extends GTMark


/**
  * T :=                  ([[GType]])
  * | ?                   ([[AnyType]])
  * | b                   ([[BaseType]])
  * | tv                  ([[TyVar]])
  * | (T, ..., T) -> T    ([[FuncType]])
  * | {l: T, ..., l: T}   ([[ObjectType]])
  *
  * where l is [[String]]
  */
sealed trait GType extends GTMark{
  override def toString: String = prettyPrint

  def prettyPrint: String = pPrint(0, id => s"#$id")

  def pPrint(envPriority: Int, showVar: Int => String): String = {
    def wrap(priority: Int)(content: String) = {
      if (priority < envPriority) s"($content)" else content
    }

    this match {
      case BaseType(name) => name.name
      case AnyType => "*"
      case TyVar(id) => showVar(id)
      case FuncType(from, to) => wrap(0)(
        from.map(_.pPrint(1, showVar)).mkString("(", ",", ")") + "->" + to.pPrint(0, showVar))
      case ObjectType(fields) => fields.map {
        case (l, t) => s"${l.name}: ${t.pPrint(0, showVar)}" }.mkString("{", ", ", "}")
    }
  }

  def astSize: Int = this match {
    case _: GroundType => 1
    case _: TyVar => 1
    case FuncType(from, to) => from.map(_.astSize).sum + to.astSize + 1
    case ObjectType(fields) => fields.map(_._2.astSize).sum + 1
  }

  def -:[T](from: List[T])(implicit conv: T => GType) = FuncType(from.map(conv), this)
}

/** A [[BaseType]] or [[AnyType]] */
sealed trait GroundType extends GType

case class BaseType(name: Symbol) extends GroundType

/** The any type */
case object AnyType extends GroundType

case class TyVar(id: Int) extends GType

case class FuncType(from: List[GType], to: GType) extends GType

case class ObjectType(fields: Map[Symbol, GType]) extends GType


/**
  * A context used for checking consistent-subtyping relation
  * @param subRel records the currently assumed sub typing relations
  * @param typeUnfold defines how to unfold a type var once
  */
case class TypeContext(subRel: Set[(GType, GType)],
                       typeUnfold: Map[Int, ObjectType]){
  def isSubType(child: GType, parent: GType): Boolean = {
    GType.checkSubType(child, parent, this).nonEmpty
  }

  def mkSubTypeError(child: GType, parent: GType): Option[SubTypeError] = {
    if(!isSubType(child, parent)){
      Some(SubTypeError(child, parent))
    } else None
  }
}

object GType {

  object API {
    implicit def symbol2base(symbol: Symbol): BaseType = BaseType(symbol)

    implicit def string2base(name: String): BaseType = BaseType(Symbol(name))

    implicit def int2tyVar(id: Int): TyVar = TyVar(id)

    def any: AnyType.type = AnyType

    def obj(fields: (Symbol, GType)*): ObjectType = ObjectType(fields.toMap)
  }

  /** the consistent-subtyping relation */
  def checkSubType(child: GType, parent: GType,
                   context: TypeContext): Option[TypeContext] = {
    import context._

    if (child == AnyType || parent == AnyType || subRel.contains(child -> parent))
      return Some(context)

    lazy val context1 = context.copy(subRel + (child -> parent))

    (child, parent) match {
      case (b1: BaseType, b2: BaseType) => if (b1 == b2) Some(context) else None
      case (FuncType(cFrom, c2), FuncType(pFrom, p2)) =>
        if(cFrom.length != pFrom.length){
          return None  // arity different
        }
        var currentContext = context1
        pFrom.zip(cFrom).foreach{ case (p, c) =>
          checkSubType(p, c, currentContext) match {
            case None => return None  // arg type mismatch
            case Some(ctx) => currentContext = ctx
          }
        }
        checkSubType(c2, p2, currentContext)
      case (TyVar(id), _) => checkSubType(typeUnfold(id), parent, context1)
      case (_, TyVar(id)) => checkSubType(child, typeUnfold(id), context1)
      case (ObjectType(fields1), ObjectType(fields2)) =>
        var currentContext = context1
        fields2.foreach { case (l, lt) =>
          fields1.get(l).flatMap{ lt1 =>
            checkSubType(lt1, lt, currentContext)
          } match {
            case None => return None // filed missing, not subtype
            case Some(c) => currentContext = c
          }
        }
        Some(currentContext)
      case _ => None
    }
  }


  // === GType random sampling ===

  val shortSymbolGen: Gen[Symbol] = {
    for(
      c <- Gen.alphaChar;
      suffixLen <- Gen.choose(0,4);
      s <- Gen.listOfN(suffixLen, Gen.alphaNumChar)) yield Symbol((c +: s).mkString)
  }

  val simpleGroundGen: Gen[GroundType] = {
    Gen.frequency(
      1 -> Gen.const(AnyType),
      2 -> Gen.oneOf("x", "y", "z", "x1", "x2").map(API.string2base),
      1 -> shortSymbolGen.map(API.symbol2base))
  }

  val simpleNameGen: Gen[Symbol] = {
    Gen.oneOf(
      Gen.oneOf('f, 'g, 'h),
      shortSymbolGen
    )
  }

  def funcGen(size: Int)(implicit params: GTypeGenParams): Gen[FuncType] = {
    if(size < 2){
      println(s"funcGen size: $size")
    }

    Gen.choose(1, math.min(size - 1, 4)).flatMap { argNum =>
      var s0 = size - argNum - 1
      val argGen =
        for (s1 <- Gen.choose(1, math.max(1, s0/2));
             t <- gTypeGen(s1)) yield {
          s0 -= s1
          t
        }


      Gen.listOfN(argNum, argGen).map{ ats => FuncType(ats.tail, ats.head)}
    }
  }

  def objGen(size: Int)(implicit params: GTypeGenParams): Gen[ObjectType] = {
    import params._
    if(size < 1){
      println(s"objGen size: $size")
    }
    Gen.choose(1, math.min(size, 3)).flatMap { fNum =>
      var s0 = size - fNum - 1
      val fieldGen =
        for (fieldName <- fNameGen;
             s1 <- Gen.choose(1, math.max(1, s0));
             t <- gTypeGen(s1)) yield {
          s0 -= s1
          (fieldName, t)
        }


      Gen.listOfN(fNum, fieldGen).map(API.obj)
    }
  }

  def gTypeGen(size: Int)(implicit params: GTypeGenParams): Gen[GType] = {
    import params._
    if (size <= 1) {
      Gen.frequency(
        5 -> groundGen,
        1 -> Gen.const(API.obj())
      )
    }
    else if(size <= 10) Gen.frequency(
      1 -> groundGen,
      1 -> tyVarGen,
      3 -> funcGen(size),
      3 -> objGen(size)
    ) else {
      Gen.frequency(
        4 -> funcGen(size),
        5 -> objGen(size),
        1 -> tyVarGen
      )
    }
  }

  case class GTypeGenParams(groundGen: Gen[GroundType],
                            fNameGen: Gen[Symbol], tyVarGen: Gen[TyVar])

  def contextGen(tyVarNum: Int, objectGen: Gen[ObjectType]): Gen[TypeContext] = {
    for(tys <- Gen.listOfN(tyVarNum, objectGen);
        map = tys.indices.zip(tys).toMap) yield
      TypeContext(subRel = Set(), typeUnfold = map)
  }

  /** TyVar ranges from 0 to 2 */
  val simpleGenParams = GTypeGenParams(simpleGroundGen, simpleNameGen, Gen.choose(0,2).map{TyVar})

  /** TyVar ranges from 0 to 2 */
  val simpleGTypeGen: Gen[GType] = {
    Gen.sized{ size => gTypeGen(size)(simpleGenParams) }
  }

  /** TyVar ranges from 0 to 2 */
  val simpleContextGen: Gen[TypeContext] = {
    contextGen(3, objGen(25)(simpleGenParams))
  }

  // === End of GType random sampling ===
}