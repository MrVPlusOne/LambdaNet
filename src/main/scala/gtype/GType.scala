package gtype

import org.scalacheck.Gen
import scala.language.implicitConversions

/** A gradual type annotation, either a [[GType]] or a [[GTHole]] */
sealed trait GTMark

/** An annotation hole that needs to be inferred. */
case class GTHole(id: Int) extends GTMark

// @formatter:off
/**
  * T :=                  ([[GType]])
  * | any                 ([[AnyType]])
  * | tv                  ([[TyVar]])
  * | (T, ..., T) -> T    ([[FuncType]])
  * | {l: T, ..., l: T}   ([[ObjectType]])
  *
  * where l is [[Symbol]]
  */
// @formatter:on
sealed trait GType extends GTMark {
  override def toString: String = prettyPrint

  def prettyPrint: String = pPrint(0)

  def pPrint(envPriority: Int): String = {
    def wrap(priority: Int)(content: String) = {
      if (priority < envPriority) s"($content)" else content
    }

    this match {
      case AnyType       => "any"
      case TyVar(symbol) => symbol.name
      case FuncType(from, to) =>
        wrap(0)(from.map(_.pPrint(1)).mkString("(", ",", ")") + "->" + to.pPrint(0))
      case ObjectType(fields) =>
        fields
          .map {
            case (l, t) => s"${l.name}: ${t.pPrint(0)}"
          }
          .mkString("{", ", ", "}")
    }
  }

  def astSize: Int = this match {
    case _: GroundType      => 1
    case FuncType(from, to) => from.map(_.astSize).sum + to.astSize + 1
    case ObjectType(fields) => fields.map(_._2.astSize).sum + 1
  }

  def -:[T](from: List[T])(implicit conv: T => GType) = FuncType(from.map(conv), this)
}

/** A [[TyVar]] or [[AnyType]] */
sealed trait GroundType extends GType {
  def id: Symbol
}

object GroundType {
  def symbolToType(symbol: Symbol): GroundType = {
    if (symbol == AnyType.id) AnyType
    else TyVar(symbol)
  }
}

/** The any type */
case object AnyType extends GroundType {
  def id: Symbol = 'any
}

case class TyVar(id: Symbol) extends GroundType

/** A [[FuncType]] or [[ObjectType]] */
sealed trait CompoundType extends GType

case class FuncType(from: List[GType], to: GType) extends CompoundType

case class ObjectType(fields: Map[Symbol, GType]) extends CompoundType {
  def extended(methods: (Symbol, CompoundType)*): ObjectType = {
    ObjectType(fields ++ methods.toMap)
  }

}

/**
  * A context used for checking consistent-subtyping relation
  *
  * @param baseTypes  the set of basic types
  * @param subRel     records the currently assumed sub typing relations
  * @param typeUnfold defines how to unfold a type var once
  */
case class TypeContext(baseTypes: Set[Symbol],
                       typeUnfold: Map[Symbol, CompoundType],
                       subRel: Set[(GType, GType)]) {

  def isSubtype(child: GType, parent: GType): Boolean = {
    GType.checkSubtype(child, parent, this).nonEmpty
  }

  def mkSubtypeError(child: GType, parent: GType): Set[SubtypeError] = {
    if (!isSubtype(child, parent)) {
      Set(SubtypeError(child, parent))
    } else Set()
  }

  def newTypeVar(name: Symbol, objectType: CompoundType): TypeContext = {
    if (typeUnfold.contains(name)) {
      println(s"warning: Redefine type var: ($name, $objectType), old value: ${typeUnfold(name)}")
    }
    copy(typeUnfold = typeUnfold.updated(name, objectType))
  }
}

object GType {

  val boolType = TyVar(Symbol("𝔹"))
  val voidType = TyVar(Symbol("void"))

  trait GTypeAPI {
    implicit def symbol2TyVar(symbol: Symbol): TyVar = TyVar(symbol)

    implicit def string2TyVar(name: String): TyVar = TyVar(Symbol(name))

    def any: AnyType.type = AnyType

    def obj(fields: (Symbol, GType)*): ObjectType = ObjectType(fields.toMap)
  }

  object API extends GTypeAPI

  /** the consistent-subtyping relation */
  def checkSubtype(child: GType, parent: GType, context: TypeContext): Option[TypeContext] = {
    import context._

    if (child == AnyType || parent == AnyType || subRel.contains(child -> parent))
      return Some(context)

    val baseTypes = context.baseTypes

    lazy val context1 = context.copy(subRel = subRel + (child -> parent))

    (child, parent) match {
      case (b1: TyVar, b2: TyVar) if baseTypes.contains(b1.id) && baseTypes.contains(b2.id) =>
        if (b1 == b2) Some(context1) else None
      case (TyVar(id), _) =>
        if (typeUnfold.contains(id)) {
          if (child == parent) Some(context1)
          else checkSubtype(typeUnfold(id), parent, context1)
        } else {
          assert(baseTypes.contains(id), s"unknown type: $id"); None
        }
      case (_, TyVar(id)) =>
        if (typeUnfold.contains(id)) {
          if (child == parent) Some(context1)
          else checkSubtype(child, typeUnfold(id), context1)
        } else {
          assert(baseTypes.contains(id), s"unknown type: $id"); None
        }
      case (FuncType(cFrom, c2), FuncType(pFrom, p2)) =>
        if (cFrom.length != pFrom.length) {
          return None // arity different
        }
        var currentContext = context1
        pFrom.zip(cFrom).foreach {
          case (p, c) =>
            checkSubtype(p, c, currentContext) match {
              case None      => return None // arg type mismatch
              case Some(ctx) => currentContext = ctx
            }
        }
        checkSubtype(c2, p2, currentContext)
      case (ObjectType(fields1), ObjectType(fields2)) =>
        var currentContext = context1
        fields2.foreach {
          case (l, lt) =>
            fields1.get(l).flatMap { lt1 =>
              checkSubtype(lt1, lt, currentContext)
            } match {
              case None    => return None // filed missing, not subtype
              case Some(c) => currentContext = c
            }
        }
        Some(currentContext)
      case _ => None
    }
  }

  // === GType random sampling ===

  val shortSymbolGen: Gen[Symbol] = {
    for (c <- Gen.alphaChar;
         suffixLen <- Gen.choose(0, 4);
         s <- Gen.listOfN(suffixLen, Gen.alphaNumChar)) yield Symbol((c +: s).mkString)
  }

  val simpleBaseTypes = List('x, 'y, 'z, 'x1, 'x2, 'int, 'number, 'string)

  val simpleGroundGen: Gen[GroundType] = {
    Gen.frequency(1 -> Gen.const(AnyType), 3 -> Gen.oneOf(simpleBaseTypes).map(API.symbol2TyVar))
  }

  val simpleNameGen: Gen[Symbol] = {
    Gen.oneOf(
      Gen.oneOf('f, 'g, 'h),
      shortSymbolGen
    )
  }

  def funcGen(size: Int)(implicit params: GTypeGenParams): Gen[FuncType] = {
    if (size < 2) {
      println(s"funcGen size: $size")
    }

    Gen.choose(1, math.min(size - 1, 4)).flatMap { argNum =>
      var s0 = size - argNum - 1
      val argGen =
        for (s1 <- Gen.choose(1, math.max(1, s0 / 2));
             t <- gTypeGen(s1)) yield {
          s0 -= s1
          t
        }

      Gen.listOfN(argNum, argGen).map { ats =>
        FuncType(ats.tail, ats.head)
      }
    }
  }

  def objGen(size: Int)(implicit params: GTypeGenParams): Gen[ObjectType] = {
    import params._
    if (size < 1) {
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
    } else if (size <= 10)
      Gen.frequency(
        1 -> groundGen,
        1 -> tyVarGen,
        3 -> funcGen(size),
        3 -> objGen(size)
      )
    else {
      Gen.frequency(
        4 -> funcGen(size),
        5 -> objGen(size),
        1 -> tyVarGen
      )
    }
  }

  case class GTypeGenParams(groundGen: Gen[GroundType], fNameGen: Gen[Symbol], tyVarGen: Gen[TyVar])

  val abcSymbols = List('A, 'B, 'C)

  def contextGen(tyVarNum: Int, objectGen: Gen[ObjectType]): Gen[TypeContext] = {
    for (tys <- Gen.listOfN(tyVarNum, objectGen);
         map = abcSymbols.zip(tys).toMap)
      yield TypeContext(simpleBaseTypes.toSet, subRel = Set(), typeUnfold = map)
  }

  val simpleGenParams = GTypeGenParams(simpleGroundGen, simpleNameGen, Gen.oneOf(abcSymbols).map {
    TyVar
  })

  val simpleGTypeGen: Gen[GType] = {
    Gen.sized { size =>
      gTypeGen(size)(simpleGenParams)
    }
  }

  /** TyVar ranges from 0 to 2 */
  val simpleContextGen: Gen[TypeContext] = {
    contextGen(3, objGen(25)(simpleGenParams))
  }

  // === End of GType random sampling ===
}
