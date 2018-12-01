package gtype

import org.scalacheck.Gen

object MiniLang {
  val TInt = BaseType('I)
  val TFloat = BaseType('F)
  val TBool = BaseType('B)

  val allBases: Seq[BaseType] = Seq(TInt, TFloat, TBool)

  def subRel(b1: BaseType, b2: BaseType): Boolean = {
    b1 == TInt && b2 == TFloat
  }

  def miniGen(anyRatio: Double): (Gen[GType], Gen[TypeContext]) = {
    val groundGen: Gen[GroundType] = for (x <- Gen.choose(0.0, 1.0);
                                          t <- if (x < anyRatio) Gen.const(AnyType) else Gen.oneOf(allBases)) yield t
    val tyVarGen = Gen.choose(0, 2).map {
      TyVar
    }
    val params = GType.GTypeGenParams(groundGen, GType.simpleNameGen, tyVarGen)


    Gen.sized { size => GType.gTypeGen(size)(params) } -> GType.contextGen(3, GType.objGen(25)(params))
  }
}
