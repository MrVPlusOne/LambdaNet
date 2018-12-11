package gtype

/**
  * Turn type definitions into a constraint graph over literals
  */
object TypeConstraintGraph {

  sealed trait TypeRewrite

  case class FuncRewrite(argTypes: List[Symbol], returnType: Symbol) extends TypeRewrite {
    override def toString: String = {
      argTypes.mkString("(", ",", ")") + "->" + returnType
    }
  }

  case class ObjectRewrite(fields: Map[Symbol, Symbol]) extends TypeRewrite {
    override def toString: String = {
      fields.map { case (f, t) => s"$f: $t" }.mkString("{", ",", "}")
    }
  }

  def typeRewritesToContext(typeRewrites: Map[Symbol, TypeRewrite]): TypeContext = {
    import GroundType.symbolToType

    val typeUnfold = typeRewrites.mapValues {
      case ObjectRewrite(fields) => ObjectType(fields.mapValues(symbolToType))
      case FuncRewrite(argTypes, returnType) =>
        FuncType(argTypes.map(symbolToType), symbolToType(returnType))
    }

    TypeContext(baseTypes = Set(), typeUnfold = typeUnfold, subRel = Set())
  }

  def typeContextToRewrites(context: TypeContext): Map[Symbol, TypeRewrite] = {

    import collection.mutable
    val tConstraints = mutable.HashMap[Symbol, TypeRewrite]()
    val typeNameMap = mutable.HashMap[CompoundType, Symbol]()

    def getTypeName(ty: GType, useName: Option[Symbol]): Symbol = {
      ty match {
        case g: GroundType => g.id
        case ty: CompoundType =>
          if (useName.isEmpty && typeNameMap.contains(ty)) {
            return typeNameMap(ty)
          }

          val tDef = ty match {
            case ObjectType(fields) =>
              ObjectRewrite(fields.mapValues(f => getTypeName(f, None)))
            case FuncType(from, to) =>
              FuncRewrite(from.map { x =>
                getTypeName(x, None)
              }, getTypeName(to, None))
          }

          val newSymbol = useName.getOrElse(Symbol("$<" + tDef.toString + ">"))
          tConstraints(newSymbol) = tDef
          typeNameMap(ty) = newSymbol
          println(s"update: $newSymbol: $tDef")
          newSymbol
      }
    }

    context.typeUnfold.foreach {
      case (tyName, ty) => getTypeName(ty, Some(tyName))
    }
    tConstraints.toMap
  }

  def typeRewritesToGroundTruths(typeRewrites: Map[Symbol, TypeRewrite]) = {
    val typeContext = typeRewritesToContext(typeRewrites)
    val types = typeContext.typeUnfold.keys.toList
    import GroundType.symbolToType

    val groundTruths = for (t1 <- types; t2 <- types if t1 != t2)
      yield {
        (t1, t2) -> typeContext.isSubtype(symbolToType(t1), symbolToType(t2))
      }

    val reflexivity = types.map(t => (t, t))
    val posExamples = groundTruths.filter(_._2).map(_._1)
    val negExamples = groundTruths.filterNot(_._2).map(_._1)
    (reflexivity, posExamples, negExamples)
  }

  object Examples {

    import GType.API._

    import JSExamples._

    val basicTypes: Map[Symbol, CompoundType] = Map(
      boolean -> obj('BoolPlaceholder -> boolean),
      void -> obj('VoidPlaceholder -> void),
      number -> obj(
        'OP_Plus -> (List(number) -: number),
        'OP_Minus -> (List(number) -: number),
        'OP_Times -> (List(number) -: number),
        'OP_Divide -> (List(number) -: number),
        'OP_LessThan -> (List(number) -: boolean),
      ),
      string -> obj(
        'OP_Plus -> (List(any) -: string),
        'charAt -> (List(number) -: string),
        'length -> number
      ),
      'Comparator -> obj('equal -> (List(any, any) -: boolean))
    ) ++ Seq[GroundType](boolean, number, string, any).map(mkArrayType).toMap

    val pointExample = {
      val typeUnfold = basicTypes ++
        Map[Symbol, CompoundType](
          'point -> obj('x -> 'number, 'moveX -> (List('number) -: 'point)),
          'point2D -> obj(
            'x -> 'number,
            'y -> 'number,
            'moveX -> (List('number) -: 'point2D),
            'moveY -> (List('number) -: 'point2D)
          ),
          'stringPoint -> obj(
            'x -> 'string,
            'moveX -> (List('number) -: 'stringPoint)
          ),
          'A -> obj('a -> 'B),
          'B -> obj('a -> 'A),
          'C -> obj('c -> 'A)
        )
      TypeContext(Set(), typeUnfold, Set())
    }
  }

}
