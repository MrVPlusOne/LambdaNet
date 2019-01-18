package gtype

import funcdiff.SimpleMath.Extensions._

/**
  * Convert a typing environment into an type alias graph.
  */
object TypeAliasGraph {

  sealed trait TypeAliasing

  case class FuncAliasing(argTypes: List[Symbol], returnType: Symbol) extends TypeAliasing {
    override def toString: String = {
      argTypes.mkString("(", ",", ")") + "->" + returnType
    }
  }

  case class ObjectAliasing(fields: Map[Symbol, Symbol]) extends TypeAliasing {
    override def toString: String = {
      fields.map { case (f, t) => s"$f: $t" }.mkString("{", ",", "}")
    }
  }

  def typeAliasingsToContext(typeAliasings: Map[Symbol, TypeAliasing]): TypeContext = {
    import GroundType.symbolToType

    val typeUnfold = typeAliasings.mapValuesNow {
      case ObjectAliasing(fields) => ObjectType(fields.mapValuesNow(symbolToType))
      case FuncAliasing(argTypes, returnType) =>
        FuncType(argTypes.map(symbolToType), symbolToType(returnType))
    }

    TypeContext(baseTypes = Set(), typeUnfold = typeUnfold, subRel = Set())
  }

  def typeContextToAliasings(context: TypeContext): Map[Symbol, TypeAliasing] = {

    import collection.mutable
    val tConstraints = mutable.HashMap[Symbol, TypeAliasing]()
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
              ObjectAliasing(fields.mapValuesNow(f => getTypeName(f, None)))
            case FuncType(from, to) =>
              FuncAliasing(from.map { x =>
                getTypeName(x, None)
              }, getTypeName(to, None))
          }

          val newSymbol = useName.getOrElse(Symbol("$<" + tDef.toString + ">"))
          tConstraints(newSymbol) = tDef
          typeNameMap(ty) = newSymbol
          tDef.toString //todo: report this compiler bug
          newSymbol
      }
    }

    context.typeUnfold.foreach {
      case (tyName, ty) => getTypeName(ty, Some(tyName))
    }
    tConstraints.toMap
  }

  /** Count the maximum reachable path length of each type on the type graph. (Useful for determining
    * the graph propagation number needed in [[TypeEncoder]].
    * If there is a circle in the aliasing graph, the length of that circle is
    * used as the its depth. */
  def typeDepth(node: Symbol, graph: Map[Symbol, TypeAliasing]): Int = {
    import collection.mutable
    val depthMap = mutable.Map[Symbol, Int](AnyType.id -> 0)

    def rec(node: Symbol): Int = {
      depthMap.get(node).foreach{ d => return d}
      depthMap(node) = 0
      val d = graph(node) match {
        case FuncAliasing(args, ret) =>
          (args :+ ret).map(rec).max + 1
        case ObjectAliasing(fields) =>
          fields.values.map(rec).max + 1
      }
      depthMap(node) = d
      d
    }

    rec(node)
  }


  def typeAliasingsToGroundTruths(typeAliasings: Map[Symbol, TypeAliasing]) = {
    val typeContext = typeAliasingsToContext(typeAliasings)
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
