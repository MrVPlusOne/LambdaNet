package gtype

import funcdiff.SimpleMath
import gtype.TypeAliasGraph.{FuncAliasing, ObjectAliasing, TypeAliasing, typeContextToAliasings}

import scala.util.Random
import collection.mutable

object TrainingTypeGeneration {

  class TrainingData() {
    val typeAliasings: mutable.HashMap[Symbol, TypeAliasing] = mutable.HashMap()
    val childMap: mutable.HashMap[Symbol, IndexedHashSet[Symbol]] = mutable.HashMap()
    val parentMap: mutable.HashMap[Symbol, IndexedHashSet[Symbol]] = mutable.HashMap()
    val fieldSet: IndexedHashSet[Symbol] = IndexedHashSet()
    val objectSet: IndexedHashSet[Symbol] = IndexedHashSet()
    val funcSet: IndexedHashSet[Symbol] = IndexedHashSet()
    var newFiledIndex: Int = 0
    var newTypeIndex: Int = 0

    def newField(): Symbol = {
      val s = Symbol(s"@field$newFiledIndex")
      newFiledIndex += 1
      fieldSet += s
      s
    }

    def addFiled(s: Symbol): Unit = {
      if (!s.name.endsWith("_UNIQUE"))
        fieldSet += s
    }

    def addType(s: Symbol): Unit = {
      childMap(s) = IndexedHashSet[Symbol]()
      parentMap(s) = IndexedHashSet[Symbol]()
    }

    def newType(isObject: Boolean): Symbol = {
      val prefix = if (isObject) "@Obj" else "@Func"
      val s = Symbol(s"$prefix$newTypeIndex")
      newTypeIndex += 1
      addType(s)
      s
    }

    def addTypeRewrite(ty: Symbol, tR: TypeAliasing): Unit = {
      require(!typeAliasings.contains(ty))

      tR match {
        case _: FuncAliasing => funcSet += ty
        case oR: ObjectAliasing =>
          objectSet += ty
          oR.fields.keys.foreach(addFiled)
      }
      typeAliasings(ty) = tR
    }

    def addSubtypeRel(child: Symbol, parent: Symbol): Unit = {
      childMap(parent) += child
      parentMap(child) += parent
    }

    def subtypeRelations: Set[(Symbol, Symbol)] = {
      var r = Set[(Symbol, Symbol)]()
      for ((c, parents) <- parentMap;
           p <- parents.elements) {
        r += (c -> p)
      }
      r
    }

    case class GenerationFailed(msg: String) extends Exception

    @throws[GenerationFailed]
    def generateMore()(implicit random: Random): Unit = {
      val re = new RandomExtension(random)
      import re._

      /**
        * @return (mutateUp?, mutateTo)
        */
      @throws[GenerationFailed]
      def mutateUpOrDown(ty: Symbol): (Boolean, Symbol) = {
        if (ty == AnyType.id) {
          return if (prob(0.85)) (prob(0.5), objectSet.sample())
          else (prob(0.5), funcSet.sample())
        }

        val options = childMap(ty).elements.map(c => (false, c)) ++
          parentMap(ty).elements.map(c => (true, c))
        if (options.isEmpty)
          throw GenerationFailed("mutateUpOrDown has no options")
        else
          SimpleMath.randomSelect(options)
      }

      if (prob(0.75)) {
        // extend or delete field
        val baseTy = objectSet.sample()
        val objRewrite = typeAliasings(baseTy).asInstanceOf[ObjectAliasing]
        val newTy = newType(isObject = true)
        if (objRewrite.fields.size >= 2 && prob(0.4)) {
          val fields = objRewrite.fields
          val k = SimpleMath.randomSelect(fields.keys.toIndexedSeq)
          // delete some field
          val newFields = fields - k
          addTypeRewrite(newTy, ObjectAliasing(newFields))
          addSubtypeRel(baseTy, newTy)
        } else {
          val field = if (prob(0.6)) newField() else fieldSet.sample()
          val fieldTy = if (prob(0.85)) {
            val f = funcSet.sample()
            if (prob(0.2)) {
              // generate recursive method type
              val oldAliasing = typeAliasings(f).asInstanceOf[FuncAliasing]
              val aliasing = if (oldAliasing.argTypes.nonEmpty && prob(0.5)) {
                val i = random.nextInt(oldAliasing.argTypes.length)
                oldAliasing.copy(argTypes = oldAliasing.argTypes.updated(i, newTy))
              } else {
                oldAliasing.copy(returnType = newTy)
              }
              val newFTy = newType(isObject = false)
              addTypeRewrite(newFTy, aliasing)
              newFTy
            } else {
              f
            }
          } else {
            if (prob(0.2)) newTy // recursive field type
            else objectSet.sample()
          }
          val baseFields = objRewrite.fields
          val newFields = baseFields.updated(field, fieldTy)
          addTypeRewrite(newTy, ObjectAliasing(newFields))
          addSubtypeRel(newTy, baseTy)
        }
      } else {
        // mutate a function
        val baseF = funcSet.sample()
        val rewrite = typeAliasings(baseF).asInstanceOf[FuncAliasing]
        if (prob(0.6) && rewrite.argTypes.nonEmpty) {
          // mutate arg type
          val argId = random.nextInt(rewrite.argTypes.length)
          val (goUp, newArgT) = mutateUpOrDown(rewrite.argTypes(argId))
          val newRewrite = rewrite.copy(argTypes = rewrite.argTypes.updated(argId, newArgT))
          val newTy = newType(isObject = false)
          addTypeRewrite(newTy, newRewrite)
          if (!goUp) addSubtypeRel(baseF, newTy) //contravariant
          else addSubtypeRel(newTy, baseF)
        } else {
          // mutate return type
          val (goUp, newReturn) = mutateUpOrDown(rewrite.returnType)
          val newRewrite = rewrite.copy(returnType = newReturn)
          val newTy = newType(isObject = false)
          addTypeRewrite(newTy, newRewrite)
          if (goUp) addSubtypeRel(baseF, newTy) //covariant
          else addSubtypeRel(newTy, baseF)
        }
      }
    }

    def tryUntilGenerate(chancesLeft: Int)(implicit random: Random): Unit = {
      if (chancesLeft < 0) {
        throw GenerationFailed("Maximum attempts hit")
      }

      try {
        generateMore()
      } catch {
        case _: GenerationFailed =>
          tryUntilGenerate(chancesLeft - 1)
      }
    }
  }

  object TrainingData {
    def fromTypeContext(typeContext: TypeContext): TrainingData = {
      val typeRewrites = TypeAliasGraph.typeContextToAliasings(typeContext)
      val (_, posExamples, _) = TypeAliasGraph.typeAliasingsToGroundTruths(typeRewrites)

      val data = new TrainingData()
      typeRewrites.foreach {
        case (s, tR) =>
          data.addType(s)
          data.addTypeRewrite(s, tR)
      }
      posExamples.foreach {
        case (c, p) => data.addSubtypeRel(c, p)
      }

      data
    }
  }

  def augmentWithRandomTypes(context: TypeContext, newTypeNum: Int)
                            (implicit random: Random): TypeContext = {

    val data = TrainingData.fromTypeContext(context)

    for (_ <- 0 until newTypeNum) {
      data.tryUntilGenerate(10)
    }

//    println(s"==== subtype relations (amount: ${data.subtypeRelations.size}) ====")

    val aliasings = data.typeAliasings.toMap
    val (_, posRels, _) = TypeAliasGraph.typeAliasingsToGroundTruths(aliasings)
//    println(s"==== true subtype relation amount: ${posRels.size} ====")

    TypeAliasGraph.typeAliasingsToContext(aliasings)
  }

  import GType.API._
  val trainingContext = TypeContext(
    baseTypes = Set(),
    typeUnfold = Map(
      'A -> obj('A_UNIQUE -> 'A),
      'void -> obj('void_UNIQUE -> 'void),
      'B -> obj(
        'B_UNIQUE -> 'B,
        'BMethod -> (List('B) -: 'B)
      ),
      'C -> obj(
        'common1 -> (List('C) -: 'C),
        'rare1 -> (List('C) -: 'C),
        'rare2 -> (List('C, 'C) -: 'A),
        'rare3 -> (List('C, 'C) -: 'B),
        'common2 -> (List() -: 'B),
      ),
      'D -> obj(
        'common1 -> TyVar('A),
        'common4 -> (List() -: 'B),
      ),
      'E -> obj(
        'common1 -> any,
        'common2 -> 'F1,
        'common3 -> (List('C, 'C) -: 'B),
        'common4 -> (List() -: 'B),
        'common5 -> any,
        'common6 -> 'F3
      ),
      'F0 -> (List() -: 'void),
      'F1 -> (List(any) -: 'E),
      'F2 -> (List('A, 'B) -: 'C),
      'F3 -> (List(any, any, any) -: any),
      'F4 -> (List(any, any, any, any) -: any),
    ),
    subRel = Set()
  )

  def main(args: Array[String]): Unit = {
//    val context = augmentWithRandomTypes(JSExamples.trainingTypeContext, 1)

    for(i <- 0 until 100) {
      println(s"step: $i")
      val context = augmentWithRandomTypes(trainingContext, 200)(new Random())

      val aliasings = typeContextToAliasings(context)
      aliasings.foreach(println)
      val depths = aliasings.keys.map {
        TypeAliasGraph.typeDepth(_, aliasings)
      }.toVector
      val avgDepth = depths.sum / depths.length
      val maxDepth = depths.max
      val minDepth = depths.min
      println(s"avgDepth = $avgDepth")
      println(s"maxDepth = $maxDepth")
      println(s"minDepth = $minDepth")
    }
  }
}

class RandomExtension(random: Random) {
  def prob(p: Double): Boolean = {
    random.nextDouble() < p
  }

}
