package gtype

import funcdiff.SimpleMath
import gtype.TypeConstraintGraph.{FuncRewrite, ObjectRewrite, TypeRewrite}

import scala.util.Random
import collection.mutable

object TrainingTypeGeneration {

  class TrainingData() {
    val typeRewrites: mutable.HashMap[Symbol, TypeRewrite] = mutable.HashMap()
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
      fieldSet += s
    }

    def addType(s: Symbol): Unit = {
      childMap(s) = IndexedHashSet[Symbol]()
      parentMap(s) = IndexedHashSet[Symbol]()
    }

    def newType(): Symbol = {
      val s = Symbol(s"@Type$newTypeIndex")
      newTypeIndex += 1
      addType(s)
      s
    }

    def addTypeRewrite(ty: Symbol, tR: TypeRewrite): Unit = {
      require(!typeRewrites.contains(ty))

      tR match {
        case _: FuncRewrite => funcSet += ty
        case oR: ObjectRewrite =>
          objectSet += ty
          oR.fields.keys.foreach(addFiled)
      }
      typeRewrites(ty) = tR
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
          val r =
            if (prob(0.85)) (prob(0.5), objectSet.sample())
            else (prob(0.5), funcSet.sample())
          return r
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
        val objRewrite = typeRewrites(baseTy).asInstanceOf[ObjectRewrite]
        val newTy = newType()
        if (objRewrite.fields.size >= 2 && prob(0.4)) {
          val fields = objRewrite.fields
          val k = SimpleMath.randomSelect(fields.keys.toIndexedSeq)
          // delete some field
          val newFields = fields - k
          addTypeRewrite(newTy, ObjectRewrite(newFields))
          addSubtypeRel(baseTy, newTy)
        } else {
          val field = if (prob(0.6)) newField() else fieldSet.sample()
          val fieldTy = if (prob(0.85)) funcSet.sample() else objectSet.sample() //todo: generate recursive types
          val baseFields = objRewrite.fields
          val newFields = baseFields.updated(field, fieldTy)
          addTypeRewrite(newTy, ObjectRewrite(newFields))
          addSubtypeRel(newTy, baseTy)
        }
      } else {
        // mutate a function
        val baseF = funcSet.sample()
        val rewrite = typeRewrites(baseF).asInstanceOf[FuncRewrite]
        if (prob(0.5) && rewrite.argTypes.nonEmpty) {
          // mutate arg type
          val argId = random.nextInt(rewrite.argTypes.length)
          val (goUp, newArgT) = mutateUpOrDown(rewrite.argTypes(argId))
          val newRewrite = rewrite.copy(argTypes = rewrite.argTypes.updated(argId, newArgT))
          val newTy = newType()
          addTypeRewrite(newTy, newRewrite)
          if (!goUp) addSubtypeRel(baseF, newTy) //contravariant
          else addSubtypeRel(newTy, baseF)
        } else {
          // mutate return type
          val (goUp, newReturn) = mutateUpOrDown(rewrite.returnType)
          val newRewrite = rewrite.copy(returnType = newReturn)
          val newTy = newType()
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
      val typeRewrites = TypeConstraintGraph.typeContextToRewrites(typeContext)
      val (_, posExamples, _) = TypeConstraintGraph.typeRewritesToGroundTruths(typeRewrites)

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

  def augmentWithRandomTypes(context: TypeContext): TypeContext = {
    implicit val random: Random = new Random()

    val data = TrainingData.fromTypeContext(context)

    for (_ <- 0 until 200) {
      data.tryUntilGenerate(10)
    }

    data.typeRewrites.foreach(println)
    println(s"==== subtype relations (amount: ${data.subtypeRelations.size}) ====")
    data.subtypeRelations.foreach(println)

    val rewrites = data.typeRewrites.toMap
    val (_, posRels, _) = TypeConstraintGraph.typeRewritesToGroundTruths(rewrites)
    println(s"true subtype relation amount: ${posRels.size}")

    TypeConstraintGraph.typeRewritesToContext(rewrites)
  }
}

class RandomExtension(random: Random) {
  def prob(p: Double): Boolean = {
    random.nextDouble() < p
  }

}
