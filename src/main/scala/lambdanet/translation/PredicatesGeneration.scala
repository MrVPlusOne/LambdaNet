package lambdanet.translation

import lambdanet.{IdAllocator, ProjectPath}
import lambdanet.translation.IR._
import lambdanet.translation.PredicateGraph._
import PredicatesGeneration._
import lambdanet.surface.GModule
import lambdanet.translation.ImportsResolution.ModuleExports
import lambdanet.translation.PredicateGraph.PVar.PVarAllocator

import scala.collection.mutable

object PredicatesGeneration {

  case class PContext(
      terms: Map[Var, PNode],
      types: Map[TypeName, PNode],
      nameSpaces: Map[Symbol, PContext]
  )

}

class PredicatesGeneration {

  def createPredicateGraph(
      modules: Map[ProjectPath, GModule],
      libraryModules: Map[ProjectPath, ModuleExports],
      pathMapping: ImportsResolution.PathMapping
  ): PredicateGraph = {
    val pVarAllocator = new PVarAllocator()
    val importedContexts =
      ImportsResolution.resolveImports(
        Left(pVarAllocator),
        modules,
        libraryModules,
        pathMapping
      )

    var libUsage: Map[PConst, Int] = Map()

    ???
  }

}
