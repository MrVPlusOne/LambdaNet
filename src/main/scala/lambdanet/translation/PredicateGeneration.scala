package lambdanet.translation

import lambdanet.{IdAllocator, ProjectPath}
import lambdanet.translation.IR._
import lambdanet.translation.PredicateGraph._
import PredicateGeneration._

import scala.collection.mutable

object PredicateGeneration {

  case class PContext(
      terms: Map[Var, PNode],
      types: Map[TypeName, PNode],
      nameSpaces: Map[Symbol, PContext]
  )

}

class PredicateGeneration extends IdAllocator[PConst] {

  def createPredicateGraph(
      modules: Map[ProjectPath, IRModule]
  ): PredicateGraph = {
    //todo: test new parsing pip line
    //todo: test imports resolution
    ???
  }

}
