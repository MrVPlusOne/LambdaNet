package lambdanet.correctness

import ammonite.{ops => amm}
import lambdanet.correctness.CrossEntropyTypeInference.AssignmentGen
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph.PAny
import lambdanet.translation.PredicateGraphLoader.libDefs

object TypeCheckingScript {
  def main(args: Array[String]): Unit = {
    val inputPath = amm.pwd / "data" / "tests" / "public"
    val (graph, nodeAnnots, results) = InputUtils.loadGraphAndPredict(inputPath)
    val checker = TypeChecker(graph, libDefs)
    val allNodes = AllNodes(graph, libDefs, results)
    import allNodes._
    val plane = projectNodes(133)
    val thing = projectNodes(1)
    val sphere = projectNodes(103)
    val relevantNodes = Set(plane, thing, sphere)
    relevantNodes.map(checker.typeAndNodeBounds(_)).foreach(println)
    val assignment = Map(
       plane -> projectTypes(133),
       thing -> libraryTypes(8642) // 𝓛[ty]8642{Object}
    ).withDefaultValue(PAny)
    val allPTypes: Set[PredicateGraph.PType] = results.flatMap(_._2.distr.map(_._2))(collection.breakOut)
    val shallowSubtype = ShallowSubtype(checker, allPTypes)
    shallowSubtype.typeAndNodeBounds(thing).foreach(println)
    assert(!checker.defaultContext.isSubtype(plane, thing, assignment))
    val sameNodes = Set(plane, thing, sphere).map(Set(_))
    val validTypes =
      Heuristics.validTypesWithAnyAssignment(results, sameNodes, checker)
    val initialState = BasicInferenceState(
      shallowSubtype,
      sameNodes,
      validTypes,
      Map.empty
    )
    val assignmentGen =
      AssignmentGen(
        initialState
      )
    assignmentGen(results, 1)
  }
}
