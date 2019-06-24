package lambdanet

import org.scalatest.WordSpec
import ammonite.ops._
import lambdanet.translation.OldPredicateGraphConstruction

class SymbolicTest extends WordSpec with MyTest {
  "print generated constraints" in {
    val parsed = OldPredicateGraphConstruction.fromRootDirectory(
      pwd / RelPath("data/tests/symbolic/ex1"),
    )

    parsed.predModules.zip(parsed.irModules).foreach {
      case (pm, im) =>
        println(pm.display(Some(im)))
    }
  }

}
