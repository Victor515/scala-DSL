import junit.framework.TestCase
import org.junit.Assert._

/**
  * Unit tests for PuzzleDSL methods
  */
case class PuzzleDSLTest(name: String) extends TestCase(name) {

  def testLiteralImplies(): Unit = {
    // This test is provided as an example of how to write your other tests.
    // Build a CNF statement, solve it using PuzzleSolver.findSolutionAssignments,
    // and check for the expected results.
    // You can use the Array.contains predicate to check for specific assignments.
    // The -> operator is a convenient Scala shorthand for constructing pairs;
    // i.e., (1, 2) is the same as 1->2.

    // Define a couple of symbols to use in our tests
    val x = Sym(First, Blue)
    val y = Sym(First, Texas)

    // True ==> True
    val tt = x and y and (x implies y)
    val ttMaybeSolution = PuzzleSolver.findSolutionAssignments(tt)
    assertTrue("True ==> True is valid", ttMaybeSolution.nonEmpty)

    // True ==> Y
    val ty = x and (x implies y)
    val tyMaybeSolution = PuzzleSolver.findSolutionAssignments(ty)
    assertTrue("True ==> Y is valid", tyMaybeSolution.nonEmpty)
    val tySolution = tyMaybeSolution.get
    assertTrue("Implication forces Y to true", tySolution contains (y -> true))

    // True =/=> False
    val tf = x and y.negated and (x implies y)
    val tfMaybeSolution = PuzzleSolver.findSolutionAssignments(tf)
    assertTrue("True ==> False is a contradiction", tfMaybeSolution.isEmpty)

    // TODO: add additional test cases

  }

  // TODO: Write tests for the other methods you implement in PuzzleDSL.scala

}
