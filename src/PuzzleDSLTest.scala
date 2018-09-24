import junit.framework.TestCase
import org.junit.Assert._

/**
  * Unit tests for PuzzleDSL methods
  */
case class PuzzleDSLTest(name: String) extends TestCase(name) {

  def testLiteralNegated(): Unit = {
    val x = Sym(First, Blue)
    val xf = Sym(First, Blue).negated

    // x.negated == not x
    assertTrue("x.negated == not x", xf == Not(x))

    // x.negated.negated = x
    assertTrue("x.negated.negated = x", xf.negated == x)
  }

  def testLiteralTruth(): Unit = {
    val x = Sym(First, Blue)
    val xf = Not(x)

    // x is true
    assertTrue(x.truth == true)

    // xf is false
    assertTrue(xf.truth == false)
  }

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

    // False ==> False
    val ff = x.negated and y.negated and (x implies y)
    val ffMaybeSolution = PuzzleSolver.findSolutionAssignments(ff)
    assertTrue("False ==> False is valid", ffMaybeSolution.nonEmpty)

    // False ==> True
    val ft = x.negated and y and (x implies y)
    val ftMaybeSolution = PuzzleSolver.findSolutionAssignments(ft)
    assertTrue("False ==> True is valid", ftMaybeSolution.nonEmpty)
  }

  def testLiteralIff(): Unit = {
    val x = Sym(First, Blue)
    val y = Sym(First, Texas)

    // True <==> True
    val tt = x and y and (x iff y)
    val ttMaybeSolution = PuzzleSolver.findSolutionAssignments(tt)
    assertTrue("True <==> True is valid", ttMaybeSolution.nonEmpty)

    // True <=/=> False
    val tf = x and y.negated and (x iff y)
    val tfMaybeSolution = PuzzleSolver.findSolutionAssignments(tf)
    assertTrue("True <==> False is contradiction", tfMaybeSolution.isEmpty)

    // False <=/=> True
    val ft = x.negated and y and (x iff y)
    val ftMaybeSolution = PuzzleSolver.findSolutionAssignments(ft)
    assertTrue("False <==> True is contradiction", ftMaybeSolution.isEmpty)

    // False <==> False
    val ff = x.negated and y.negated and (x iff y)
    val ffMaybeSolution = PuzzleSolver.findSolutionAssignments(ff)
    assertTrue("False <==> False is valid", ffMaybeSolution.nonEmpty)

    // x <==> True
    val xt = y and (x iff y)
    val xtMaybeSolution = PuzzleSolver.findSolutionAssignments(xt)
    assertTrue("x <==> True is valid", xtMaybeSolution.nonEmpty)
    val xtSolution = xtMaybeSolution.get
    assertTrue("x <==> True forces x to true", xtSolution contains (x -> true))

    // x <==> False
    val xf = y.negated and (x iff y)
    val xfMaybeSolution = PuzzleSolver.findSolutionAssignments(xf)
    assertTrue("x <==> False is valid", xfMaybeSolution.nonEmpty)
    val xfSolution = xfMaybeSolution.get
    assertTrue("x <==> False forces x to false", xfSolution contains (x -> false))

    // True <==> y
    val yt = x and (x iff y)
    val ytMaybeSolution = PuzzleSolver.findSolutionAssignments(yt)
    assertTrue("True <==> y is valid", ytMaybeSolution.nonEmpty)
    val ytSolution = ytMaybeSolution.get
    assertTrue("True <==> y forces y to true", ytSolution contains (y -> true))

    // False <==> y
    val yf = x.negated and (x iff y)
    val yfMaybeSolution = PuzzleSolver.findSolutionAssignments(yf)
    assertTrue("False <==> y is valid", yfMaybeSolution.nonEmpty)
    val yfSolution = yfMaybeSolution.get
    assertTrue("False <==> y forces y to false", yfSolution contains (x -> false))
  }

  def testAttrImpliesAt(): Unit = {

  }

  def testAttrImplies(): Unit = {

  }

  def testAttrIff(): Unit = {

  }

  // TODO: Write tests for the other methods you implement in PuzzleDSL.scala
  def testJustBehind(): Unit = {
    val yJustBehindX = Arizona.justBehind(California)
    val xAtFirst = Sym(First, California)
    val yAtSecond = Sym(Second, Arizona)
    val cnf = yJustBehindX and xAtFirst
    val solution = PuzzleSolver.findSolutionAssignments(cnf).get
    assertTrue("California is at first", solution contains(xAtFirst -> true))
    assertTrue("Arizona is just behind California", solution contains(yAtSecond -> true))
  }

  def testBehind(): Unit = {
    val yBehindX = Arizona.behind(California)
    val xAtFirst = Sym(First, California)
    val yAtSecond = Sym(Second, Arizona)
    val yAtThird = Sym(Third, Arizona)
    val yAtFourth = Sym(Fourth, Arizona)
    val yAtFifth = Sym(Fifth, Arizona)
    val cnf1 = yBehindX and xAtFirst
    val solution = PuzzleSolver.findSolutionAssignments(cnf1).get
    assertTrue("California is at first", solution contains(xAtFirst -> true))
    assertTrue("Arizona is behind California", (solution contains(yAtSecond -> true)) ||
      (solution contains(yAtThird -> true)) || (solution contains(yAtFourth -> true)) || (solution contains(yAtFifth -> true)))
  }
}
