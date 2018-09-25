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
    val x = Sym(First, Blue)
    val y = Sym(First, Texas)

    // True ==> True
    val tt = x and y and Blue.impliesAt(First, Texas)
    val ttMaybeSolution = PuzzleSolver.findSolutionAssignments(tt)
    assertTrue("True impliesAt True is valid", ttMaybeSolution.nonEmpty)

    // True ==> y
    val ty = x and Blue.impliesAt(First, Texas)
    val tyMaybeSolution = PuzzleSolver.findSolutionAssignments(ty)
    assertTrue("True impliesAt y is valid", tyMaybeSolution.nonEmpty)
    val tySolution = tyMaybeSolution.get
    assertTrue("True impliesAt y forces y to true", tySolution contains (y->true))
  }

  def testAttrImplies(): Unit = {
    val blackFirst = Sym(First, Black)
    val texasFirst = Sym(First, Texas)

    // Black implies Texas and Black is at First
    val cnf1 = (Black implies Texas) and blackFirst
    val cnf1MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf1)
    assertTrue("CNF1 is valid", cnf1MaybeSolution.nonEmpty)
    val cnf1Solution = cnf1MaybeSolution.get
    assertTrue("Texas is at First", cnf1Solution contains (texasFirst->true))

    // Black is at First, Texas is not at First, Black implies Texas
    val cnf2 = (Black implies Texas) and blackFirst and texasFirst.negated
    val cnf2MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf2)
    assertTrue("Black implies Texas is not valid", cnf2MaybeSolution.isEmpty)
  }

  def testAttrIff(): Unit = {
    val blackFirst = Sym(First, Black)
    val texasFirst = Sym(First, Texas)

    // Black Iff Texas and Black is at First
    val cnf1 = (Black iff Texas) and blackFirst
    val cnf1MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf1)
    assertTrue("CNF1 is valid", cnf1MaybeSolution.nonEmpty)
    val cnf1Solution = cnf1MaybeSolution.get
    assertTrue("Texas is at First", cnf1Solution contains (texasFirst->true))

    // Black Iff Texas and Black is not at First
    val cnf2 = (Black iff Texas) and blackFirst.negated
    val cnf2MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf2)
    assertTrue("CNF2 is valid", cnf2MaybeSolution.nonEmpty)
    val cnf2Solution = cnf2MaybeSolution.get
    assertTrue("Texas is not at First", cnf2Solution contains texasFirst->false)

    // Black Iff Texas and Texas is at First
    val cnf3 = (Black iff Texas) and texasFirst
    val cnf3MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf3)
    assertTrue("CNF3 is valid", cnf3MaybeSolution.nonEmpty)
    val cnf3Solution = cnf3MaybeSolution.get
    assertTrue("Black is not at First", cnf3Solution contains blackFirst->true)

    // Black Iff Texas and Texas is not at First
    val cnf4 = (Black iff Texas) and texasFirst.negated
    val cnf4MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf4)
    assertTrue("CNF4 is valid", cnf4MaybeSolution.nonEmpty)
    val cnf4Solution = cnf4MaybeSolution.get
    assertTrue("Texas is not at First", cnf4Solution contains blackFirst->false)
  }

  def testJustBehind(): Unit = {
    val yJustBehindX = Arizona justBehind California
    val xAtSecond = Sym(Second, California)
    val yAtThird = Sym(Third, Arizona)

    // X is not at Second and Y is just behind X
    val cnf1 = xAtSecond.negated and yJustBehindX
    val solution1 = PuzzleSolver.findSolutionAssignments(cnf1).get
    assertTrue("Y is not at Third", solution1 contains (yAtThird -> false))

    // Y is not at Third and Y is just behind X
    val cnf2 = yJustBehindX and yAtThird.negated
    val solution2 = PuzzleSolver.findSolutionAssignments(cnf2).get
    assertTrue("X is not at Second", solution2 contains (xAtSecond -> false))
  }

  def testBehind(): Unit = {
    val yBehindX = Arizona behind California
    val xAtSecond = Sym(Second, California)
    val xAtFirst = Sym(First, California)
    val xAtThird = Sym(Third, California)
    val yAtThird = Sym(Third, Arizona)
    val yAtFourth = Sym(Fourth, Arizona)
    val yAtFifth = Sym(Fifth, Arizona)

    // X is at Second, Y behind X
    val cnf1 = yBehindX and xAtSecond
    val cnf1MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf1)
    assertTrue("CNF1 is valid", cnf1MaybeSolution.nonEmpty)
    val cnf1Solution = cnf1MaybeSolution.get
    assertTrue("Y is at Third", cnf1Solution contains yAtThird->true)
    assertTrue("Y is at Fourth", cnf1Solution contains yAtFourth->true)
    assertTrue("Y is at Fifth", cnf1Solution contains yAtFifth->true)

    // Y is at fourth, Y behind X
    val cnf2 = yBehindX and yAtFourth
    val cnf2MaybeSolution = PuzzleSolver.findSolutionAssignments(cnf2)
    assertTrue("CNF2 is valid", cnf2MaybeSolution.nonEmpty)
    val cnf2Solution = cnf2MaybeSolution.get
    assertTrue("X is at First", cnf2Solution contains xAtFirst->true)
    assertTrue("X is at Second", cnf2Solution contains xAtSecond->true)
    assertTrue("X is at Third", cnf2Solution contains xAtThird->true)
  }
}
