import junit.framework.TestCase
import org.junit.Assert._

/**
  * Unit tests for PuzzleConstraints methods
  */
case class PuzzleConstraintsTest(name: String) extends TestCase(name) {

    def testSolution(): Unit = {
        val solution = PuzzleConstraints.solution
        val expected = Some(Solution(
            PositionAttributes(Ava, Red, Honda, Lawyer, Louisiana, Y2018),
            PositionAttributes(Emma, White, Jeep, Engineer, Texas, Y2010),
            PositionAttributes(Isabel, Green, Ford, Doctor, NewMexico, Y2015),
            PositionAttributes(Sophie, Blue, Toyota, Nurse, Arizona, Y2001),
            PositionAttributes(Olivia, Black, Kia, Biologist, California, Y2005)))
        assertEquals("Solution equals to the expected", expected, solution)
    }

    def testTexasCarPosition(): Unit = {
        val solution = PuzzleConstraints.solution.get
        val actual = PuzzleConstraints.texasCarPosition(solution)
        val expected = Second
        assertEquals("Texas car is at second", expected, actual)
    }
}
