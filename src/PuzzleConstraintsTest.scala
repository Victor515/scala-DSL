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
        // Texas car is at Second
        val solution1 = Solution(
            PositionAttributes(Ava, Red, Honda, Lawyer, Louisiana, Y2018),
            PositionAttributes(Emma, White, Jeep, Engineer, Texas, Y2010),
            PositionAttributes(Isabel, Green, Ford, Doctor, NewMexico, Y2015),
            PositionAttributes(Sophie, Blue, Toyota, Nurse, Arizona, Y2001),
            PositionAttributes(Olivia, Black, Kia, Biologist, California, Y2005))
        val actual1 = PuzzleConstraints.texasCarPosition(solution1)
        val expected1 = Second
        assertEquals("Texas car is at second", expected1, actual1)


        // Texas car is at First
        val solution2 = Solution(
            PositionAttributes(Ava, Red, Honda, Lawyer, Texas, Y2018),
            PositionAttributes(Emma, White, Jeep, Engineer, Louisiana, Y2010),
            PositionAttributes(Isabel, Green, Ford, Doctor, NewMexico, Y2015),
            PositionAttributes(Sophie, Blue, Toyota, Nurse, Arizona, Y2001),
            PositionAttributes(Olivia, Black, Kia, Biologist, California, Y2005))
        val actual2 = PuzzleConstraints.texasCarPosition(solution2)
        val expected2 = First
        assertEquals("Texas car is at first", expected2, actual2)

        // Texas car is at Third
        val solution3 = Solution(
            PositionAttributes(Ava, Red, Honda, Lawyer, Louisiana, Y2018),
            PositionAttributes(Emma, White, Jeep, Engineer, NewMexico, Y2010),
            PositionAttributes(Isabel, Green, Ford, Doctor, Texas, Y2015),
            PositionAttributes(Sophie, Blue, Toyota, Nurse, Arizona, Y2001),
            PositionAttributes(Olivia, Black, Kia, Biologist, California, Y2005))
        val actual3 = PuzzleConstraints.texasCarPosition(solution3)
        val expected3 = Third
        assertEquals("Texas car is at third", expected3, actual3)

        // Texas car is at Fourth
        val solution4 = Solution(
            PositionAttributes(Ava, Red, Honda, Lawyer, Louisiana, Y2018),
            PositionAttributes(Emma, White, Jeep, Engineer, Arizona, Y2010),
            PositionAttributes(Isabel, Green, Ford, Doctor, NewMexico, Y2015),
            PositionAttributes(Sophie, Blue, Toyota, Nurse, Texas, Y2001),
            PositionAttributes(Olivia, Black, Kia, Biologist, California, Y2005))
        val actual4 = PuzzleConstraints.texasCarPosition(solution4)
        val expected4 = Fourth
        assertEquals("Texas car is at fourth", expected4, actual4)

        // Texas car is at Fifth
        val solution5 = Solution(
            PositionAttributes(Ava, Red, Honda, Lawyer, Louisiana, Y2018),
            PositionAttributes(Emma, White, Jeep, Engineer, California, Y2010),
            PositionAttributes(Isabel, Green, Ford, Doctor, NewMexico, Y2015),
            PositionAttributes(Sophie, Blue, Toyota, Nurse, Arizona, Y2001),
            PositionAttributes(Olivia, Black, Kia, Biologist, Texas, Y2005))
        val actual5 = PuzzleConstraints.texasCarPosition(solution5)
        val expected5 = Fifth
        assertEquals("Texas car is at fifth", expected5, actual5)
    }
}
