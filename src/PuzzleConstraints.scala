/**
 * Object packaging the constraints of the logic puzzle.
 */
object PuzzleConstraints {

  // The Doctor is third from the front.
  def c01: CNF = ???

  // The woman in the 2015 vehicle is somewhere behind
  // the car with Louisiana plates, and somewhere ahead of the Blue car.
  def c02: CNF = ???

  // Sophie is directly ahead of the Biologist.
  def c03: CNF = ???

  // The Kia has a California plate.
  def c04: CNF = ???

  // The White car is directly behind the car with Louisiana plates.
  def c05: CNF = ???

  // Olivia is somewhere behind the Green car.
  def c06: CNF = ???

  // The Ford has New Mexico plates.
  def c07: CNF = ???

  // The White car is somewhere ahead of the Ford.
  def c08: CNF = ???

  // The car with Arizona plates is directly behind the 2015 vehicle.
  def c09: CNF = ???

  // The 2018 vehicle is Red.
  def c10: CNF = ???

  // The Lawyer is in the first car.
  def c11: CNF = ???

  // The Nurse is directly behind the 2015 vehicle.
  def c12: CNF = ???

  // The Engineer is somewhere behind Ava, and somewhere ahead of the Blue car.
  def c13: CNF = ???

  // The second car is a 2010 vehicle.
  def c14: CNF = ???

  // Isabel is in the Green car.
  def c15: CNF = ???

  // The 2001 vehicle is directly ahead of the Kia.
  def c16: CNF = ???

  // The 2001 vehicle is a Toyota.
  def c17: CNF = ???

  // The Blue car is somewhere behind the Honda,
  // and somewhere ahead of the Black car.
  def c18: CNF = ???

  // The White car is somewhere behind the Honda,
  // and somewhere ahead of the Blue car.
  def c19: CNF = ???

  /**
   * Conjunction of all 18 constraints derived from the puzzle hints.
   */
  def constraints: CNF =
    ( c01 and c02 and c03 and c04 and c05 and c06
      and c07 and c08 and c09 and c10 and c11 and c12
      and c13 and c14 and c15 and c16 and c17 and c18 and c19)

  /**
   * Solution to the puzzle given the constraints above.
   * Returns None if no solution was found by the solver.
   */
  def solution: Option[Solution] = PuzzleSolver.findSolution(constraints)

  /**
   * Returns the position of the car with Texas plates in the provided solution.
   * Will throw an Exception if the car is not found in the solution
   * (i.e., the provided solution is invalid).
   */
  def texasCarPosition(solution: Solution): Position = {
    // Hint: Use a match expression to destructure the solution.
    ???
  }
}
