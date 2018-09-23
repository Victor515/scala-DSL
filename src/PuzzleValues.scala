// ATTENTION:
//
// This file contains only provided support code.
// The contents fo this file should not be modified as part of your solution.
//
// You will need to use the types and values defined in this file
// to complete the implementation of your homework solution.

/**
 * Abstract attribute type for the position
 * of a car and its driver in line.
 */
abstract class Position
case object First extends Position
case object Second extends Position
case object Third extends Position
case object Fourth extends Position
case object Fifth extends Position

/**
 * Abstract attribute type for the name of a driver.
 */
abstract class Name extends PuzzleAttribute
case object Ava extends Name
case object Emma extends Name
case object Isabel extends Name
case object Olivia extends Name
case object Sophie extends Name

/**
 * Abstract attribute type for the color of a car.
 */
abstract class Color extends PuzzleAttribute
case object Blue extends Color
case object Green extends Color
case object Red extends Color
case object White extends Color
case object Black extends Color

/**
 * Abstract attribute type for the make of a car.
 */
abstract class Make extends PuzzleAttribute
case object Ford extends Make
case object Honda extends Make
case object Jeep extends Make
case object Kia extends Make
case object Toyota extends Make

/**
 * Abstract attribute type for the profession of a driver.
 */
abstract class Profession extends PuzzleAttribute
case object Biologist extends Profession
case object Doctor extends Profession
case object Engineer extends Profession
case object Lawyer extends Profession
case object Nurse extends Profession

/**
 * Abstract attribute type for the state of a car's plates.
 */
abstract class State extends PuzzleAttribute
case object Arizona extends State
case object California extends State
case object Louisiana extends State
case object NewMexico extends State
case object Texas extends State

/**
 * Abstract attribute type for the year of a car.
 */
abstract class Year extends PuzzleAttribute
case object Y2001 extends Year
case object Y2005 extends Year
case object Y2010 extends Year
case object Y2015 extends Year
case object Y2018 extends Year

/**
 * Compound data type for recording all attributes
 * for the car and driver at a single position in line.
 */
case class PositionAttributes(
  name: Name,
  carColor: Color,
  carMake: Make,
  profession: Profession,
  carState: State,
  carYear: Year)

/**
 * Compound data type for recording all attributes
 * for each car and driver at each position in line.
 */
case class Solution(
  first: PositionAttributes,
  second: PositionAttributes,
  third: PositionAttributes,
  fourth: PositionAttributes,
  fifth: PositionAttributes)
