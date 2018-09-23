// ATTENTION:
//
// This file contains only provided support code.
// The contents fo this file should not be modified as part of your solution.
//
// The PuzzleSolver object below should be treated as a black box.
// You will need to call its two documented methods to implement and
// test your solution. However, you do not need to read the code in this file,
// nor do you need to understand how it works since the code in this file uses
// many Scala features that we have not covered in class.

/**
 * Object packaging methods used to find a solution to the logic puzzle.
 *
 * This object includes an implementation of the DPLL algorithm
 * to provide a reasonably efficient SAT solver:
 *
 * https://en.wikipedia.org/wiki/DPLL_algorithm
 */
object PuzzleSolver {
  import scala.util.Try

  /**
   * Given a set of puzzle constraints,
   * optionally returns all symbol assignments for a satisfying solution.
   *
   * Each symbol is paired with either a true or false value,
   * and each symbol appears only once in the Array.
   *
   * Returns None if a contradiction was found.
   */
  def findSolutionAssignments(constraints: CNF): Option[Array[(Sym, Boolean)]] =
    Try {
      val constraintList = ConstraintsHelper.cnf2list(constraints)
      val solutionEnv = DPLL.dpll(constraintList, Map.empty)
      solutionEnv.toArray
    }.toOption

  /**
   * A convenience wrapper around findSolutionAssignments,
   * which converts the optional symbol assignments into an optional Solution.
   * Returns None if no solution was found by findSolutionAssignments.
   *
   * Before attempting to solve, input puzzle constraints are augmented with
   * additional basic constraints implied by the problem description:
   *   - Each position has only one car and one driver; therefore,
   *     only one of each attribute type is associated with each position.
   *   - Each attribute value appears at exactly one of the five positions.
   */
  def findSolution(constraints: CNF): Option[Solution] = {
    val fullConstraints = ConstraintsHelper.augmentPuzzleHints(constraints)
    findSolutionAssignments(fullConstraints) map {
      assignments =>
        val trueAttributes = assignments.filter(_._2).map(_._1)
        val attributesByPosition =
          ( trueAttributes.groupBy(_.position).toList
            .sortBy(x => ConstraintsHelper.positions.indexOf(x._1))
            .map(_._2.toList)
            .map(describePosition) )
        val List((First, a), (Second, b), (Third, c), (Fourth, d), (Fifth, e)) =
          attributesByPosition
        Solution(a, b, c, d, e)
    }
  }

  private def describePosition(symbols: List[Sym]): (Position, PositionAttributes) = {
    require(symbols.nonEmpty, "No attributes found for position")
    type F = (Name, Color, Make, Profession, State, Year) => PositionAttributes
    val position = symbols.head.position
    require(symbols.size == 6, s"Conflicting attributes found for $position position")
    val f: F = symbols.foldLeft(PositionAttributes.apply _) { (f, sym) =>
      sym.attribute match {
        case n: Name => ((_, c, m, j, s, y) => f(n, c, m, j, s, y)): F
        case c: Color => ((n, _, m, j, s, y) => f(n, c, m, j, s, y)): F
        case m: Make => ((n, c, _, j, s, y) => f(n, c, m, j, s, y)): F
        case j: Profession => ((n, c, m, _, s, y) => f(n, c, m, j, s, y)): F
        case s: State => ((n, c, m, j, _, y) => f(n, c, m, j, s, y)): F
        case y: Year => ((n, c, m, j, s, _) => f(n, c, m, j, s, y)): F
      }
    }
    position -> f(null, null, null, null, null, null)
  }

  private type DisjList = List[Literal]
  private type ConjList = List[DisjList]

  private object ConstraintsHelper {
    val positions = List(First, Second, Third, Fourth, Fifth)
    val names = List(Ava, Emma, Isabel, Olivia, Sophie)
    val colors = List(Blue, Green, Red, White, Black)
    val makes = List(Ford, Honda, Jeep, Kia, Toyota)
    val professions = List(Biologist, Doctor, Engineer, Lawyer, Nurse)
    val states = List(Arizona, California, Louisiana, NewMexico, Texas)
    val years = List(Y2001, Y2005, Y2010, Y2015, Y2018)
    val attributeGroups = List(names, colors, makes, professions, states, years)

    def augmentPuzzleHints(hints: CNF): CNF =
      Conj(hints, oneOfEachAtEach)

    def cnf2list(cnf: CNF): ConjList =
      cnf match {
        case Conj(xs, ys) => cnf2list(xs) ++ cnf2list(ys)
        case x: Clause => List(clause2list(x))
      }

    def clause2list(clause: Clause): DisjList =
      clause match {
        case Disj(xs, ys) => clause2list(xs) ++ clause2list(ys)
        case x: Literal => List(x)
      }

    def oneOfEach(pos: Position, attrs: List[PuzzleAttribute]): CNF = {
      val atLeastOne = attrs.map(x => (x isOrdered pos): Clause).reduceLeft(_ or _)

      val atMostOne = {
        for {
          x <- attrs
          y <- attrs
          if x != y
        } yield ((x isNotOrdered pos) or (y isNotOrdered pos)): CNF
      }.reduceLeft(_ and _)

      atLeastOne and atMostOne
    }

    def oneOfEachAtEach: CNF = {
      val oneAtEach = {
        for {
          p <- positions
          ag <- attributeGroups
        } yield oneOfEach(p, ag)
      }.reduceLeft(_ and _)

      val eachAttr = {
        for {
          attrGroup <- attributeGroups
          a <- attrGroup
          } yield {
            for (p <- positions) yield { (a isOrdered p): Clause }
          }.reduceLeft(_ or _): CNF
      }.reduceLeft(_ and _)

      oneAtEach and eachAttr
    }
  }

  private object DPLL {

    import scala.collection.breakOut

    type Env = Map[Sym, Boolean]

    def contradictionError = sys.error("Contradiction")
    def unsatError = sys.error("Unsatisfiable")

    def consistent(cnf: ConjList) = cnf.isEmpty

    def unsatisfiable(cnf: ConjList) = cnf.exists(_.isEmpty)

    def lookup(atom: Literal, env: Env) =
      env.get(atom.symbol).map(_ == atom.truth)

    def simplify(cnf: ConjList, env: Env) = {
      def help(atoms: DisjList, acc: DisjList): Option[DisjList] = {
        if (atoms.isEmpty) Some(acc)
        else lookup(atoms.head, env) match {
          case Some(true) => None // the clause is true, so we can eliminate it
          case Some(false) => help(atoms.tail, acc) // drop false values from clause
          case None => help(atoms.tail, atoms.head :: acc) // leave it in the clause
        }
      }
      cnf.flatMap(help(_, Nil))
    }

    def updateAssignment(env: Env, atom: Literal) =
      env.get(atom.symbol) match {
        case Some(truth) if truth != atom.truth => contradictionError
        case None => env + (atom.symbol -> atom.truth)
        case _ => env
      }

    def unitProp(cnf: ConjList, env: Env): (ConjList, Env) = {
      val (units, others) = cnf.partition(_.size == 1)
      if (units.isEmpty) cnf->env
      else {
        val env2 = units.map(_.head).foldLeft(env)(updateAssignment)
        simplify(others, env2)->env2
      }
    }

    def pureProp(cnf: ConjList, env: Env): (ConjList, Env) = {
      val atomSets: Set[Literal] = cnf.flatten.toSet
      val pureLits = for {
        (k, vs) <- atomSets.groupBy(_.symbol)
        if vs.size == 1
      } yield (k, vs.head.truth)
      if (pureLits.isEmpty) cnf->env
      else {
        val env2 = env ++ pureLits
        simplify(cnf, env2)->env2
      }
    }

    def allSyms(cnf: ConjList): Stream[Sym] =
      (for {
        clause <- cnf
        literal <- clause
      } yield literal.symbol)(breakOut)

    def findNextSym(cnf: ConjList, env: Env): Sym = {
      allSyms(cnf).dropWhile(env contains _).head
    }

    def dpll(cnf: ConjList, env: Env): Env = {
      if (consistent(cnf)) env
      else if (unsatisfiable(cnf)) unsatError
      else {
        val (cnf2, env2) = unitProp(cnf, env)
        val (cnf3, env3) = pureProp(cnf2, env2)
        if (env3.size > env.size) dpll(cnf3, env3)
        else {
          val nextSym = findNextSym(cnf, env)
          (Try {
            val env4 = env + (nextSym->true)
            dpll(simplify(cnf, env4), env4)
            } orElse Try {
              val env4 = env + (nextSym->false)
              dpll(simplify(cnf, env4), env4)
            }).get
        }
      }
    }
  }
}
