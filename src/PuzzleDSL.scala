/**
 * Abstract implementation of a Conjunctive Normal Form statement.
 */
abstract class CNF {
  /**
   * Logical AND operator for combining two CNF statements.
   */
  def and(that: CNF): CNF = Conj(this, that)
}

/**
 * Conjunction of two simpler CNF statements.
 */
case class Conj(x: CNF, y: CNF) extends CNF

/**
 * Abstract implementation of a disjunctive clause.
 */
abstract class Clause extends CNF {
  /**
   * Logical OR operator for combining two disjunctive clauses.
   */
  def or(that: Clause) = Disj(this, that)
}

/**
 * Disjunction of two simpler CNF statements.
 */
case class Disj(x: Clause, y: Clause) extends Clause

/**
 * Literal (e.g., the variable `x`) in a CNF statement.
 * May be positive (e.g., `x`) or negative (e.g., `not x`).
 */
abstract class Literal extends Clause {
  /**
   * The symbol associated with this literal.
   * E.g., the symbol for the literal `not x` is `x`.
   */
  def symbol: Sym  // This is an abstract method.

  /**
   * Material implication operator for a literal.
   * The consequence maybe a literal or any other Clause.
   */
  def implies(that: Clause): Clause = Disj(this.negated, that)

  /**
   * Material biconditional operator for two literals.
   */
  def iff(that: Literal): CNF = Conj(this.implies(that), that.implies(this))

  /**
   * Returns the negated form of this literal.
   * At most one level of negation is allowed.
   * E.g., `not x` negated is `x`, and `x` negated is `not x`.
   * The literal `not not x` cannot be constructed, as it should be simply `x`.
   */
  def negated: Literal = {
    // Hint: Use a match expression. The `symbol` method may help too.
    this match {
      case Not(symbol) => symbol
      case Sym(_, _) => Not(this.symbol)
    }
  }

  /**
   * The truth value of this literal assuming its symbol is assigned true.
   * E.g., the truth of `x` is true, and the truth of `not x` is false.
   */
  def truth: Boolean = {
    // Hint: Use a match expression.
    this match {
      case Not(_) => false
      case Sym(_, _) => true
    }
  }
}

/**
 * The symbol of a literal (without any negations).
 * For our logic puzzle, each symbol is a position-attribute pair.
 * If the symbol has the value true, then the given attribute is associated
 * with the car or driver at the given position.
 * E.g., Sym(Third, Blue) being true means that the Blue car is Third in line.
 */
case class Sym(position: Position, attribute: PuzzleAttribute) extends Literal {
  def symbol = this
}

/**
 * Negated symbol literal.
 * Note that a single level of negation is enforced by the fact that
 * the single constructor argument is a Sym rather than a Literal.
 */
case class Not(symbol: Sym) extends Literal

/**
 * Abstract implementation of operations for all attribute value types.
 */
abstract class PuzzleAttribute {

  /**
   * Assertion that an attribute is associated
   * with the car/driver at the given position.
   *
   * You should use this operator to encode constraints
   * rather than constructing a Sym directly.
   * (This will make your code more strongly resemble a DSL.)
   */
  def isOrdered(position: Position): Literal =
    Sym(position, this)

  /**
   * Assertion that an attribute is not associated
   * with the car/driver at the given position.
   *
   * You should use this operator to encode constraints
   * and implement other operations rather than calling
   * literal.negated or constructing a Not directly.
   * (This will make your code more strongly resemble a DSL.)
   */
  def isNotOrdered(position: Position): Literal =
    (this isOrdered position).negated

  /**
   * Material implication at a given position of another attribute by this attribute.
   * This method should be used as a helper to implement the implies method below.
   * You should not call this method directly in your constraint definitions.
   */
  def impliesAt(position: Position, that: PuzzleAttribute): CNF =
    Sym(position, this) implies Sym(position, that)

  /**
   * Material implication operator for two attributes at the same position,
   * asserted for all five positions.
   */
  def implies(that: PuzzleAttribute): CNF = {
    // Hint: Use impliesAt for all five positions.
    impliesAt(First, that) and impliesAt(Second, that) and
      impliesAt(Third, that) and impliesAt(Fourth, that) and impliesAt(Fifth, that)
  }

  /**
   * Material biconditional operator for two attributes at the same position,
   * asserted for all five positions.
   */
  def iff(that: PuzzleAttribute): CNF = {
    this.implies(that) and that.implies(this)
  }

  /**
   * Ordering operator for two attributes,
   * asserting that the car/driver with `this` attribute
   * immediately follows the car/driver with `that` attribute in line.
   */
  def justBehind(that: PuzzleAttribute): CNF = {
    // Hint: This one is a bit more complex than the previous operators.
    // Think about all possible relative positions, and which are valid here.
    (Sym(Second, this) iff Sym(First, that)) and (Sym(Third, this) iff Sym(Second, that)) and
      (Sym(Fourth, this) iff Sym(Third, that)) and (Sym(Fifth, this) iff Sym(Fourth, that))
  }

  /**
   * Ordering operator for two attributes,
   * asserting that the car/driver with `this` attribute
   * follows the car/driver with `that` attribute in line,
   * but there may also be one or more cars between them in the line.
   */
  def behind(that: PuzzleAttribute): CNF = {
    // Hint: This one is a bit more complex than justBehind,
    // but you can use the same general strategy.
    (Sym(Second, this) iff Sym(First, that)) and (Sym(Third, this) iff Sym(First, that)) and
      (Sym(Third, this) iff Sym(Second, that)) and (Sym(Fourth, this) iff Sym(First, that)) and
      (Sym(Fourth, this) iff Sym(Second, that)) and (Sym(Fourth, this) iff Sym(Third, that)) and
      (Sym(Fifth, this) iff Sym(First, that)) and (Sym(Fifth, this) iff Sym(Second, that)) and
      (Sym(Fifth, this) iff Sym(Third, that)) and (Sym(Fifth, this) iff Sym(Fourth, that))

  }
}
