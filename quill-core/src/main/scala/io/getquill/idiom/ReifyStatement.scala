package io.getquill.idiom

import io.getquill.ast._

object ReifyStatement {

  def forExecution(liftingPlaceholder: Int => String, statement: Statement): (String, List[ScalarValueLift]) = ???
  def forProbing(liftingPlaceholder: Int => String, statement: Statement): String = ???

  def apply(
    liftingPlaceholder: Int => String,
    emptyQuery:         String,
    statement:          Statement, forProbing: Boolean
  ): (String, List[ScalarLift]) = {
    def apply(acc: (String, List[ScalarLift]), token: Token): (String, List[ScalarLift]) =
      (acc, token) match {
        case ((s1, liftings), StringToken(s2))       => (s"$s1$s2", liftings)
        case ((s1, liftings), Statement(tokens))     => tokens.foldLeft((s1, liftings))(apply)
        case ((s1, liftings), ScalarLiftToken(lift)) => (s"$s1${liftingPlaceholder(liftings.size)}", liftings :+ lift)
      }
    val expanded =
      forProbing match {
        case true => statement
        case false =>
          Statement {
            statement.tokens.foldLeft(List.empty[Token]) {
              case (tokens, ScalarLiftToken(lift: ScalarQueryLift)) =>
                lift.value.asInstanceOf[Traversable[Any]].toList match {
                  case Nil => List(StringToken(emptyQuery))
                  case values =>
                    val liftings = values.map(v => ScalarLiftToken(ScalarValueLift(lift.name, v, lift.encoder)))
                    tokens ++ liftings
                }
              case (tokens, token) =>
                tokens :+ token
            }
          }
      }
    apply(("", List.empty), expanded)
  }
}
