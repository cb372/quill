package io.getquill.idiom

import io.getquill.dsl.EncodingDsl
import io.getquill.ast.ScalarLift
import io.getquill.util.Interleave

object ReifyStatement {

  def forExecution(liftingPlaceholder: Int => String, statement: Statement): (String, List[ScalarLift]) = ???
  def forProbing(liftingPlaceholder: Int => String, statement: Statement): String = ???

  def apply(liftingPlaceholder: Int => String, statement: Statement, forProbing: Boolean): (String, List[ScalarLift]) = {
    def apply(acc: (String, List[ScalarLift]), token: Token): (String, List[ScalarLift]) =
      (acc, token) match {
        case ((s1, liftings), StringToken(s2))    => (s"$s1$s2", liftings)
        case ((s1, liftings), Statement(tokens))  => tokens.foldLeft((s1, liftings))(apply)
        case ((s1, liftings), LiftingToken(lift)) => (s"$s1${liftingPlaceholder(liftings.size)}", liftings :+ lift)
      }
    val expanded =
      forProbing match {
        case true => statement
        case false =>
          Statement {
            statement.tokens.foldLeft(List.empty[Token]) {
              case (tokens, LiftingToken(lift)) =>
                val encoder = lift.encoder.asInstanceOf[EncodingDsl#Encoder[Any]]
                val liftings = encoder.expand(lift.name, lift.value).map(LiftingToken)
                val separators = List.fill(liftings.size - 1)(StringToken(","))
                tokens ++ Interleave(liftings, separators)
              case (tokens, token) =>
                tokens :+ token
            }
          }
      }
    apply(("", List.empty), expanded)
  }
}
