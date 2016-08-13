package io.getquill.norm

import io.getquill.ast.Ast
import io.getquill.ast.Query
import io.getquill.ast.StatelessTransformer
import io.getquill.norm.capture.AvoidCapture
import io.getquill.ast.Action

object Normalize extends StatelessTransformer {

  override def apply(q: Ast): Ast =
    super.apply(BetaReduction(q))

  override def apply(q: Action): Action =
    super.apply(ApplyIntermediateMap(NormalizeReturning(q)))

  override def apply(q: Query): Query =
    ApplyIntermediateMap(norm(AvoidCapture(q)))

  private def norm(q: Query): Query =
    q match {
      case NormalizeNestedStructures(query) => norm(query)
      case MapRenamedProperties(query)      => norm(query)
      case SymbolicReduction(query)         => norm(query)
      case AdHocReduction(query)            => norm(query)
      case OrderTerms(query)                => norm(query)
      case other                            => other
    }
}
