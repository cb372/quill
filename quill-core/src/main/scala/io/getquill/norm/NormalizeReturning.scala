package io.getquill.norm

import io.getquill.ast._

object NormalizeReturning extends StatelessTransformer {
  override def apply(e: Action): Action = {
    e match {
      case Returning(Insert(query, assignments), returning) =>
        Insert(query, assignments.filterNot(_.property == returning))
      case Returning(Update(query, assignments), returning) =>
        Update(query, assignments.filterNot(_.property == returning))
      case _ => super.apply(e)
    }
  }
}
