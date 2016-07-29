package io.getquill.norm

import io.getquill.ast._

object RenameAssignments extends StatelessTransformer {

  override def apply(e: Action): Action =
    e match {
      case Insert(table: Entity, assignments) =>
        Insert(table, renameAssignments(assignments, table))

      case Update(table: Entity, assignments) =>
        Update(table, renameAssignments(assignments, table))

      case Update(filter @ Filter(table: Entity, x, where), assignments) =>
        Update(filter, renameAssignments(assignments, table))

      case other =>
        super.apply(other)
    }

  private def renameAssignments(assignments: List[Assignment], table: Entity) = {
    val propertyAlias = table.properties.map(p => p.property -> p.alias).toMap
    assignments.map(a => a.copy(property = propertyAlias.getOrElse(a.property, a.property)))
  }
}
