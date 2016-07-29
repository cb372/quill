package io.getquill.context.sql.norm

import io.getquill.norm.FlattenOptionOperation
import io.getquill.norm.RenameAssignments
import io.getquill.norm.Normalize
import io.getquill.norm.RenameProperties
import io.getquill.ast.Ast

object SqlNormalize {

  private val normalize =
    (identity[Ast] _)
      .andThen(Normalize.apply _)
      .andThen(ExpandJoin.apply _)
      .andThen(Normalize.apply _)
      .andThen(MergeSecondaryJoin.apply _)
      .andThen(FlattenOptionOperation.apply _)
      .andThen(RenameAssignments.apply _)
      .andThen(RenameProperties.apply _)

  def apply(ast: Ast) = normalize(ast)
}
