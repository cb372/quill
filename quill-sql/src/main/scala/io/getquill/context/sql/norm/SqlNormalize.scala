package io.getquill.context.sql.norm

import io.getquill.norm.FlattenOptionOperation
import io.getquill.norm.Normalize
import io.getquill.ast.Ast

object SqlNormalize {

  private val normalize =
    (identity[Ast] _)
      .andThen(Normalize.apply _)
      .andThen(ExpandJoin.apply _)
      .andThen(Normalize.apply _)
      .andThen(MergeSecondaryJoin.apply _)
      .andThen(FlattenOptionOperation.apply _)

  def apply(ast: Ast) = normalize(ast)
}
