package io.getquill

import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.idiom.Statement
import io.getquill.context.sql.idiom.QuestionMarkBindVariables

trait MirrorSqlDialect
  extends SqlIdiom
  with QuestionMarkBindVariables {
}

object MirrorSqlDialect extends MirrorSqlDialect {
  override def prepareForProbing(statement: Statement) = statement
}
