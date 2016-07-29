package io.getquill

import io.getquill.idiom.StatementInterpolator._
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.idiom.Statement
import io.getquill.context.sql.idiom.QuestionMarkBindVariables

trait SqliteDialect
  extends SqlIdiom
  with QuestionMarkBindVariables {

  override def prepareForProbing(statement: Statement) = stmt"sqlite3_prepare_v2($statement)"
}

object SqliteDialect extends SqliteDialect
