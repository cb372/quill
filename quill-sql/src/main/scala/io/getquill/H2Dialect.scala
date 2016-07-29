package io.getquill

import io.getquill.idiom.StatementInterpolator._
import java.util.concurrent.atomic.AtomicInteger
import io.getquill.context.sql.idiom.PositionalBindVariables
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.idiom.Statement

trait H2Dialect
  extends SqlIdiom
  with PositionalBindVariables {

  private[getquill] val preparedStatementId = new AtomicInteger

  override def prepareForProbing(statement: Statement) =
    stmt"PREPARE p${preparedStatementId.incrementAndGet.toString.token} AS $statement}"
}

object H2Dialect extends H2Dialect
