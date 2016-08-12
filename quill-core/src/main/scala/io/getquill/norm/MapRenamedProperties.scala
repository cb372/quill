package io.getquill.norm

import io.getquill.ast._

object MapRenamedProperties extends StatelessTransformer {

  def unapply(q: Query): Option[Query] =
    q match {
      case ConfiguredEntity(e, alias, props) if (props.nonEmpty) =>
        val ident = Ident("x")
        val fields = props.map {
          case PropertyAlias(a, b) =>
            Ident(a) -> Property(ident, b)
        }
        val record = Record(fields.toMap, ident)
        val entity = alias.map(SimpleEntity(_)).getOrElse(e)
        Some(Map(entity, ident, record))
      case q =>
        None
    }
}
