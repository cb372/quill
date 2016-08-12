package io.getquill.norm

import io.getquill.ast._

case class BetaReduction(map: collection.Map[Ident, Ast])
  extends StatelessTransformer {

  override def apply(ast: Ast) =
    ast match {
      case Property(Tuple(values), name) =>
        val aliases = values.distinct
        aliases match {
          case alias :: Nil if values.size > 1 =>
            super.apply(Property(alias, name))
          case _ => apply(values(name.drop(1).toInt - 1))
        }

      case FunctionApply(Function(params, body), values) =>
        apply(BetaReduction(map ++ params.zip(values).toMap).apply(body))

      case ident: Ident =>
        map.get(ident).map(BetaReduction(map - ident)(_)).getOrElse(ident)

      case Function(params, body) =>
        Function(params, BetaReduction(map -- params)(body))

      case OptionOperation(t, a, b, c) =>
        OptionOperation(t, apply(a), b, BetaReduction(map - b)(c))

      case Block(statements) =>
        val vals = statements.collect { case x: Val => x.name -> x.body }
        BetaReduction(map ++ vals)(statements.last)

      case Foreach(query, alias, body) =>
        Foreach(query, alias, BetaReduction(map - alias)(body))

      case Property(Record(fields, default), prop) =>
        fields.getOrElse(Ident(prop), Property(default, prop))

      case Returning(action, alias, prop) =>
        val t = BetaReduction(map - alias)
        Returning(apply(action), alias, t(prop))

      case other =>
        super.apply(other)
    }

  override def apply(e: Assignment) =
    e match {
      case Assignment(alias, prop, value) =>
        val t = BetaReduction(map - alias)
        Assignment(alias, t(prop), t(value))
    }

  override def apply(query: Query) =
    query match {
      case Filter(a, b, c) =>
        Filter(apply(a), b, BetaReduction(map - b)(c))
      case Map(a, b, c) =>
        Map(apply(a), b, BetaReduction(map - b)(c))
      case FlatMap(a, b, c) =>
        FlatMap(apply(a), b, BetaReduction(map - b)(c))
      case SortBy(a, b, c, d) =>
        SortBy(apply(a), b, BetaReduction(map - b)(c), d)
      case GroupBy(a, b, c) =>
        GroupBy(apply(a), b, BetaReduction(map - b)(c))
      case Join(t, a, b, iA, iB, on) =>
        Join(t, apply(a), apply(b), iA, iB, BetaReduction(map - iA - iB)(on))
      case _: Take | _: Entity | _: Drop | _: Union | _: UnionAll | _: Aggregation | _: Distinct =>
        super.apply(query)
    }
}

object BetaReduction {

  def apply(ast: Ast, t: (Ident, Ast)*): Ast =
    BetaReduction(t.toMap)(ast) match {
      case `ast` => ast
      case other => apply(other)
    }
}
