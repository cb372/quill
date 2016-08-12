package io.getquill.norm

import io.getquill.ast._

object ApplyIntermediateMap extends StatelessTransformer {

  private def isomorphic(e: Ast, c: Ast, alias: Ident) =
    BetaReduction(e, alias -> c) == c

  override def apply(q: Action): Action =
    q match {

      case Returning(i @ Insert(Map(q, x, p), assignments), alias, prop) =>
        val t = BetaReduction(collection.Map(alias -> p))
        Returning(apply(i), alias, t(prop))

      case Returning(i @ Update(Map(q, x, p), assignments), alias, prop) =>
        val t = BetaReduction(collection.Map(alias -> p))
        Returning(apply(i), alias, t(prop))

      case Insert(Map(q, x, p), assignments) =>
        Insert(q, apply(x, p, assignments))

      case Update(Map(q, x, p), assignments) =>
        Update(q, apply(x, p, assignments))

      case Delete(Map(q, x, p)) =>
        Delete(q)

      case other =>
        super.apply(other)
    }

  private def apply(x: Ident, p: Ast, assignments: List[Assignment]) =
    assignments.map {
      case Assignment(alias, prop, value) =>
        val t = BetaReduction(collection.Map(alias -> p))
        val propr = t(prop)
        val valuer = t(value)
        Assignment(alias, propr, valuer)
    }

  override def apply(q: Query): Query =
    q match {

      case q @ Map(Map(a: GroupBy, b, c), d, e)       => q
      case q @ FlatMap(Map(a: GroupBy, b, c), d, e)   => q
      case q @ Filter(Map(a: GroupBy, b, c), d, e)    => q
      case q @ SortBy(Map(a: GroupBy, b, c), d, e, f) => q
      case q @ Take(Map(a: GroupBy, b, c), d)         => q
      case q @ Drop(Map(a: GroupBy, b, c), d)         => q
      case q @ Map(a: GroupBy, b, c) if (b == c)      => q

      //  map(i => (i.i, i.l)).distinct.map(x => (x._1, x._2)) =>
      //    map(i => (i.i, i.l)).distinct
      case Map(Distinct(Map(a, b, c)), d, e) if isomorphic(e, c, d) =>
        Distinct(Map(a, b, c))

      // a.map(b => b) =>
      //    a
      case Map(a: Query, b, c) if (b == c) =>
        a

      // a.map(b => c).map(d => e) =>
      //    a.map(b => e[d := c])
      case Map(Map(a, b, c), d, e) =>
        val er = BetaReduction(e, d -> c)
        Map(a, b, er)

      // a.map(b => c).flatMap(d => e) =>
      //    a.flatMap(b => e[d := c])
      case FlatMap(Map(a, b, c), d, e) =>
        val er = BetaReduction(e, d -> c)
        FlatMap(a, b, er)

      // a.map(b => c).filter(d => e) =>
      //    a.filter(b => e[d := c]).map(b => c)
      case Filter(Map(a, b, c), d, e) =>
        val er = BetaReduction(e, d -> c)
        Map(Filter(a, b, er), b, c)

      // a.map(b => c).sortBy(d => e) =>
      //    a.sortBy(b => e[d := c]).map(b => c)
      case SortBy(Map(a, b, c), d, e, f) =>
        val er = BetaReduction(e, d -> c)
        Map(SortBy(a, b, er, f), b, c)

      // a.map(b => c).drop(d) =>
      //    a.drop(d).map(b => c)
      case Drop(Map(a, b, c), d) =>
        Map(Drop(a, d), b, c)

      // a.map(b => c).take(d) =>
      //    a.drop(d).map(b => c)
      case Take(Map(a, b, c), d) =>
        Map(Take(a, d), b, c)

      case other =>
        super.apply(other)
    }
}
