package io.getquill.context.mirror

import io.getquill.MirrorIdiom
import io.getquill.Spec
import io.getquill.testContext
import io.getquill.testContext._
import io.getquill.idiom.StatementInterpolator._
import io.getquill.Literal
import io.getquill.ast.Ast
import io.getquill.ast._

class MirrorIdiomSpec extends Spec {

  import MirrorIdiom._

  implicit val naming = Literal

  "shows schema" - {
    "table" in {
      val q = quote {
        query[TestEntity].schema(_.entity("test"))
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].schema(_.entity("test"))"""
    }
    "columns" in {
      val q = quote {
        query[TestEntity].schema(_.columns(_.i -> "'i", _.o -> "'o"))
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].schema(_.columns(_.i -> "'i", _.o -> "'o"))"""
    }
    "composed" in {
      val q = quote {
        query[TestEntity].schema(_.entity("entity_alias").columns(_.s -> "s_alias", _.i -> "i_alias"))
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].schema(_.entity("entity_alias").columns(_.s -> "s_alias", _.i -> "i_alias"))"""
    }
  }

  "shows queries" - {

    "complex" in {
      val q = quote {
        query[TestEntity].filter(t => t.s == "test").flatMap(t => query[TestEntity]).drop(9).take(10).map(t => t)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].flatMap(t => query[TestEntity].filter(x => t.s == "test")).drop(9).take(10)"""
    }
  }

  "shows set operation queries" - {
    "union" in {
      val q = quote {
        qr1.filter(a => a.s == "s").union(qr1.filter(b => b.i == 1))
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].filter(a => a.s == "s").union(query[TestEntity].filter(b => b.i == 1))"""
    }
    "unionAll" in {
      val q = quote {
        qr1.filter(a => a.s == "s").unionAll(qr1.filter(b => b.i == 1))
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].filter(a => a.s == "s").unionAll(query[TestEntity].filter(b => b.i == 1))"""
    }
  }

  "shows join queries" - {
    "inner join" in {
      val q = quote {
        qr1.join(qr2).on((a, b) => a.s == b.s)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].join(query[TestEntity2]).on((a, b) => a.s == b.s)"""
    }
    "left join" in {
      val q = quote {
        qr1.leftJoin(qr2).on((a, b) => a.s == b.s)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].leftJoin(query[TestEntity2]).on((a, b) => a.s == b.s)"""
    }
    "right join" in {
      val q = quote {
        qr1.rightJoin(qr2).on((a, b) => a.s == b.s)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].rightJoin(query[TestEntity2]).on((a, b) => a.s == b.s)"""
    }
    "full join" in {
      val q = quote {
        qr1.fullJoin(qr2).on((a, b) => a.s == b.s)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].fullJoin(query[TestEntity2]).on((a, b) => a.s == b.s)"""
    }
  }

  "shows sorted queries" - {
    "asc" in {
      val q = quote {
        qr1.sortBy(t => t.i)(Ord.asc)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => t.i)(Ord.asc)"""
    }
    "desc" in {
      val q = quote {
        qr1.sortBy(t => t.i)(Ord.desc)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => t.i)(Ord.desc)"""
    }
    "ascNullsFirst" in {
      val q = quote {
        qr1.sortBy(t => t.i)(Ord.ascNullsFirst)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => t.i)(Ord.ascNullsFirst)"""
    }
    "descNullsFirst" in {
      val q = quote {
        qr1.sortBy(t => t.i)(Ord.descNullsFirst)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => t.i)(Ord.descNullsFirst)"""
    }
    "ascNullsLast" in {
      val q = quote {
        qr1.sortBy(t => t.i)(Ord.ascNullsLast)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => t.i)(Ord.ascNullsLast)"""
    }
    "descNullsLast" in {
      val q = quote {
        qr1.sortBy(t => t.i)(Ord.descNullsLast)
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => t.i)(Ord.descNullsLast)"""
    }
    "tuple" in {
      val q = quote {
        qr1.sortBy(t => (t.i, t.s))(Ord(Ord.descNullsLast, Ord.ascNullsLast))
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].sortBy(t => (t.i, t.s))(Ord(Ord.descNullsLast, Ord.ascNullsLast))"""
    }
  }

  "shows grouped queries" in {
    val q = quote {
      qr1.groupBy(t => t.i)
    }
    translate(q.ast) mustEqual
      stmt"""query[TestEntity].groupBy(t => t.i)"""
  }

  "shows functions" in {
    val q = quote {
      (s: String) => s
    }
    translate(q.ast) mustEqual
      stmt"""(s) => s"""
  }

  "shows operations" - {
    "unary" in {
      val q = quote {
        (xs: testContext.Query[_]) => !xs.nonEmpty
      }
      translate(q.ast) mustEqual
        stmt"""(xs) => !xs.nonEmpty"""
    }
    "binary" in {
      val q = quote {
        (xs: testContext.Query[_]) => xs.nonEmpty && xs != null
      }
      translate(q.ast) mustEqual
        stmt"""(xs) => xs.nonEmpty && (xs != null)"""
    }
    "function apply" - {
      "function reference" in {
        val q = quote {
          (s: String => String) => s("a")
        }
        translate(q.ast) mustEqual
          stmt"""(s) => s.apply("a")"""
      }
      "local function" in {
        val q = quote {
          ((s: String) => s)("s")
        }
        translate(q.ast) mustEqual
          stmt""""s""""
      }
    }
  }

  "shows aggregations" - {
    "min" in {
      val q = quote {
        qr1.map(t => t.i).min
      }
      translate(q.ast) mustEqual
        stmt"query[TestEntity].map(t => t.i).min"
    }
    "max" in {
      val q = quote {
        qr1.map(t => t.i).max
      }
      translate(q.ast) mustEqual
        stmt"query[TestEntity].map(t => t.i).max"
    }
    "avg" in {
      val q = quote {
        qr1.map(t => t.i).avg
      }
      translate(q.ast) mustEqual
        stmt"query[TestEntity].map(t => t.i).avg"
    }
    "sum" in {
      val q = quote {
        qr1.map(t => t.i).sum
      }
      translate(q.ast) mustEqual
        stmt"query[TestEntity].map(t => t.i).sum"
    }
    "size" in {
      val q = quote {
        qr1.map(t => t.i).size
      }
      translate(q.ast) mustEqual
        stmt"query[TestEntity].map(t => t.i).size"
    }
  }

  "shows unary operators" - {
    "prefix" - {
      "!" in {
        val q = quote {
          (s: String) => !(s == "s")
        }
        translate(q.ast) mustEqual
          stmt"""(s) => !(s == "s")"""
      }
    }
    "prostfix" - {
      "isEmpty" in {
        val q = quote {
          (xs: testContext.Query[_]) => xs.isEmpty
        }
        translate(q.ast) mustEqual
          stmt"""(xs) => xs.isEmpty"""
      }
      "nonEmpty" in {
        val q = quote {
          (xs: testContext.Query[_]) => xs.nonEmpty
        }
        translate(q.ast) mustEqual
          stmt"""(xs) => xs.nonEmpty"""
      }
    }
  }

  "shows binary operators" - {
    "-" in {
      val q = quote {
        (a: Int, b: Int) => a - b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a - b"""
    }
    "+" in {
      val q = quote {
        (a: Int, b: Int) => a + b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a + b"""
    }
    "*" in {
      val q = quote {
        (a: Int, b: Int) => a * b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a * b"""
    }
    "==" in {
      val q = quote {
        (a: Int, b: Int) => a == b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a == b"""
    }
    "!=" in {
      val q = quote {
        (a: Int, b: Int) => a != b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a != b"""
    }
    "&&" in {
      val q = quote {
        (a: Boolean, b: Boolean) => a && b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a && b"""
    }
    "||" in {
      val q = quote {
        (a: Boolean, b: Boolean) => a || b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a || b"""
    }
    ">" in {
      val q = quote {
        (a: Int, b: Int) => a > b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a > b"""
    }
    ">=" in {
      val q = quote {
        (a: Int, b: Int) => a >= b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a >= b"""
    }
    "<" in {
      val q = quote {
        (a: Int, b: Int) => a < b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a < b"""
    }
    "<=" in {
      val q = quote {
        (a: Int, b: Int) => a <= b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a <= b"""
    }
    "/" in {
      val q = quote {
        (a: Int, b: Int) => a / b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a / b"""
    }
    "%" in {
      val q = quote {
        (a: Int, b: Int) => a % b
      }
      translate(q.ast) mustEqual
        stmt"""(a, b) => a % b"""
    }
  }

  "shows properties" in {
    val q = quote {
      (e: TestEntity) => e.s
    }
    translate(q.ast) mustEqual
      stmt"""(e) => e.s"""
  }

  "shows values" - {
    "constant" - {
      "string" in {
        val q = quote {
          "test"
        }
        translate(q.ast) mustEqual
          stmt""""test""""
      }
      "unit" in {
        val q = quote {
          {}
        }
        translate(q.ast) mustEqual
          stmt"""{}"""
      }
      "value" in {
        val q = quote {
          1
        }
        translate(q.ast) mustEqual
          stmt"""1"""
      }
    }
    "null" in {
      val q = quote {
        1 != null
      }
      translate(q.ast) mustEqual
        stmt"""1 != null"""
    }
    "tuple" in {
      val q = quote {
        (null, 1, "a")
      }
      translate(q.ast) mustEqual
        stmt"""(null, 1, "a")"""
    }
    "collection" - {
      def verify(ast: Ast) = translate(ast) mustEqual stmt"Collection(1, 2, 3)"
      "set" in {
        val q = quote {
          Set(1, 2, 3)
        }
        verify(q.ast)
      }
      "list" in {
        val q = quote {
          List(1, 2, 3)
        }
        verify(q.ast)
      }
      "seq" in {
        val q = quote {
          Seq(1, 2, 3)
        }
        verify(q.ast)
      }
    }
  }

  "shows idents" in {
    val q = quote {
      (a: String) => a
    }
    translate(q.ast) mustEqual
      stmt"""(a) => a"""
  }

  "shows actions" - {
    "update" in {
      val q = quote {
        query[TestEntity].filter(t => t.s == "test").update(t => t.s -> "a")
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].filter(t => t.s == "test").update(t => t.s -> "a")"""
    }
    "insert" - {
      "normal" in {
        val q = quote {
          query[TestEntity].insert(t => t.s -> "a")
        }
        translate(q.ast) mustEqual
          stmt"""query[TestEntity].insert(t => t.s -> "a")"""
      }

      "returning" in {
        val q = quote {
          query[TestEntity].insert(t => t.s -> "a").returning(_.l)
        }
        translate(q.ast) mustEqual
          stmt"""query[TestEntity].insert(t => t.s -> "a")"""
      }
    }

    "delete" in {
      val q = quote {
        query[TestEntity].delete
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].delete"""
    }
  }

  "shows infix" - {
    "as part of the query" in {
      val q = quote {
        qr1.filter(t => infix"true".as[Boolean])
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].filter(t => infix"true")"""
    }
    "with params" in {
      val q = quote {
        qr1.filter(t => infix"${t.s} == 's'".as[Boolean])
      }
      translate(q.ast) mustEqual
        stmt"""query[TestEntity].filter(t => infix"$${t.s} == 's'")"""
    }
  }

  "shows inline statement" in {
    val q = quote {
      val a = 1
      1 / a
    }
    translate(q.ast) mustEqual stmt"1 / 1"
  }

  "shows option operations" - {
    "map" in {
      val q = quote {
        (o: Option[Int]) => o.map(v => v)
      }
      translate(q.ast) mustEqual
        stmt"(o) => o.map((v) => v)"
    }
    "forall" in {
      val q = quote {
        (o: Option[Boolean]) => o.forall(v => v)
      }
      translate(q.ast) mustEqual
        stmt"(o) => o.forall((v) => v)"
    }
    "exists" in {
      val q = quote {
        (o: Option[Boolean]) => o.exists(v => v)
      }
      translate(q.ast) mustEqual
        stmt"(o) => o.exists((v) => v)"
    }
  }

  "shows bindings" - {
    "quotedReference" in {
      val ast: Ast = QuotedReference("ignore", Filter(Ident("a"), Ident("b"), Ident("c")))
      translate(ast) mustEqual
        stmt"""a.filter(b => c)"""
    }

    "lift" in {
      val q = quote {
        lift(1)
      }
      translate(q.ast) mustEqual
        stmt"${(q.ast: Lift).token}"
    }
  }

  "shows dynamic asts" in {
    translate(Dynamic(1)) mustEqual
      stmt"1"
  }

  "shows if" in {
    val q = quote {
      (i: Int) =>
        if (i > 10) "a" else "b"
    }
    translate(q.ast.body) mustEqual
      stmt"""if(i > 10) "a" else "b""""
  }

  "shows distinct" in {
    val q = quote {
      query[TestEntity].distinct
    }
    translate(q.ast) mustEqual
      stmt"""query[TestEntity].distinct"""
  }
}