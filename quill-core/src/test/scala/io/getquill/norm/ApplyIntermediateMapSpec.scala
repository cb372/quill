package io.getquill.norm

import io.getquill.Spec
import io.getquill.testContext.TestEntity
import io.getquill.testContext.implicitOrd
import io.getquill.testContext.qr1
import io.getquill.testContext.qr2
import io.getquill.testContext.query
import io.getquill.testContext.quote
import io.getquill.testContext.unquote

class ApplyIntermediateMapSpec extends Spec {

  "avoids applying the intermmediate map after a groupBy" - {
    "flatMap" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y._1).flatMap(s => qr2.filter(z => z.s == s))
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
    "filter" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y._1).filter(s => s == "s")
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
    "map" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y._1).map(s => s)
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
    "sortBy" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y._1).sortBy(s => s)
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
    "take" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y._1).take(1)
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
    "drop" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y._1).drop(1)
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
    "identity map" in {
      val q = quote {
        qr1.groupBy(t => t.i).map(y => y)
      }
      ApplyIntermediateMap(q.ast) mustEqual q.ast
    }
  }

  "applies intermediate map" - {
    "flatMap" in {
      val q = quote {
        qr1.map(y => y.s).flatMap(s => qr2.filter(z => z.s == s))
      }
      val n = quote {
        qr1.flatMap(y => qr2.filter(z => z.s == y.s))
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "filter" in {
      val q = quote {
        qr1.map(y => y.s).filter(s => s == "s")
      }
      val n = quote {
        qr1.filter(y => y.s == "s").map(y => y.s)
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "map" in {
      val q = quote {
        qr1.map(y => y.s).map(s => s)
      }
      val n = quote {
        qr1.map(y => y.s)
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "sortBy" in {
      val q = quote {
        qr1.map(y => y.s).sortBy(s => s)
      }
      val n = quote {
        qr1.sortBy(y => y.s).map(y => y.s)
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "identity map" in {
      val q = quote {
        qr1.sortBy(y => y.s).map(y => y)
      }
      val n = quote {
        qr1.sortBy(y => y.s)
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "distinct" in {
      val q = quote {
        query[TestEntity].map(i => (i.i, i.l)).distinct.map(x => (x._1, x._2))
      }
      val n = quote {
        query[TestEntity].map(i => (i.i, i.l)).distinct
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "take" in {
      val q = quote {
        qr1.map(y => y.s).take(1)
      }
      val n = quote {
        qr1.take(1).map(y => y.s)
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
    "drop" in {
      val q = quote {
        qr1.map(y => y.s).drop(1)
      }
      val n = quote {
        qr1.drop(1).map(y => y.s)
      }
      ApplyIntermediateMap(q.ast) mustEqual n.ast
    }
  }
}
