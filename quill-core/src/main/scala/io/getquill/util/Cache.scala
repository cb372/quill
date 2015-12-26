package io.getquill.util

import java.lang.System.{ currentTimeMillis => now }
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConverters._
import java.io.Closeable

class Cache[K, V <: Closeable] {

  private case class Entry(value: Option[V], expiration: Long)

  private val cache = new ConcurrentHashMap[K, Entry]().asScala
  private val scheduler = Executors.newScheduledThreadPool(1)

  private val evict = new Runnable {
    override def run =
      for ((key, Entry(value, expiration)) <- cache)
        if (expiration <= now) {
          value.map(_.close)
          cache -= key
        }
  }

  scheduler.scheduleAtFixedRate(evict, 2, 2, TimeUnit.SECONDS)

  def getOrElseUpdate(key: K, value: => Option[V], ttl: Duration): Option[V] = {
    val expiration = now + ttl.toMillis
    cache.get(key) match {
      case Some(entry) =>
        cache.put(key, entry.copy(expiration = expiration))
        entry.value
      case None =>
        val v = value
        cache.put(key, Entry(v, expiration))
        v
    }
  }
}