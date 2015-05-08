package util

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable.HashMap

class IdentityGenerator(start: Int = 0) {
  private val identities = new HashMap[Any, Int]
  private val counter = new AtomicInteger(start)

  def apply(x: Any) = identities.getOrElseUpdate(x, counter.incrementAndGet())
}
