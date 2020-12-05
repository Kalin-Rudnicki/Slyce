package klib

import scalaz.Scalaz.ToOptionIdOps

class Timer {

  private var startTimes: List[Long] = Nil

  def push: Long = {
    val now = System.currentTimeMillis
    startTimes = now :: startTimes
    now
  }

  def elapsed: Option[Long] =
    startTimes.headOption.map(System.currentTimeMillis - _)

  def pop: Option[Long] =
    startTimes match {
      case Nil =>
        None
      case head :: tail =>
        startTimes = tail
        (System.currentTimeMillis - head).some
    }

  def clear: Unit =
    startTimes = Nil

  def time[C](c: => C): (Long, C) = {
    val start = System.currentTimeMillis
    val res = c
    val end = System.currentTimeMillis
    (end - start, res)
  }

}

object Timer {

  def time[C](c: => C): (Long, C) =
    (new Timer).time(c)

  def dumpTime[C](name: String)(c: => C): C = {
    val (t, r) = time(c)
    println(s"$name: ${sFormat(t)}")
    r
  }

  def sFormat(elapsed: Long): String =
    s"${elapsed.toDouble / 1000}s"

}
