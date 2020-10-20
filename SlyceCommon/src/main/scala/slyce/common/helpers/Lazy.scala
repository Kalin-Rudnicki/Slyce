package slyce.common.helpers

class Lazy[+V] private (v: => V) {

  lazy val value: V = v

}

object Lazy {

  def apply[V](v: => V): Lazy[V] =
    new Lazy(v)

}
