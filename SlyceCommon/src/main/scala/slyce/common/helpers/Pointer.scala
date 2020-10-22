package slyce.common.helpers

import scalaz.Scalaz.ToBooleanOpsFromBoolean

final class Pointer[V] private (v: V, s: Boolean) {
  private var set = s
  private var value: V = v
}

object Pointer {

  def apply[V](v: V): Pointer[V] =
    new Pointer(v, true)

  // This could possibly use some tweaking,
  // but it seems like the safest way to get the desired result at the moment
  def withSelf[V, R](vf: Pointer[V] => Pointer[V]): Pointer[V] = {
    val self = new Pointer[V](null.asInstanceOf[V], false)
    val res = vf(self)
    self.value = res.value
    self.set = true
    self
  }

  def unapply[V](arg: Pointer[V]): Option[V] =
    arg.set.option(arg.value)

}
