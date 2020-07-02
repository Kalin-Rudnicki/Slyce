package slyce.architecture

import scalaz.\/

trait Stage[-I, +E, +O] {

  def apply(input: I): E \/ O

  def preMap[I2](f: I2 => I): Stage[I2, E, O] =
    input => apply(f(input))

  def map[O2](f: O => O2): Stage[I, E, O2] =
    apply(_).map(f)

  def mapError[E2](f: E => E2): Stage[I, E2, O] =
    apply(_).bimap(f, a => a)

  def biMap[E2, O2](fE: E => E2)(fO: O => O2): Stage[I, E2, O2] =
    apply(_).bimap(fE, fO)

  def >+>[E2 >: E, O2](that: Stage[O, E2, O2]): Stage[I, E2, O2] =
    apply(_).flatMap(that.apply)

}
