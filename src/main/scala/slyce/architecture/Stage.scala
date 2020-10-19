package slyce.architecture

import scalaz.\/
import scalaz.syntax.either._

trait Stage[-I, +E, +O] {

  def apply(input: I): E \/ O

  def pass_input[I2 <: I]: Stage[I2, E, (I2, O)] = { input =>
    apply(input).map((input, _))
  }

  def preMap[I2](f: I2 => I): Stage[I2, E, O] =
    input => apply(f(input))

  def map[O2](f: O => O2): Stage[I, E, O2] =
    apply(_).map(f)

  def mapError[E2](f: E => E2): Stage[I, E2, O] =
    apply(_).leftMap(f)

  def biMap[E2, O2](fE: E => E2)(fO: O => O2): Stage[I, E2, O2] =
    apply(_).bimap(fE, fO)

  def >+>[E2 >: E, O2](that: Stage[O, E2, O2]): Stage[I, E2, O2] =
    apply(_).flatMap(that.apply)

  def >>+>[I2 <: I, E2 >: E, O2](that: Stage[(I2, O), E2, O2]): Stage[I2, E2, O2] = { input =>
    apply(input).fold(
      l => l.left,
      that(input, _),
    )
  }

  def <+<[E2 >: E, O2](that: Stage[E2, Nothing, O2]): Stage[I, O2, O] =
    apply(_).fold(
      that(_).swap,
      r => r.right,
    )

  def <<+<[I2 <: I, E2](that: Stage[(I2, E), Nothing, E2]): Stage[I2, E2, O] = { input =>
    apply(input).fold(
      that(input, _).swap,
      r => r.right,
    )
  }

}
