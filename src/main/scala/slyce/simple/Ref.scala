package slyce.simple

class Ref[A] private () {
  private var _value: Option[A] = None

  def value: A =
    _value.get

  def set(value: A): Unit =
    _value = Some(value)

}

object Ref {

  def empty[A]: Ref[A] =
    new Ref

  def of[A](value: A): Ref[A] = {
    val ref: Ref[A] = empty
    ref.set(value)
    ref
  }

}
