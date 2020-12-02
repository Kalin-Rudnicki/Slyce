package slyce.common.helpers

import scalaz.-\/
import scalaz.\/
import scalaz.\/-
import scalaz.syntax.either._
import shapeless.::
import shapeless.HList
import shapeless.HNil

case class Applier[E, T <: HList] private (value: List[E] \/ T) {

  def apply[E2 >: E, A](add: List[E2] \/ A): Applier[E2, A :: T] =
    value match {
      case left @ -\/(l1) =>
        add match {
          case -\/(l2) =>
            Applier((l1 ::: l2).left)
          case \/-(_) =>
            Applier(left)
        }
      case \/-(r1) =>
        add match {
          case left @ -\/(_) =>
            Applier(left)
          case \/-(r2) =>
            Applier((r2 :: r1).right)
        }
    }

}

object Applier {

  val init: Applier[Nothing, HNil] = Applier(HNil.right)

}
