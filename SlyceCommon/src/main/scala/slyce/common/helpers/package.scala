package slyce.common

import scala.annotation.tailrec

import scalaz.\/
import scalaz.-\/
import scalaz.\/-
import scalaz.Scalaz.ToEitherOps

package object helpers {

  implicit class TraverseOps[E, R](obj: List[List[E] \/ R]) {

    def traverseErrs: List[E] \/ List[R] =
      obj.foldLeft(Nil.right: List[E] \/ List[R]) {
        case (\/-(res), \/-(todo)) =>
          (todo :: res).right
        case (\/-(_), -\/(todo)) =>
          todo.left
        case (res @ -\/(_), \/-(_)) =>
          res
        case (-\/(res), -\/(todo)) =>
          (res ::: todo).left
      }

  }

  @tailrec
  def findAll[A](unseen: Set[A], seen: Set[A] = Set.empty)(f: A => Set[A]): Set[A] =
    if (unseen.isEmpty)
      seen
    else {
      val nowSeen = seen | unseen
      findAll(unseen.flatMap(f) &~ nowSeen, nowSeen)(f)
    }

  def split[A, B](list: List[A \/ B]): (List[A], List[B]) = {
    @tailrec
    def loop(todo: List[A \/ B], a: List[A], b: List[B]): (List[A], List[B]) =
      todo match {
        case Nil =>
          (a.reverse, b.reverse)
        case head :: tail =>
          head match {
            case -\/(a2) =>
              loop(tail, a2 :: a, b)
            case \/-(b2) =>
              loop(tail, a, b2 :: b)
          }
      }

    loop(list, Nil, Nil)
  }

}
