import scala.annotation.tailrec

import scalaz.-\/
import scalaz.\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/-

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

  implicit class CharOps(char: Char) {

    def unescape: String = {
      val s =
        char match {
          case '\n' =>
            "\\n"
          case '\t' =>
            "\\t"
          case c =>
            c.toString
        }

      s"'$s'"
    }

  }

  @tailrec
  def findAll[A](unseen: Set[A], seen: Set[A] = Set())(f: A => Set[A]): Set[A] =
    if (unseen.isEmpty)
      seen
    else {
      val nowSeen = seen | unseen
      findAll(unseen.flatMap(f) &~ nowSeen, nowSeen)(f)
    }

}
