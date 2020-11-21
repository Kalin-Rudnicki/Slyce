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

  implicit class CharOps(char: Char) {

    def unesc: String =
      unesc("'")

    def unesc(_1: String, _2: Option[String] = None): String = {
      val left = _1
      val right = _2.getOrElse(_1)
      val charText = char match {
        case '\n' =>
          "\\n"
        case '\\' =>
          "\\\\"
        case '\t' =>
          "\\t"
        case c =>
          c.toString
      }
      s"$left$charText$right"
    }

  }

  implicit class StringOps(string: String) {

    def unesc: String =
      unesc("\"")

    def unesc(_1: String, _2: Option[String] = None, f: String => String = s => s): String = {
      // `f` serves the purpose of if you want to wrap in quotes, but also have other text inside the quotes
      val left = _1
      val right = _2.getOrElse(_1)
      val strText = f(string.map(_.unesc("")).mkString)
      s"$left$strText$right"
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
