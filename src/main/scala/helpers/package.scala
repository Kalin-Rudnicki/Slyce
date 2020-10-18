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

    def unesc: String =
      char match {
        case '\n' =>
          "\\n"
        case '\\' =>
          "\\\\"
        case '\t' =>
          "\\t"
        case c =>
          c.toString
      }

    def unescape: String =
      s"'${char.unesc}'"

  }

  implicit class StringOps(string: String) {

    def unesc: String =
      '\"' + string.map(_.unesc).mkString + '\"'
  }

  @tailrec
  def findAll[A](unseen: Set[A], seen: Set[A] = Set())(f: A => Set[A]): Set[A] =
    if (unseen.isEmpty)
      seen
    else {
      val nowSeen = seen | unseen
      findAll(unseen.flatMap(f) &~ nowSeen, nowSeen)(f)
    }

  def idtStrs(strs: String*)(implicit idt: String): List[String] =
    strs.toList.map(s => s"$idt$s")

  def idtLists(strLists: List[String]*)(implicit idt: String): List[String] =
    strLists.toList.flatMap(idtStrs(_: _*))

}
