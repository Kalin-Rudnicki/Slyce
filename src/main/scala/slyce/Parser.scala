package slyce

import scala.annotation.tailrec
import scala.language.implicitConversions

import scalaz.-\/
import scalaz.\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/-

object Parser {

  private[slyce] def parseIncludes(chars: List[Char]): String \/ (List[Char], List[String]) = {
    @tailrec
    def loop2(chars: List[Char], saved: List[Char]): String \/ (List[Char], String) =
      if (saved.isEmpty)
        chars match {
          case Nil =>
            "Unexpected EOF".left
          case '\n' :: _ =>
            "Unexpected '\\n'".left
          case c :: tail =>
            loop2(tail, c :: saved)
        }
      else
        chars match {
          case Nil =>
            (Nil, saved.reverse.mkString).right
          case '\n' :: tail =>
            (tail, saved.reverse.mkString).right
          case c :: tail =>
            loop2(tail, c :: saved)
        }

    @tailrec
    def loop(chars: List[Char], saved: List[String]): String \/ (List[Char], List[String]) =
      chars match {
        case '\n' :: tail =>
          loop(tail, saved)
        case '@' :: 'i' :: 'n' :: 'c' :: 'l' :: 'u' :: 'd' :: 'e' :: ' ' :: tail =>
          loop2(tail, Nil) match {
            case err: -\/[String] =>
              err
            case \/-((tail2, str)) =>
              loop(tail2, str :: saved)
          }
        case _ =>
          (chars, saved.reverse).right
      }

    loop(chars, Nil)
  }

  def parse(chars: List[Char]): Any =
    for {
      tmp1 <- parseIncludes(chars)
      (chars1, includes) = tmp1
    } yield includes

}
