package slyce.simple

import scala.annotation.tailrec

import scalaz.Scalaz.ToEitherOps
import scalaz.\/

case class Tokenizer[T] private (fs: List[Tokenizer.F[T]]) {

  def tokenize(chars: List[Char]): String \/ List[T] = {
    @tailrec
    def loop(list: List[Tokenizer.F[T]]): String \/ List[T] = {
      list match {
        case Nil =>
          "Unable to parse chars".left
        case h :: t =>
          try {
            val (remaining, toks, ref) = h(chars)
            try {
              ref.value.tokenize(remaining).map(toks.reverse ::: _)
            } catch {
              case _: NoSuchElementException =>
                "Unset ref".left
            }
          } catch {
            case _: MatchError =>
              loop(t)
          }
      }
    }

    chars match {
      case Nil =>
        Nil.right
      case _ =>
        loop(fs)
    }
  }

  def passivelyAllow(allowed: Set[Char]): Tokenizer[T] = {
    val ref: Ref[Tokenizer[T]] = Ref.empty
    val tok: Tokenizer[T] =
      new Tokenizer[T](
        (
            (chars: List[Char]) =>
              chars match {
                case h :: tail if allowed.contains(h) =>
                  (tail, Nil, ref)
              }
        ) ::
          fs
      )
    ref.set(tok)
    tok
  }

  def withWs: Tokenizer[T] =
    passivelyAllow(Set(' ', '\t'))

  def withNl: Tokenizer[T] =
    passivelyAllow(Set('\n'))

  def withNlWs: Tokenizer[T] =
    passivelyAllow(Set(' ', '\t', '\n'))

}

object Tokenizer {

  type F[T] = List[Char] => (List[Char], List[T], Ref[Tokenizer[T]])

  def apply[T](fs: (List[Char] => (List[Char], List[T], Ref[Tokenizer[T]]))*): Tokenizer[T] =
    new Tokenizer[T](fs.toList)

}
