package slyce.implementations.generation.lexer

import scalaz.NonEmptyList

sealed trait Regex

object Regex {

  sealed trait CharClass extends Regex {

    def chars: Set[Char]

    def contains(c: Char): Boolean =
      this match {
        case CharClass.Inclusive(chars) =>
          chars.contains(c)
        case CharClass.Exclusive(chars) =>
          !chars.contains(c)
      }

  }

  object CharClass {
    final case class Inclusive(chars: Set[Char]) extends CharClass
    final case class Exclusive(chars: Set[Char]) extends CharClass
  }

  final case class Sequence(seq: List[Regex]) extends Regex

  final case class Group(seqs: NonEmptyList[Sequence]) extends Regex

  final case class Repeat(reg: Regex, min: Int, max: Option[Int]) extends Regex

}
