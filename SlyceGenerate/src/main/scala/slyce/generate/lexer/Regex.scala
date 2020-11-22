package slyce.generate.lexer

import scalaz.NonEmptyList
import scalaz.Scalaz.ToOptionIdOps

import klib.CharStringOps._

sealed trait Regex {

  def canPassThrough: Boolean =
    this match {
      case _: Regex.CharClass =>
        false
      case Regex.Sequence(seq) =>
        seq.foldLeft(true) {
          case (true, r) =>
            r.canPassThrough
          case _ =>
            false
        }
      case Regex.Group(seqs) =>
        seqs.list.toList.exists(_.canPassThrough)
      case Regex.Repeat(_, 0, _) =>
        true
      case Regex.Repeat(reg, _, _) =>
        reg.canPassThrough
    }

}

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

    override def toString: String =
      s"${this.getClass.getSimpleName}(${this.chars.toList.sorted.map(_.unesc).mkString(", ")})"

  }

  object CharClass {

    final case class Inclusive(chars: Set[Char]) extends CharClass {

      def |(other: Inclusive): Inclusive =
        Inclusive(this.chars | other.chars)

    }
    object Inclusive {

      def apply(chars: Char*): Inclusive =
        Inclusive(chars.toSet)

      val az: Inclusive = Inclusive('a'.to('z').toSet)
      val AZ: Inclusive = Inclusive('A'.to('Z').toSet)
      val d: Inclusive = Inclusive('0'.to('9').toSet)

    }

    final case class Exclusive(chars: Set[Char]) extends CharClass
    object Exclusive {
      def apply(chars: Char*): Exclusive =
        Exclusive(chars.toSet)
    }

  }

  final case class Sequence(seq: List[Regex]) extends Regex
  object Sequence {
    def apply(regs: Regex*): Sequence =
      Sequence(regs.toList)
  }

  final case class Group(seqs: NonEmptyList[Sequence]) extends Regex
  object Group {
    def apply(seq0: Sequence, seqN: Sequence*): Group =
      Group(NonEmptyList(seq0, seqN: _*))
  }

  final case class Repeat(reg: Regex, min: Int, max: Option[Int]) extends Regex
  object Repeat {

    def ?(reg: Regex): Repeat =
      Repeat(reg, 0, 1.some)

    def *(reg: Regex): Repeat =
      Repeat(reg, 0, None)

    def +(reg: Regex): Repeat =
      Repeat(reg, 1, None)

  }

}
