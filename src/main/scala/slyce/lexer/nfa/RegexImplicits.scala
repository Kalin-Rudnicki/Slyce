package slyce.lexer.nfa

import scalaz.NonEmptyList

import slyce.lexer.nfa.Regex.CharClass._
import slyce.lexer.nfa.Regex._
import slyce.lexer.nfa.Regex.{CharClass => CC}

object RegexImplicits {

  implicit class CharClassOps(cc: Regex.CharClass) {

    def unary_! : CC =
      cc match {
        case Only(chars) =>
          Except(chars)
        case Except(chars) =>
          Only(chars)
      }

    def |(other: CC): CC =
      cc match {
        case Only(mChars) =>
          other match {
            case Only(oChars) =>
              Only(mChars | oChars)
            case Except(oChars) =>
              Except(oChars &~ mChars)
          }
        case Except(mChars) =>
          other match {
            case Only(oChars) =>
              Except(mChars &~ oChars)
            case Except(oChars) =>
              Except(mChars & oChars)
          }
      }

    def &(other: CC): CC =
      cc match {
        case Only(mChars) =>
          other match {
            case Only(oChars) =>
              Only(mChars & oChars)
            case Except(oChars) =>
              Only(mChars &~ oChars)
          }
        case Except(mChars) =>
          other match {
            case Only(oChars) =>
              Only(oChars &~ mChars)
            case Except(oChars) =>
              Except(oChars | mChars)
          }
      }

    def -(other: CC): CC =
      cc match {
        case Only(mChars) =>
          other match {
            case Only(oChars) =>
              Only(mChars &~ oChars)
            case Except(oChars) =>
              Only(mChars & oChars)
          }
        case Except(mChars) =>
          other match {
            case Only(oChars) =>
              Except(mChars | oChars)
            case Except(oChars) =>
              Only(oChars &~ mChars)
          }
      }

  }

  implicit class RegexOps(reg: Regex) {

    def *(repeat: (Int, Option[Int])): Regex =
      repeat match {
        case (min, None) =>
          Repeat.Infinite(min, reg)
        case (min, Some(max)) =>
          Repeat.Between(min, max, reg)
      }

    def >>(next: Regex): Regex =
      reg match {
        case g: Group =>
          Sequence(NonEmptyList(next, g))
        case Sequence(list) =>
          Sequence(NonEmptyList.nel(next, list.list))
        case cc: CC =>
          Sequence(NonEmptyList(next, cc))
        case repeat: Repeat =>
          Sequence(NonEmptyList(next, repeat))
      }

    def @|(or: Regex): Regex = {
      val orSeq: Sequence =
        or match {
          case g: Group =>
            Sequence(NonEmptyList(g))
          case s: Sequence =>
            s
          case cc: CC =>
            Sequence(NonEmptyList(cc))
          case repeat: Repeat =>
            Sequence(NonEmptyList(repeat))
        }

      reg match {
        case Group(options) =>
          Group(
            NonEmptyList.nel[Sequence](
              orSeq,
              options.list
            )
          )
        case s: Sequence =>
          Group(
            NonEmptyList(
              orSeq,
              s
            )
          )
        case cc: CC =>
          Group(
            NonEmptyList(
              orSeq,
              Sequence(NonEmptyList(cc))
            )
          )
        case repeat: Repeat =>
          Group(
            NonEmptyList(
              orSeq,
              Sequence(NonEmptyList(repeat))
            )
          )
      }
    }

  }

}
