package slyce.generation.raw.lexer.nfa

import scalaz.NonEmptyList

import slyce.generation.GenerationMessage.??
import slyce.generation.raw.lexer.nfa.Regex.CharClass._
import slyce.generation.raw.lexer.nfa.Regex._
import slyce.generation.raw.lexer.nfa.Regex.{CharClass => CC}

object RegexImplicits {

  // =====| Converters |=====

  // TODO (KR) : Reversed properly?
  implicit def stringToSequence(str: String): Sequence =
    Sequence(str.toList.reverse.map[CC](cc => cc))

  implicit def charToCharClass(c: Char): CC =
    CC.only(c)

  implicit def charToCharClassOps(c: Char): CharClassOps =
    charClassToCharClassOps(charToCharClass(c))

  implicit def charClassToCharClassOps(cc: CC): CharClassOps =
    new CharClassOps(cc)

  implicit def regexToRegexOps(reg: Regex): RegexOps =
    new RegexOps(reg)

  // =====| Helpers |=====

  class CharClassOps(cc: Regex.CharClass) {

    // =====|  |=====

    def unary_! : CC =
      flip

    def :|(other: CC): CC =
      union(other)

    def :&(other: CC): CC =
      intersection(other)

    def :-(other: CC): CC =
      difference(other)

    // =====|  |=====

    def flip: CC =
      cc match {
        case Only(chars) =>
          Except(chars)
        case Except(chars) =>
          Only(chars)
      }

    def union(other: CC): CC =
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

    def intersection(other: CC): CC =
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

    def difference(other: CC): CC =
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

  class RegexOps(reg: Regex) {

    // =====|  |=====

    def *(r: (Int, Option[Int])): Regex =
      repeat(r)

    def =>>(next: Regex): Regex =
      followedBy(next)

    def <|>(other: Regex): Regex =
      or(other)

    // =====|  |=====

    def repeat(repeat: (Int, Option[Int])): Regex =
      repeat match {
        case (min, None) =>
          Repeat.Infinite(min, reg)
        case (min, Some(max)) =>
          Repeat.Between(min, max, reg)
      }

    def followedBy(next: Regex): Regex =
      reg match {
        case g: Group =>
          Sequence(next :: g :: Nil)
        case Sequence(list) =>
          Sequence(next :: list)
        case cc: CC =>
          Sequence(next :: cc :: Nil)
        case repeat: Repeat =>
          Sequence(next :: repeat :: Nil)
      }

    def or(other: Regex): Regex = {
      val orSeq: Sequence =
        other match {
          case g: Group =>
            Sequence(g :: Nil)
          case s: Sequence =>
            s
          case cc: CC =>
            Sequence(cc :: Nil)
          case repeat: Repeat =>
            Sequence(repeat :: Nil)
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
              Sequence(cc :: Nil)
            )
          )
        case repeat: Repeat =>
          Group(
            NonEmptyList(
              orSeq,
              Sequence(repeat :: Nil)
            )
          )
      }
    }

  }

}
