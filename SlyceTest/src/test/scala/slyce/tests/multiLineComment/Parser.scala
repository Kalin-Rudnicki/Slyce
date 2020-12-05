// DO NOT EDIT : Automatically generated by Slyce v0.1.0 @ 11/22/2020
package slyce.tests.multiLineComment

import scala.annotation.tailrec

import scalaz.\/
import scalaz.-\/
import scalaz.\/-
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToOptionIdOps

import slyce.common.helpers._
import slyce.parse._
import slyce.parse.{architecture => arch}

object Data {

  sealed trait Token extends Dfa.Token
  sealed trait HasSpanToken extends Token with Dfa.Token.HasSpan
  object Token {
    case object EOF extends Token
    final case class comment(text: String, span: Dfa.Token.Span) extends HasSpanToken
    final case class multiLineComment(text: String, span: Dfa.Token.Span) extends HasSpanToken
  }
  object HasSpanToken {
    def unapply(arg: HasSpanToken): Option[Dfa.Token.Span] = arg.span.some
  }

  sealed trait NonTerminal
  object NonTerminal {

    sealed trait __Start extends NonTerminal
    object __Start {

      final case class _1(
          _1: NonTerminal.Comments,
          _2: Token.EOF.type,
      ) extends __Start

    }

    sealed trait Comment extends NonTerminal
    object Comment {

      final case class _1(
          _1: Token.comment,
      ) extends Comment

      final case class _2(
          _1: Token.multiLineComment,
      ) extends Comment

    }

    sealed trait Comments extends NonTerminal {

      def toList: List[NonTerminal.Comment] = {
        @tailrec
        def loop(unseen: Comments, seen: List[NonTerminal.Comment]): List[NonTerminal.Comment] =
          unseen match {
            case Comments._1(n, tail) =>
              loop(tail, n :: seen)
            case Comments._2 =>
              seen.reverse
          }

        loop(this, Nil)
      }

    }
    object Comments {

      final case class _1(
          _1: NonTerminal.Comment,
          _2: NonTerminal.Comments,
      ) extends Comments

      case object _2 extends Comments

    }

  }

}

object Parser extends arch.Parser[String, List[String], Data.NonTerminal.Comments] {
  import Data._

  private val lexer: Dfa[Token] = ???
  private val grammar: Builder[Token, NonTerminal, NonTerminal.Comments]#StateMachine = ???

  override def apply(input: String): List[String] \/ NonTerminal.Comments =
    arch.Parser(lexer, grammar)(input)

}
