package slyce.implementations.parsing

import scala.annotation.tailrec

import helpers._
import scalaz.\/
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps

final case class Dfa[+Tok <: Dfa.Token](initialState: Dfa.State[Tok]) {

  def parse(str: String): List[String] \/ List[Tok] = {
    @tailrec
    def loop(
        state: Dfa.State[Tok],
        remaining: List[Char],
        current: List[Char],
        past: List[Char],
        toks: List[Tok],
        tok: Option[(Dfa.State[Tok], List[Tok])]
    ): List[String] \/ List[Tok] =
      remaining match {
        case Nil =>
          current match {
            case Nil =>
              toks.reverse.right
            case _ =>
              tok match {
                case None =>
                  List("Unexpected EOF").left
                case Some((to, found)) =>
                  loop(
                    state = to,
                    remaining = remaining.reverse_:::(past),
                    current = Nil,
                    past = Nil,
                    toks = found ::: toks,
                    tok = None
                  )
              }
          }
        case c :: rem =>
          state(c) match {
            case None =>
              tok match {
                case None =>
                  List(s"Unexpected ${c.unescape}").left
                case Some((to, found)) =>
                  loop(
                    state = to,
                    remaining = remaining.reverse_:::(past),
                    current = Nil,
                    past = Nil,
                    toks = found ::: toks,
                    tok = None
                  )
              }
            case Some(to) =>
              to.yields match {
                case None =>
                  loop(
                    state = to,
                    remaining = rem,
                    current = c :: current,
                    past = c :: past,
                    toks = toks,
                    tok = tok
                  )
                case Some(Dfa.State.Yield(to2, yields)) =>
                  val cur = c :: current
                  loop(
                    state = to,
                    remaining = rem,
                    current = cur,
                    past = Nil,
                    toks = toks,
                    tok = (to2, yields(cur.reverse.mkString).toList).some
                  )
              }
          }
      }

    loop(
      state = initialState,
      remaining = str.toCharArray.toList,
      current = Nil,
      past = Nil,
      toks = Nil,
      tok = None
    )
  }

}

object Dfa {

  trait Token {

    def name: String = this.getClass.getSimpleName

    def text: String

    override def toString: String =
      s"""$name("${text.map(_.unesc).mkString}")"""

  }

  final case class State[+Tok](
      id: Int,
      transitions: Map[Char, Option[Lazy[State[Tok]]]],
      elseTransition: Option[Lazy[State[Tok]]],
      yields: Option[State.Yield[Tok]]
  ) {

    def apply(c: Char): Option[State[Tok]] =
      transitions.getOrElse(c, elseTransition).map(_.value)

  }

  object State {

    final class Yield[+Tok](_to: => State[Tok], val yields: String => Option[Tok]) {
      lazy val to: State[Tok] = _to
    }

    object Yield {

      def apply[Tok](to: State[Tok])(yields: String => Option[Tok]): Yield[Tok] =
        new Yield(to, yields)

      def unapply[Tok](arg: Yield[Tok]): Option[(State[Tok], String => Option[Tok])] =
        (arg.to, arg.yields).some

    }

  }

}
