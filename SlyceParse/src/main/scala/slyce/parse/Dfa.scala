package slyce.parse

import scala.annotation.tailrec
import scala.util.Try

import scalaz.\/
import scalaz.\/-
import scalaz.-\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps

import slyce.common.helpers._
import slyce.parse.{architecture => arch}

final case class Dfa[+Tok <: Dfa.Token](
    initialState: Dfa.State[Tok],
    eofTok: Tok,
) extends arch.Lexer[String, List[String], Tok] {

  override def apply(str: String): List[String] \/ List[Tok] = {
    @tailrec
    def loop(
        state: Dfa.State[Tok],
        startPos: Dfa.Token.Pos,
        currentPos: Dfa.Token.Pos,
        remaining: List[Char],
        current: List[Char],
        past: List[Char],
        toks: List[Tok],
        yields: Option[(Dfa.Token.Pos, List[Char], Dfa.State.Yields[Tok])],
    ): List[String] \/ List[Tok] = {
      def calcToks(str: String, yields: List[Dfa.State.Yields.Yield[Tok]]): List[String] \/ List[Tok] = {
        def calcYield(y: Dfa.State.Yields.Yield[Tok]): List[String] \/ Tok = {
          def subStr(
              pos: Dfa.Token.Pos,
              s: String,
              trim: (Int, Int),
          ): List[String] \/ (Dfa.Token.Pos, Dfa.Token.Pos, String) = {
            def calcIdx(i: Int): Int =
              (i >= 0).fold(i, s.length + 1 + i)

            val start = calcIdx(trim._1)
            val end = calcIdx(trim._2)
            val strs =
              for {
                before <- Try(s.substring(0, start))
                in <- Try(s.substring(start, end))
              } yield (before, in)

            strs.toOption match {
              case None =>
                List(s"Unable to make Token @ ${startPos.pos}").left
              case Some((before, in)) =>
                val startPos = before.toList.foldLeft(pos)(_.onChar(_))
                val endPos = in.toList.foldLeft(startPos)(_.onChar(_))
                (startPos, endPos, in).right
            }
          }

          // NOTE : Removed the ability to be able to have span, and then pass text within span.
          //      : Seemed overkill. Possibly bring this back in the future.
          subStr(startPos, str, y.spanRange).map {
            case (p1, p2, r) =>
              y.tokF(r, Dfa.Token.Span(p1, p2))
          }
        }

        yields.map(calcYield).traverseErrs
      }

      remaining match {
        case Nil =>
          current match {
            case Nil =>
              (eofTok :: toks).reverse.right
            case _ =>
              yields match {
                case None =>
                  List("Unexpected EOF").left
                case Some((pos, chars, Dfa.State.Yields(to, yields))) =>
                  calcToks(chars.reverse.mkString, yields) match {
                    case err @ -\/(_) =>
                      err
                    case \/-(nToks) =>
                      loop(
                        state = to,
                        remaining = remaining.reverse_:::(past),
                        current = Nil,
                        startPos = pos,
                        currentPos = pos,
                        past = Nil,
                        toks = nToks ::: toks,
                        yields = None,
                      )
                  }
              }
          }
        case c :: rem =>
          state(c) match {
            case None =>
              yields match {
                case None =>
                  List(s"Unexpected ${c.unesc} @ ${currentPos.pos}").left
                case Some((pos, chars, Dfa.State.Yields(to, yields))) =>
                  calcToks(chars.reverse.mkString, yields) match {
                    case err @ -\/(_) =>
                      err
                    case \/-(nToks) =>
                      loop(
                        state = to,
                        remaining = remaining.reverse_:::(past),
                        current = Nil,
                        startPos = pos,
                        currentPos = pos,
                        past = Nil,
                        toks = nToks ::: toks,
                        yields = None,
                      )
                  }
              }
            case Some(newState) =>
              val nPos = currentPos.onChar(c)
              newState.yields match {
                case None =>
                  loop(
                    state = newState,
                    remaining = rem,
                    current = c :: current,
                    startPos = startPos,
                    currentPos = nPos,
                    past = c :: past,
                    toks = toks,
                    yields = yields,
                  )
                case yields @ Some(Dfa.State.Yields(to, _)) =>
                  val cur = c :: current

                  loop(
                    state = newState,
                    remaining = rem,
                    current = cur,
                    startPos = startPos,
                    currentPos = nPos,
                    past = Nil,
                    toks = toks,
                    yields = yields.map((nPos, cur, _)),
                  )
              }
          }
      }
    }

    loop(
      state = initialState,
      remaining = str.toList,
      current = Nil,
      startPos = Dfa.Token.Pos._0,
      currentPos = Dfa.Token.Pos._0,
      past = Nil,
      toks = Nil,
      yields = None,
    )
  }

}

object Dfa {

  trait Token
  object Token {

    trait HasSpan {
      def span: Span
    }

    final case class Span(
        start: Pos,
        stop: Pos,
    )

    final case class Pos(
        abs: Int,
        line: Int,
        inLine: Int,
    ) {

      def onChar(c: Char): Pos =
        c match {
          case '\n' =>
            Pos(abs + 1, line + 1, 0)
          case _ =>
            Pos(abs + 1, line, inLine + 1)
        }

      def pos: String =
        s"$line:$inLine"

    }

    object Pos {

      val _0: Pos = Pos(0, 0, 0)

    }

  }

  final case class State[+Tok](
      id: Int,
      transitions: Map[Char, Option[Lazy[State[Tok]]]],
      elseTransition: Option[Lazy[State[Tok]]],
      yields: Option[State.Yields[Tok]],
  ) {

    def apply(c: Char): Option[State[Tok]] =
      transitions.getOrElse(c, elseTransition).map(_.value)

  }

  object State {

    final class Yields[+Tok](val to: Lazy[State[Tok]], val yields: List[Yields.Yield[Tok]])

    object Yields {

      final case class Yield[+Tok](
          tokF: (String, Token.Span) => Tok,
          spanRange: (Int, Int),
      )

      def apply[Tok](to: => State[Tok])(yields: Yield[Tok]*): Yields[Tok] =
        new Yields(Lazy(to), yields.toList)

      def unapply[Tok](arg: Yields[Tok]): Option[(State[Tok], List[Yield[Tok]])] =
        (arg.to.value, arg.yields).some

    }

  }

}
