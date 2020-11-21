package slyce.tests.argList

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
    object __ {

      def apply(str: String, span: Dfa.Token.Span): Token =
        str match {
          case "," =>
            `,`(span)
          case "(" =>
            `(`(span)
          case ")" =>
            `)`(span)
          case _ =>
            ???
        }

      final case class `,`(span: Dfa.Token.Span) extends HasSpanToken
      final case class `(`(span: Dfa.Token.Span) extends HasSpanToken
      final case class `)`(span: Dfa.Token.Span) extends HasSpanToken
    }
    final case class _var(text: String, span: Dfa.Token.Span) extends HasSpanToken
  }
  object HasSpanToken {
    def unapply(arg: HasSpanToken): Option[Dfa.Token.Span] = arg.span.some
  }

  sealed trait NonTerminal
  object NonTerminal {

    sealed trait __Start extends NonTerminal
    object __Start {

      final case class _1(
        _1: NonTerminal.List,
        _2: Token.EOF.type,
      ) extends __Start

    }

    sealed trait AnonList1 extends NonTerminal
    object AnonList1 {

      final case class _1(
        _1: Token._var,
        _2: NonTerminal.AnonList1_2,
      ) extends AnonList1

      case object _2 extends AnonList1

    }

    sealed trait AnonList1_2 extends NonTerminal
    object AnonList1_2 {

      final case class _1(
        _1: Token.__.`,`,
        _2: Token._var,
        _3: NonTerminal.AnonList1_2,
      ) extends AnonList1_2

      case object _2 extends AnonList1_2

    }

    sealed trait List extends NonTerminal
    object List {

      final case class _1(
        _1: Token.__.`(`,
        _2: NonTerminal.AnonList1,
        _3: Token.__.`)`,
      ) extends List

    }

  }

}

object Parser extends arch.Parser[String, List[String], Data.NonTerminal.List] {
  import Data._

  private val lexer: Dfa[Token] = {
    lazy val s0: Dfa.State[Token] =
      Dfa.State(
        id = 0,
        transitions = Map(
          0x9.toChar -> Some(Lazy(s1)), // '\t'
          0xA.toChar -> Some(Lazy(s1)), // '\n'
          0x20.toChar -> Some(Lazy(s1)), // ' '
          0x28.toChar -> Some(Lazy(s3)), // '('
          0x29.toChar -> Some(Lazy(s3)), // ')'
          0x2C.toChar -> Some(Lazy(s3)), // ','
          0x5F.toChar -> Some(Lazy(s2)), // '_'
          0x61.toChar -> Some(Lazy(s2)), // 'a'
          0x62.toChar -> Some(Lazy(s2)), // 'b'
          0x63.toChar -> Some(Lazy(s2)), // 'c'
          0x64.toChar -> Some(Lazy(s2)), // 'd'
          0x65.toChar -> Some(Lazy(s2)), // 'e'
          0x66.toChar -> Some(Lazy(s2)), // 'f'
          0x67.toChar -> Some(Lazy(s2)), // 'g'
          0x68.toChar -> Some(Lazy(s2)), // 'h'
          0x69.toChar -> Some(Lazy(s2)), // 'i'
          0x6A.toChar -> Some(Lazy(s2)), // 'j'
          0x6B.toChar -> Some(Lazy(s2)), // 'k'
          0x6C.toChar -> Some(Lazy(s2)), // 'l'
          0x6D.toChar -> Some(Lazy(s2)), // 'm'
          0x6E.toChar -> Some(Lazy(s2)), // 'n'
          0x6F.toChar -> Some(Lazy(s2)), // 'o'
          0x70.toChar -> Some(Lazy(s2)), // 'p'
          0x71.toChar -> Some(Lazy(s2)), // 'q'
          0x72.toChar -> Some(Lazy(s2)), // 'r'
          0x73.toChar -> Some(Lazy(s2)), // 's'
          0x74.toChar -> Some(Lazy(s2)), // 't'
          0x75.toChar -> Some(Lazy(s2)), // 'u'
          0x76.toChar -> Some(Lazy(s2)), // 'v'
          0x77.toChar -> Some(Lazy(s2)), // 'w'
          0x78.toChar -> Some(Lazy(s2)), // 'x'
          0x79.toChar -> Some(Lazy(s2)), // 'y'
          0x7A.toChar -> Some(Lazy(s2)), // 'z'
        ),
        elseTransition = None,
        yields = None,
      )
    lazy val s1: Dfa.State[Token] =
      Dfa.State(
        id = 1,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s2: Dfa.State[Token] =
      Dfa.State(
        id = 2,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s2)), // '0'
          0x31.toChar -> Some(Lazy(s2)), // '1'
          0x32.toChar -> Some(Lazy(s2)), // '2'
          0x33.toChar -> Some(Lazy(s2)), // '3'
          0x34.toChar -> Some(Lazy(s2)), // '4'
          0x35.toChar -> Some(Lazy(s2)), // '5'
          0x36.toChar -> Some(Lazy(s2)), // '6'
          0x37.toChar -> Some(Lazy(s2)), // '7'
          0x38.toChar -> Some(Lazy(s2)), // '8'
          0x39.toChar -> Some(Lazy(s2)), // '9'
          0x41.toChar -> Some(Lazy(s2)), // 'A'
          0x42.toChar -> Some(Lazy(s2)), // 'B'
          0x43.toChar -> Some(Lazy(s2)), // 'C'
          0x44.toChar -> Some(Lazy(s2)), // 'D'
          0x45.toChar -> Some(Lazy(s2)), // 'E'
          0x46.toChar -> Some(Lazy(s2)), // 'F'
          0x47.toChar -> Some(Lazy(s2)), // 'G'
          0x48.toChar -> Some(Lazy(s2)), // 'H'
          0x49.toChar -> Some(Lazy(s2)), // 'I'
          0x4A.toChar -> Some(Lazy(s2)), // 'J'
          0x4B.toChar -> Some(Lazy(s2)), // 'K'
          0x4C.toChar -> Some(Lazy(s2)), // 'L'
          0x4D.toChar -> Some(Lazy(s2)), // 'M'
          0x4E.toChar -> Some(Lazy(s2)), // 'N'
          0x4F.toChar -> Some(Lazy(s2)), // 'O'
          0x50.toChar -> Some(Lazy(s2)), // 'P'
          0x51.toChar -> Some(Lazy(s2)), // 'Q'
          0x52.toChar -> Some(Lazy(s2)), // 'R'
          0x53.toChar -> Some(Lazy(s2)), // 'S'
          0x54.toChar -> Some(Lazy(s2)), // 'T'
          0x55.toChar -> Some(Lazy(s2)), // 'U'
          0x56.toChar -> Some(Lazy(s2)), // 'V'
          0x57.toChar -> Some(Lazy(s2)), // 'W'
          0x58.toChar -> Some(Lazy(s2)), // 'X'
          0x59.toChar -> Some(Lazy(s2)), // 'Y'
          0x5A.toChar -> Some(Lazy(s2)), // 'Z'
          0x5F.toChar -> Some(Lazy(s2)), // '_'
          0x61.toChar -> Some(Lazy(s2)), // 'a'
          0x62.toChar -> Some(Lazy(s2)), // 'b'
          0x63.toChar -> Some(Lazy(s2)), // 'c'
          0x64.toChar -> Some(Lazy(s2)), // 'd'
          0x65.toChar -> Some(Lazy(s2)), // 'e'
          0x66.toChar -> Some(Lazy(s2)), // 'f'
          0x67.toChar -> Some(Lazy(s2)), // 'g'
          0x68.toChar -> Some(Lazy(s2)), // 'h'
          0x69.toChar -> Some(Lazy(s2)), // 'i'
          0x6A.toChar -> Some(Lazy(s2)), // 'j'
          0x6B.toChar -> Some(Lazy(s2)), // 'k'
          0x6C.toChar -> Some(Lazy(s2)), // 'l'
          0x6D.toChar -> Some(Lazy(s2)), // 'm'
          0x6E.toChar -> Some(Lazy(s2)), // 'n'
          0x6F.toChar -> Some(Lazy(s2)), // 'o'
          0x70.toChar -> Some(Lazy(s2)), // 'p'
          0x71.toChar -> Some(Lazy(s2)), // 'q'
          0x72.toChar -> Some(Lazy(s2)), // 'r'
          0x73.toChar -> Some(Lazy(s2)), // 's'
          0x74.toChar -> Some(Lazy(s2)), // 't'
          0x75.toChar -> Some(Lazy(s2)), // 'u'
          0x76.toChar -> Some(Lazy(s2)), // 'v'
          0x77.toChar -> Some(Lazy(s2)), // 'w'
          0x78.toChar -> Some(Lazy(s2)), // 'x'
          0x79.toChar -> Some(Lazy(s2)), // 'y'
          0x7A.toChar -> Some(Lazy(s2)), // 'z'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token._var.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s3: Dfa.State[Token] =
      Dfa.State(
        id = 3,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.__.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )

    Dfa(s0, Token.EOF)
  }

  private val grammar: Builder[Token, NonTerminal, NonTerminal.List]#StateMachine =
    Builder.builder[Token, NonTerminal, NonTerminal.List].build { builder =>
      val elem: Matcher[builder.StackFrame.StackElement, builder.ElementT] = { element =>
        builder.StackFrame.StackElement.unapply(element).map(_._3)
      }
      val stateElem: Matcher[builder.StackFrame.StackElement, (builder.State, builder.ElementT)] = { element =>
        builder.StackFrame.StackElement.unapply(element).map { case (_1, _, _3) => (_1, _3) }
      }

      lazy val s0: builder.State =
        builder.State(
          id = 0,
          acceptF = Some {
            case \/-(_: NonTerminal.List) => s1
            case -\/(_: Token.__.`(`) => s2
          },
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s1: builder.State =
        builder.State(
          id = 1,
          acceptF = Some {
            case -\/(Token.EOF) => s3
          },
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s2: builder.State =
        builder.State(
          id = 2,
          acceptF = Some {
            case -\/(_: Token._var) => s4
            case \/-(_: NonTerminal.AnonList1) => s5
          },
          returnFs = Nil,
          spontaneouslyGenerates = List(
            NonTerminal.AnonList1._2,
          ),
          finalReturnF = None,
        )
      lazy val s3: builder.State =
        builder.State(
          id = 3,
          acceptF = None,
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = Some {
            case elem(-\/(Token.EOF)) :: elem(\/-(rawTree: NonTerminal.List)) :: Nil =>
              rawTree
          },
        )
      lazy val s4: builder.State =
        builder.State(
          id = 4,
          acceptF = Some {
            case -\/(_: Token.__.`,`) => s6
            case \/-(_: NonTerminal.AnonList1_2) => s7
          },
          returnFs = Nil,
          spontaneouslyGenerates = List(
            NonTerminal.AnonList1_2._2,
          ),
          finalReturnF = None,
        )
      lazy val s5: builder.State =
        builder.State(
          id = 5,
          acceptF = Some {
            case -\/(_: Token.__.`)`) => s8
          },
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s6: builder.State =
        builder.State(
          id = 6,
          acceptF = Some {
            case -\/(_: Token._var) => s9
          },
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s7: builder.State =
        builder.State(
          id = 7,
          acceptF = None,
          returnFs = List(
            {
              case elem(\/-(_2: NonTerminal.AnonList1_2)) :: stateElem(state, -\/(_1: Token._var)) :: stackT =>
                (
                  state,
                  NonTerminal.AnonList1._1(_1, _2),
                  stackT,
                )
            },
          ),
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s8: builder.State =
        builder.State(
          id = 8,
          acceptF = None,
          returnFs = List(
            {
              case elem(-\/(_3: Token.__.`)`)) :: elem(\/-(_2: NonTerminal.AnonList1)) :: stateElem(state, -\/(_1: Token.__.`(`)) :: stackT =>
                (
                  state,
                  NonTerminal.List._1(_1, _2, _3),
                  stackT,
                )
            },
          ),
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s9: builder.State =
        builder.State(
          id = 9,
          acceptF = Some {
            case -\/(_: Token.__.`,`) => s6
            case \/-(_: NonTerminal.AnonList1_2) => s10
          },
          returnFs = Nil,
          spontaneouslyGenerates = List(
            NonTerminal.AnonList1_2._2,
          ),
          finalReturnF = None,
        )
      lazy val s10: builder.State =
        builder.State(
          id = 10,
          acceptF = None,
          returnFs = List(
            {
              case elem(\/-(_3: NonTerminal.AnonList1_2)) :: elem(-\/(_2: Token._var)) :: stateElem(state, -\/(_1: Token.__.`,`)) :: stackT =>
                (
                  state,
                  NonTerminal.AnonList1_2._1(_1, _2, _3),
                  stackT,
                )
            },
          ),
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )

      s0
    } {
      case (t1 @ HasSpanToken(s1), t2 @ HasSpanToken(s2)) =>
        (s2.start.abs > s1.start.abs).fold(t2, t1)
      case (eof @ Token.EOF, _) =>
        eof
      case (_, eof) =>
        eof
    }

  override def apply(input: String): List[String] \/ NonTerminal.List =
    arch.Parser(lexer, grammar)(input)

}
