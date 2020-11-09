package slyce.tests.calc

import scala.io.Source

import scalaz.-\/
import scalaz.\/-
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToOptionIdOps

import slyce.parse._
import slyce.common.helpers._

object ExOutput extends App {

  sealed trait Token extends Dfa.Token
  sealed trait HasSpanToken extends Token with Dfa.Token.HasSpan
  object Token {
    case object EOF extends Token
    object __ {

      def apply(str: String, span: Dfa.Token.Span): Token =
        str match {
          case "\n" =>
            `\n`(span)
          case "=" =>
            `=`(span)
          case "(" =>
            `(`(span)
          case ")" =>
            `)`(span)
          case _ =>
            ???
        }

      final case class `\n`(span: Dfa.Token.Span) extends HasSpanToken
      final case class `=`(span: Dfa.Token.Span) extends HasSpanToken
      final case class `(`(span: Dfa.Token.Span) extends HasSpanToken
      final case class `)`(span: Dfa.Token.Span) extends HasSpanToken
    }
    final case class _var(text: String, span: Dfa.Token.Span) extends HasSpanToken
    final case class addOp(text: String, span: Dfa.Token.Span) extends HasSpanToken
    final case class float(text: String, span: Dfa.Token.Span) extends HasSpanToken
    final case class int(text: String, span: Dfa.Token.Span) extends HasSpanToken
    final case class multOp(text: String, span: Dfa.Token.Span) extends HasSpanToken
    final case class powOp(text: String, span: Dfa.Token.Span) extends HasSpanToken
  }
  object HasSpanToken {
    def unapply(arg: HasSpanToken): Option[Dfa.Token.Span] = arg.span.some
  }

  sealed trait NonTerminal
  object NonTerminal {

    sealed trait __Start extends NonTerminal
    object __Start {

      final case class _1(
          _1: NonTerminal.Lines,
          _2: Token.EOF.type,
      ) extends __Start

    }

    sealed trait AnonList1 extends NonTerminal
    object AnonList1 {

      final case class _1(
          _1: Token.__.`\n`,
          _2: NonTerminal.AnonList1,
      ) extends AnonList1

      case object _2 extends AnonList1

    }

    sealed trait Assign extends NonTerminal
    object Assign {

      final case class _1(
          _1: Token._var,
          _2: Token.__.`=`,
          _3: NonTerminal.Expr,
      ) extends Assign

    }

    sealed trait Expr extends NonTerminal
    object Expr {

      final case class _1(
          _1: NonTerminal.Expr_2,
          _2: Token.powOp,
          _3: NonTerminal.Expr,
      ) extends Expr

      final case class _2(
          _1: NonTerminal.Expr_2,
      ) extends Expr

    }

    sealed trait Expr_2 extends NonTerminal
    object Expr_2 {

      final case class _1(
          _1: NonTerminal.Expr_2,
          _2: Token.multOp,
          _3: NonTerminal.Expr_3,
      ) extends Expr_2

      final case class _2(
          _1: NonTerminal.Expr_3,
      ) extends Expr_2

    }

    sealed trait Expr_3 extends NonTerminal
    object Expr_3 {

      final case class _1(
          _1: NonTerminal.Expr_3,
          _2: Token.addOp,
          _3: NonTerminal.Expr_4,
      ) extends Expr_3

      final case class _2(
          _1: NonTerminal.Expr_4,
      ) extends Expr_3

    }

    sealed trait Expr_4 extends NonTerminal
    object Expr_4 {

      final case class _1(
          _1: Token.__.`(`,
          _2: NonTerminal.Expr,
          _3: Token.__.`)`,
      ) extends Expr_4

      final case class _2(
          _1: Token.int,
      ) extends Expr_4

      final case class _3(
          _1: Token.float,
      ) extends Expr_4

      final case class _4(
          _1: Token._var,
      ) extends Expr_4

    }

    sealed trait Line extends NonTerminal
    object Line {

      final case class _1(
          _1: NonTerminal.Expr,
      ) extends Line

      final case class _2(
          _1: NonTerminal.Assign,
      ) extends Line

    }

    sealed trait Lines extends NonTerminal
    object Lines {

      final case class _1(
          _1: NonTerminal.AnonList1,
          _2: NonTerminal.Line,
          _3: NonTerminal.Lines_2,
      ) extends Lines

      case object _2 extends Lines

    }

    sealed trait Lines_2 extends NonTerminal
    object Lines_2 {

      final case class _1(
          _1: NonTerminal.AnonList1,
          _2: NonTerminal.Line,
          _3: NonTerminal.AnonList1,
          _4: NonTerminal.Lines_2,
      ) extends Lines_2

      case object _2 extends Lines_2

    }

  }

  val dfa: Dfa[Token] = {
    lazy val s0: Dfa.State[Token] =
      Dfa.State(
        id = 0,
        transitions = Map(
          0x9.toChar -> Some(Lazy(s1)), // '\t'
          0xa.toChar -> Some(Lazy(s9)), // '\n'
          0x20.toChar -> Some(Lazy(s1)), // ' '
          0x28.toChar -> Some(Lazy(s9)), // '('
          0x29.toChar -> Some(Lazy(s9)), // ')'
          0x2a.toChar -> Some(Lazy(s14)), // '*'
          0x2b.toChar -> Some(Lazy(s8)), // '+'
          0x2d.toChar -> Some(Lazy(s2)), // '-'
          0x2f.toChar -> Some(Lazy(s5)), // '/'
          0x30.toChar -> Some(Lazy(s10)), // '0'
          0x31.toChar -> Some(Lazy(s10)), // '1'
          0x32.toChar -> Some(Lazy(s10)), // '2'
          0x33.toChar -> Some(Lazy(s10)), // '3'
          0x34.toChar -> Some(Lazy(s10)), // '4'
          0x35.toChar -> Some(Lazy(s10)), // '5'
          0x36.toChar -> Some(Lazy(s10)), // '6'
          0x37.toChar -> Some(Lazy(s10)), // '7'
          0x38.toChar -> Some(Lazy(s10)), // '8'
          0x39.toChar -> Some(Lazy(s10)), // '9'
          0x3d.toChar -> Some(Lazy(s9)), // '='
          0x5e.toChar -> Some(Lazy(s6)), // '^'
          0x5f.toChar -> Some(Lazy(s15)), // '_'
          0x61.toChar -> Some(Lazy(s15)), // 'a'
          0x62.toChar -> Some(Lazy(s15)), // 'b'
          0x63.toChar -> Some(Lazy(s15)), // 'c'
          0x64.toChar -> Some(Lazy(s15)), // 'd'
          0x65.toChar -> Some(Lazy(s15)), // 'e'
          0x66.toChar -> Some(Lazy(s15)), // 'f'
          0x67.toChar -> Some(Lazy(s15)), // 'g'
          0x68.toChar -> Some(Lazy(s15)), // 'h'
          0x69.toChar -> Some(Lazy(s15)), // 'i'
          0x6a.toChar -> Some(Lazy(s15)), // 'j'
          0x6b.toChar -> Some(Lazy(s15)), // 'k'
          0x6c.toChar -> Some(Lazy(s15)), // 'l'
          0x6d.toChar -> Some(Lazy(s15)), // 'm'
          0x6e.toChar -> Some(Lazy(s15)), // 'n'
          0x6f.toChar -> Some(Lazy(s15)), // 'o'
          0x70.toChar -> Some(Lazy(s15)), // 'p'
          0x71.toChar -> Some(Lazy(s15)), // 'q'
          0x72.toChar -> Some(Lazy(s15)), // 'r'
          0x73.toChar -> Some(Lazy(s15)), // 's'
          0x74.toChar -> Some(Lazy(s15)), // 't'
          0x75.toChar -> Some(Lazy(s15)), // 'u'
          0x76.toChar -> Some(Lazy(s15)), // 'v'
          0x77.toChar -> Some(Lazy(s15)), // 'w'
          0x78.toChar -> Some(Lazy(s15)), // 'x'
          0x79.toChar -> Some(Lazy(s15)), // 'y'
          0x7a.toChar -> Some(Lazy(s15)), // 'z'
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
          0x30.toChar -> Some(Lazy(s10)), // '0'
          0x31.toChar -> Some(Lazy(s10)), // '1'
          0x32.toChar -> Some(Lazy(s10)), // '2'
          0x33.toChar -> Some(Lazy(s10)), // '3'
          0x34.toChar -> Some(Lazy(s10)), // '4'
          0x35.toChar -> Some(Lazy(s10)), // '5'
          0x36.toChar -> Some(Lazy(s10)), // '6'
          0x37.toChar -> Some(Lazy(s10)), // '7'
          0x38.toChar -> Some(Lazy(s10)), // '8'
          0x39.toChar -> Some(Lazy(s10)), // '9'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.addOp.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s3: Dfa.State[Token] =
      Dfa.State(
        id = 3,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s11)), // '0'
          0x31.toChar -> Some(Lazy(s11)), // '1'
          0x32.toChar -> Some(Lazy(s11)), // '2'
          0x33.toChar -> Some(Lazy(s11)), // '3'
          0x34.toChar -> Some(Lazy(s11)), // '4'
          0x35.toChar -> Some(Lazy(s11)), // '5'
          0x36.toChar -> Some(Lazy(s11)), // '6'
          0x37.toChar -> Some(Lazy(s11)), // '7'
          0x38.toChar -> Some(Lazy(s11)), // '8'
          0x39.toChar -> Some(Lazy(s11)), // '9'
        ),
        elseTransition = None,
        yields = None,
      )
    lazy val s4: Dfa.State[Token] =
      Dfa.State(
        id = 4,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s5: Dfa.State[Token] =
      Dfa.State(
        id = 5,
        transitions = Map(
          0x2a.toChar -> Some(Lazy(s18)), // '*'
          0x2f.toChar -> Some(Lazy(s17)), // '/'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.multOp.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s6: Dfa.State[Token] =
      Dfa.State(
        id = 6,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.powOp.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s7: Dfa.State[Token] =
      Dfa.State(
        id = 7,
        transitions = Map(
          0x2f.toChar -> Some(Lazy(s13)), // '/'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s12)()),
      )
    lazy val s8: Dfa.State[Token] =
      Dfa.State(
        id = 8,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.addOp.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s9: Dfa.State[Token] =
      Dfa.State(
        id = 9,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.__.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s10: Dfa.State[Token] =
      Dfa.State(
        id = 10,
        transitions = Map(
          0x2e.toChar -> Some(Lazy(s3)), // '.'
          0x30.toChar -> Some(Lazy(s10)), // '0'
          0x31.toChar -> Some(Lazy(s10)), // '1'
          0x32.toChar -> Some(Lazy(s10)), // '2'
          0x33.toChar -> Some(Lazy(s10)), // '3'
          0x34.toChar -> Some(Lazy(s10)), // '4'
          0x35.toChar -> Some(Lazy(s10)), // '5'
          0x36.toChar -> Some(Lazy(s10)), // '6'
          0x37.toChar -> Some(Lazy(s10)), // '7'
          0x38.toChar -> Some(Lazy(s10)), // '8'
          0x39.toChar -> Some(Lazy(s10)), // '9'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.int.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s11: Dfa.State[Token] =
      Dfa.State(
        id = 11,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s11)), // '0'
          0x31.toChar -> Some(Lazy(s11)), // '1'
          0x32.toChar -> Some(Lazy(s11)), // '2'
          0x33.toChar -> Some(Lazy(s11)), // '3'
          0x34.toChar -> Some(Lazy(s11)), // '4'
          0x35.toChar -> Some(Lazy(s11)), // '5'
          0x36.toChar -> Some(Lazy(s11)), // '6'
          0x37.toChar -> Some(Lazy(s11)), // '7'
          0x38.toChar -> Some(Lazy(s11)), // '8'
          0x39.toChar -> Some(Lazy(s11)), // '9'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.float.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s12: Dfa.State[Token] =
      Dfa.State(
        id = 12,
        transitions = Map(
          0x2a.toChar -> Some(Lazy(s7)), // '*'
        ),
        elseTransition = Some(Lazy(s16)),
        yields = None,
      )
    lazy val s13: Dfa.State[Token] =
      Dfa.State(
        id = 13,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s14: Dfa.State[Token] =
      Dfa.State(
        id = 14,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.multOp.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s15: Dfa.State[Token] =
      Dfa.State(
        id = 15,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s15)), // '0'
          0x31.toChar -> Some(Lazy(s15)), // '1'
          0x32.toChar -> Some(Lazy(s15)), // '2'
          0x33.toChar -> Some(Lazy(s15)), // '3'
          0x34.toChar -> Some(Lazy(s15)), // '4'
          0x35.toChar -> Some(Lazy(s15)), // '5'
          0x36.toChar -> Some(Lazy(s15)), // '6'
          0x37.toChar -> Some(Lazy(s15)), // '7'
          0x38.toChar -> Some(Lazy(s15)), // '8'
          0x39.toChar -> Some(Lazy(s15)), // '9'
          0x41.toChar -> Some(Lazy(s15)), // 'A'
          0x42.toChar -> Some(Lazy(s15)), // 'B'
          0x43.toChar -> Some(Lazy(s15)), // 'C'
          0x44.toChar -> Some(Lazy(s15)), // 'D'
          0x45.toChar -> Some(Lazy(s15)), // 'E'
          0x46.toChar -> Some(Lazy(s15)), // 'F'
          0x47.toChar -> Some(Lazy(s15)), // 'G'
          0x48.toChar -> Some(Lazy(s15)), // 'H'
          0x49.toChar -> Some(Lazy(s15)), // 'I'
          0x4a.toChar -> Some(Lazy(s15)), // 'J'
          0x4b.toChar -> Some(Lazy(s15)), // 'K'
          0x4c.toChar -> Some(Lazy(s15)), // 'L'
          0x4d.toChar -> Some(Lazy(s15)), // 'M'
          0x4e.toChar -> Some(Lazy(s15)), // 'N'
          0x4f.toChar -> Some(Lazy(s15)), // 'O'
          0x50.toChar -> Some(Lazy(s15)), // 'P'
          0x51.toChar -> Some(Lazy(s15)), // 'Q'
          0x52.toChar -> Some(Lazy(s15)), // 'R'
          0x53.toChar -> Some(Lazy(s15)), // 'S'
          0x54.toChar -> Some(Lazy(s15)), // 'T'
          0x55.toChar -> Some(Lazy(s15)), // 'U'
          0x56.toChar -> Some(Lazy(s15)), // 'V'
          0x57.toChar -> Some(Lazy(s15)), // 'W'
          0x58.toChar -> Some(Lazy(s15)), // 'X'
          0x59.toChar -> Some(Lazy(s15)), // 'Y'
          0x5a.toChar -> Some(Lazy(s15)), // 'Z'
          0x5f.toChar -> Some(Lazy(s15)), // '_'
          0x61.toChar -> Some(Lazy(s15)), // 'a'
          0x62.toChar -> Some(Lazy(s15)), // 'b'
          0x63.toChar -> Some(Lazy(s15)), // 'c'
          0x64.toChar -> Some(Lazy(s15)), // 'd'
          0x65.toChar -> Some(Lazy(s15)), // 'e'
          0x66.toChar -> Some(Lazy(s15)), // 'f'
          0x67.toChar -> Some(Lazy(s15)), // 'g'
          0x68.toChar -> Some(Lazy(s15)), // 'h'
          0x69.toChar -> Some(Lazy(s15)), // 'i'
          0x6a.toChar -> Some(Lazy(s15)), // 'j'
          0x6b.toChar -> Some(Lazy(s15)), // 'k'
          0x6c.toChar -> Some(Lazy(s15)), // 'l'
          0x6d.toChar -> Some(Lazy(s15)), // 'm'
          0x6e.toChar -> Some(Lazy(s15)), // 'n'
          0x6f.toChar -> Some(Lazy(s15)), // 'o'
          0x70.toChar -> Some(Lazy(s15)), // 'p'
          0x71.toChar -> Some(Lazy(s15)), // 'q'
          0x72.toChar -> Some(Lazy(s15)), // 'r'
          0x73.toChar -> Some(Lazy(s15)), // 's'
          0x74.toChar -> Some(Lazy(s15)), // 't'
          0x75.toChar -> Some(Lazy(s15)), // 'u'
          0x76.toChar -> Some(Lazy(s15)), // 'v'
          0x77.toChar -> Some(Lazy(s15)), // 'w'
          0x78.toChar -> Some(Lazy(s15)), // 'x'
          0x79.toChar -> Some(Lazy(s15)), // 'y'
          0x7a.toChar -> Some(Lazy(s15)), // 'z'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token._var.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s16: Dfa.State[Token] =
      Dfa.State(
        id = 16,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s12)()),
      )
    lazy val s17: Dfa.State[Token] =
      Dfa.State(
        id = 17,
        transitions = Map(
          0xa.toChar -> Some(Lazy(s4)), // '\n'
        ),
        elseTransition = Some(Lazy(s17)),
        yields = None,
      )
    lazy val s18: Dfa.State[Token] =
      Dfa.State(
        id = 18,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s12)()),
      )

    Dfa(s0, Token.EOF)
  }

  val stateMachine: Builder[Token, NonTerminal, NonTerminal.Lines]#StateMachine =
    Builder
      .builder[Token, NonTerminal, NonTerminal.Lines]
      .build { builder =>
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
              case \/-(_: NonTerminal.Lines)     => s1
              case \/-(_: NonTerminal.AnonList1) => s2
              case -\/(_: Token.__.`\n`)         => s3
            },
            returnFs = Nil,
            spontaneouslyGenerates = List(
              NonTerminal.AnonList1._2,
              NonTerminal.Lines._2,
            ),
            finalReturnF = None,
          )
        lazy val s1: builder.State =
          builder.State(
            id = 1,
            acceptF = Some {
              case -\/(Token.EOF) => s20
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s2: builder.State =
          builder.State(
            id = 2,
            acceptF = Some {
              case \/-(_: NonTerminal.Assign) => s11
              case \/-(_: NonTerminal.Expr_3) => s8
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case \/-(_: NonTerminal.Expr)   => s13
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_2) => s5
              case \/-(_: NonTerminal.Expr_4) => s4
              case -\/(_: Token._var)         => s27
              case \/-(_: NonTerminal.Line)   => s21
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s3: builder.State =
          builder.State(
            id = 3,
            acceptF = Some {
              case \/-(_: NonTerminal.AnonList1) => s12
              case -\/(_: Token.__.`\n`)         => s3
            },
            returnFs = Nil,
            spontaneouslyGenerates = List(
              NonTerminal.AnonList1._2,
            ),
            finalReturnF = None,
          )
        lazy val s4: builder.State =
          builder.State(
            id = 4,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_1: NonTerminal.Expr_4)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_3._2(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s5: builder.State =
          builder.State(
            id = 5,
            acceptF = Some {
              case -\/(_: Token.powOp)  => s7
              case -\/(_: Token.multOp) => s6
            },
            returnFs = List(
              {
                case stateElem(state, \/-(_1: NonTerminal.Expr_2)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr._2(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s6: builder.State =
          builder.State(
            id = 6,
            acceptF = Some {
              case \/-(_: NonTerminal.Expr_3) => s14
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_4) => s4
              case -\/(_: Token._var)         => s23
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s7: builder.State =
          builder.State(
            id = 7,
            acceptF = Some {
              case \/-(_: NonTerminal.Expr_3) => s8
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case \/-(_: NonTerminal.Expr)   => s9
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_2) => s5
              case \/-(_: NonTerminal.Expr_4) => s4
              case -\/(_: Token._var)         => s23
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s8: builder.State =
          builder.State(
            id = 8,
            acceptF = Some {
              case -\/(_: Token.addOp) => s15
            },
            returnFs = List(
              {
                case stateElem(state, \/-(_1: NonTerminal.Expr_3)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_2._2(_1),
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
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_3: NonTerminal.Expr)) :: elem(-\/(_2: Token.powOp)) :: elem(
                      \/-(_1: NonTerminal.Expr_2),
                    ) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr._1(_1, _2, _3),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s10: builder.State =
          builder.State(
            id = 10,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, -\/(_1: Token.int)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_4._2(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s11: builder.State =
          builder.State(
            id = 11,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_1: NonTerminal.Assign)) :: stackT =>
                  (
                    state,
                    NonTerminal.Line._2(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s12: builder.State =
          builder.State(
            id = 12,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_2: NonTerminal.AnonList1)) :: elem(-\/(_1: Token.__.`\n`)) :: stackT =>
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
        lazy val s13: builder.State =
          builder.State(
            id = 13,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_1: NonTerminal.Expr)) :: stackT =>
                  (
                    state,
                    NonTerminal.Line._1(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s14: builder.State =
          builder.State(
            id = 14,
            acceptF = Some {
              case -\/(_: Token.addOp) => s15
            },
            returnFs = List(
              {
                case stateElem(state, \/-(_3: NonTerminal.Expr_3)) :: elem(-\/(_2: Token.multOp)) :: elem(
                      \/-(_1: NonTerminal.Expr_2),
                    ) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_2._1(_1, _2, _3),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s15: builder.State =
          builder.State(
            id = 15,
            acceptF = Some {
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_4) => s16
              case -\/(_: Token._var)         => s23
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s16: builder.State =
          builder.State(
            id = 16,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_3: NonTerminal.Expr_4)) :: elem(-\/(_2: Token.addOp)) :: elem(
                      \/-(_1: NonTerminal.Expr_3),
                    ) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_3._1(_1, _2, _3),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s17: builder.State =
          builder.State(
            id = 17,
            acceptF = Some {
              case \/-(_: NonTerminal.Expr_3) => s8
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case \/-(_: NonTerminal.Expr)   => s18
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_2) => s5
              case \/-(_: NonTerminal.Expr_4) => s4
              case -\/(_: Token._var)         => s23
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s18: builder.State =
          builder.State(
            id = 18,
            acceptF = Some {
              case -\/(_: Token.__.`)`) => s19
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s19: builder.State =
          builder.State(
            id = 19,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, -\/(_3: Token.__.`)`)) :: elem(\/-(_2: NonTerminal.Expr)) :: elem(
                      -\/(_1: Token.__.`(`),
                    ) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_4._1(_1, _2, _3),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s20: builder.State =
          builder.State(
            id = 20,
            acceptF = None,
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = Some {
              case elem(\/-(rawTree: NonTerminal.Lines)) :: elem(-\/(Token.EOF)) :: Nil =>
                rawTree
            },
          )
        lazy val s21: builder.State =
          builder.State(
            id = 21,
            acceptF = Some {
              case \/-(_: NonTerminal.Lines_2)   => s22
              case \/-(_: NonTerminal.AnonList1) => s24
              case -\/(_: Token.__.`\n`)         => s3
            },
            returnFs = Nil,
            spontaneouslyGenerates = List(
              NonTerminal.Lines_2._2,
              NonTerminal.AnonList1._2,
            ),
            finalReturnF = None,
          )
        lazy val s22: builder.State =
          builder.State(
            id = 22,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_3: NonTerminal.Lines_2)) :: elem(\/-(_2: NonTerminal.Line)) :: elem(
                      \/-(_1: NonTerminal.AnonList1),
                    ) :: stackT =>
                  (
                    state,
                    NonTerminal.Lines._1(_1, _2, _3),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s23: builder.State =
          builder.State(
            id = 23,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, -\/(_1: Token._var)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_4._4(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s24: builder.State =
          builder.State(
            id = 24,
            acceptF = Some {
              case \/-(_: NonTerminal.Assign) => s11
              case \/-(_: NonTerminal.Expr_3) => s8
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case \/-(_: NonTerminal.Expr)   => s13
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_2) => s5
              case \/-(_: NonTerminal.Expr_4) => s4
              case -\/(_: Token._var)         => s27
              case \/-(_: NonTerminal.Line)   => s25
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s25: builder.State =
          builder.State(
            id = 25,
            acceptF = Some {
              case \/-(_: NonTerminal.AnonList1) => s28
              case -\/(_: Token.__.`\n`)         => s3
            },
            returnFs = Nil,
            spontaneouslyGenerates = List(
              NonTerminal.AnonList1._2,
            ),
            finalReturnF = None,
          )
        lazy val s26: builder.State =
          builder.State(
            id = 26,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, -\/(_1: Token.float)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_4._3(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s27: builder.State =
          builder.State(
            id = 27,
            acceptF = Some {
              case -\/(_: Token.__.`=`) => s29
            },
            returnFs = List(
              {
                case stateElem(state, -\/(_1: Token._var)) :: stackT =>
                  (
                    state,
                    NonTerminal.Expr_4._4(_1),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s28: builder.State =
          builder.State(
            id = 28,
            acceptF = Some {
              case \/-(_: NonTerminal.Lines_2)   => s30
              case \/-(_: NonTerminal.AnonList1) => s24
              case -\/(_: Token.__.`\n`)         => s3
            },
            returnFs = Nil,
            spontaneouslyGenerates = List(
              NonTerminal.Lines_2._2,
              NonTerminal.AnonList1._2,
            ),
            finalReturnF = None,
          )
        lazy val s29: builder.State =
          builder.State(
            id = 29,
            acceptF = Some {
              case \/-(_: NonTerminal.Expr_3) => s8
              case -\/(_: Token.int)          => s10
              case -\/(_: Token.__.`(`)       => s17
              case \/-(_: NonTerminal.Expr)   => s31
              case -\/(_: Token.float)        => s26
              case \/-(_: NonTerminal.Expr_2) => s5
              case \/-(_: NonTerminal.Expr_4) => s4
              case -\/(_: Token._var)         => s23
            },
            returnFs = Nil,
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s30: builder.State =
          builder.State(
            id = 30,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_4: NonTerminal.Lines_2)) :: elem(\/-(_3: NonTerminal.AnonList1)) :: elem(
                      \/-(_2: NonTerminal.Line),
                    ) :: elem(\/-(_1: NonTerminal.AnonList1)) :: stackT =>
                  (
                    state,
                    NonTerminal.Lines_2._1(_1, _2, _3, _4),
                    stackT,
                  )
              },
            ),
            spontaneouslyGenerates = Nil,
            finalReturnF = None,
          )
        lazy val s31: builder.State =
          builder.State(
            id = 31,
            acceptF = None,
            returnFs = List(
              {
                case stateElem(state, \/-(_3: NonTerminal.Expr)) :: elem(-\/(_2: Token.__.`=`)) :: elem(
                      -\/(_1: Token._var),
                    ) :: stackT =>
                  (
                    state,
                    NonTerminal.Assign._1(_1, _2, _3),
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

  {
    val source = Source.fromFile("res-test/calc/samples/ex1.txt")
    val str = source.mkString
    source.close

    val res = for {
      toks <- dfa(str)
      rawTree <- stateMachine(toks)
    } yield rawTree

    res match {
      case -\/(err) =>
        println("Error:")
        println()
        err.foreach(println)
      case \/-(rawTree) =>
        println("Success:")
        println()
        println(rawTree)
    }
  }

}
