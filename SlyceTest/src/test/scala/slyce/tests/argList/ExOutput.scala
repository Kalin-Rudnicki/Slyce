package slyce.tests.argList

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

  val dfa: Dfa[Token] = {
    lazy val s0: Dfa.State[Token] =
      Dfa.State(
        id = 0,
        transitions = Map(
          0x9.toChar -> Some(Lazy(s2)), // '\t'
          0xA.toChar -> Some(Lazy(s2)), // '\n'
          0x20.toChar -> Some(Lazy(s2)), // ' '
          0x28.toChar -> Some(Lazy(s1)), // '('
          0x29.toChar -> Some(Lazy(s1)), // ')'
          0x2C.toChar -> Some(Lazy(s1)), // ','
          0x5F.toChar -> Some(Lazy(s3)), // '_'
          0x61.toChar -> Some(Lazy(s3)), // 'a'
          0x62.toChar -> Some(Lazy(s3)), // 'b'
          0x63.toChar -> Some(Lazy(s3)), // 'c'
          0x64.toChar -> Some(Lazy(s3)), // 'd'
          0x65.toChar -> Some(Lazy(s3)), // 'e'
          0x66.toChar -> Some(Lazy(s3)), // 'f'
          0x67.toChar -> Some(Lazy(s3)), // 'g'
          0x68.toChar -> Some(Lazy(s3)), // 'h'
          0x69.toChar -> Some(Lazy(s3)), // 'i'
          0x6A.toChar -> Some(Lazy(s3)), // 'j'
          0x6B.toChar -> Some(Lazy(s3)), // 'k'
          0x6C.toChar -> Some(Lazy(s3)), // 'l'
          0x6D.toChar -> Some(Lazy(s3)), // 'm'
          0x6E.toChar -> Some(Lazy(s3)), // 'n'
          0x6F.toChar -> Some(Lazy(s3)), // 'o'
          0x70.toChar -> Some(Lazy(s3)), // 'p'
          0x71.toChar -> Some(Lazy(s3)), // 'q'
          0x72.toChar -> Some(Lazy(s3)), // 'r'
          0x73.toChar -> Some(Lazy(s3)), // 's'
          0x74.toChar -> Some(Lazy(s3)), // 't'
          0x75.toChar -> Some(Lazy(s3)), // 'u'
          0x76.toChar -> Some(Lazy(s3)), // 'v'
          0x77.toChar -> Some(Lazy(s3)), // 'w'
          0x78.toChar -> Some(Lazy(s3)), // 'x'
          0x79.toChar -> Some(Lazy(s3)), // 'y'
          0x7A.toChar -> Some(Lazy(s3)), // 'z'
        ),
        elseTransition = None,
        yields = None,
      )
    lazy val s1: Dfa.State[Token] =
      Dfa.State(
        id = 1,
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
    lazy val s2: Dfa.State[Token] =
      Dfa.State(
        id = 2,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s3: Dfa.State[Token] =
      Dfa.State(
        id = 3,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s3)), // '0'
          0x31.toChar -> Some(Lazy(s3)), // '1'
          0x32.toChar -> Some(Lazy(s3)), // '2'
          0x33.toChar -> Some(Lazy(s3)), // '3'
          0x34.toChar -> Some(Lazy(s3)), // '4'
          0x35.toChar -> Some(Lazy(s3)), // '5'
          0x36.toChar -> Some(Lazy(s3)), // '6'
          0x37.toChar -> Some(Lazy(s3)), // '7'
          0x38.toChar -> Some(Lazy(s3)), // '8'
          0x39.toChar -> Some(Lazy(s3)), // '9'
          0x41.toChar -> Some(Lazy(s3)), // 'A'
          0x42.toChar -> Some(Lazy(s3)), // 'B'
          0x43.toChar -> Some(Lazy(s3)), // 'C'
          0x44.toChar -> Some(Lazy(s3)), // 'D'
          0x45.toChar -> Some(Lazy(s3)), // 'E'
          0x46.toChar -> Some(Lazy(s3)), // 'F'
          0x47.toChar -> Some(Lazy(s3)), // 'G'
          0x48.toChar -> Some(Lazy(s3)), // 'H'
          0x49.toChar -> Some(Lazy(s3)), // 'I'
          0x4A.toChar -> Some(Lazy(s3)), // 'J'
          0x4B.toChar -> Some(Lazy(s3)), // 'K'
          0x4C.toChar -> Some(Lazy(s3)), // 'L'
          0x4D.toChar -> Some(Lazy(s3)), // 'M'
          0x4E.toChar -> Some(Lazy(s3)), // 'N'
          0x4F.toChar -> Some(Lazy(s3)), // 'O'
          0x50.toChar -> Some(Lazy(s3)), // 'P'
          0x51.toChar -> Some(Lazy(s3)), // 'Q'
          0x52.toChar -> Some(Lazy(s3)), // 'R'
          0x53.toChar -> Some(Lazy(s3)), // 'S'
          0x54.toChar -> Some(Lazy(s3)), // 'T'
          0x55.toChar -> Some(Lazy(s3)), // 'U'
          0x56.toChar -> Some(Lazy(s3)), // 'V'
          0x57.toChar -> Some(Lazy(s3)), // 'W'
          0x58.toChar -> Some(Lazy(s3)), // 'X'
          0x59.toChar -> Some(Lazy(s3)), // 'Y'
          0x5A.toChar -> Some(Lazy(s3)), // 'Z'
          0x5F.toChar -> Some(Lazy(s3)), // '_'
          0x61.toChar -> Some(Lazy(s3)), // 'a'
          0x62.toChar -> Some(Lazy(s3)), // 'b'
          0x63.toChar -> Some(Lazy(s3)), // 'c'
          0x64.toChar -> Some(Lazy(s3)), // 'd'
          0x65.toChar -> Some(Lazy(s3)), // 'e'
          0x66.toChar -> Some(Lazy(s3)), // 'f'
          0x67.toChar -> Some(Lazy(s3)), // 'g'
          0x68.toChar -> Some(Lazy(s3)), // 'h'
          0x69.toChar -> Some(Lazy(s3)), // 'i'
          0x6A.toChar -> Some(Lazy(s3)), // 'j'
          0x6B.toChar -> Some(Lazy(s3)), // 'k'
          0x6C.toChar -> Some(Lazy(s3)), // 'l'
          0x6D.toChar -> Some(Lazy(s3)), // 'm'
          0x6E.toChar -> Some(Lazy(s3)), // 'n'
          0x6F.toChar -> Some(Lazy(s3)), // 'o'
          0x70.toChar -> Some(Lazy(s3)), // 'p'
          0x71.toChar -> Some(Lazy(s3)), // 'q'
          0x72.toChar -> Some(Lazy(s3)), // 'r'
          0x73.toChar -> Some(Lazy(s3)), // 's'
          0x74.toChar -> Some(Lazy(s3)), // 't'
          0x75.toChar -> Some(Lazy(s3)), // 'u'
          0x76.toChar -> Some(Lazy(s3)), // 'v'
          0x77.toChar -> Some(Lazy(s3)), // 'w'
          0x78.toChar -> Some(Lazy(s3)), // 'x'
          0x79.toChar -> Some(Lazy(s3)), // 'y'
          0x7A.toChar -> Some(Lazy(s3)), // 'z'
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

    Dfa(s0, Token.EOF)
  }

  val stateMachine: Builder[Token, NonTerminal, NonTerminal.List]#StateMachine =
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
            case -\/(_: Token.__.`)`) => s6
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
            case elem(\/-(rawTree: NonTerminal.List)) :: elem(-\/(Token.EOF)) :: Nil =>
              rawTree
          },
        )
      lazy val s4: builder.State =
        builder.State(
          id = 4,
          acceptF = Some {
            case -\/(_: Token.__.`,`) => s7
            case \/-(_: NonTerminal.AnonList1_2) => s8
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
            case -\/(_: Token.__.`)`) => s6
          },
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s6: builder.State =
        builder.State(
          id = 6,
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
      lazy val s7: builder.State =
        builder.State(
          id = 7,
          acceptF = Some {
            case -\/(_: Token._var) => s9
          },
          returnFs = Nil,
          spontaneouslyGenerates = Nil,
          finalReturnF = None,
        )
      lazy val s8: builder.State =
        builder.State(
          id = 8,
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
      lazy val s9: builder.State =
        builder.State(
          id = 9,
          acceptF = Some {
            case -\/(_: Token.__.`,`) => s7
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

  {
    val source = Source.fromFile("res-test/argList/samples/ex1.txt")
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
