package slyce.tests.argList

import scala.io.Source

import scalaz.-\/
import scalaz.\/-
import scalaz.Scalaz.ToEitherOps

import slyce.parse._
import slyce.common.helpers._

object ExOutput extends App {

  sealed trait Token extends Dfa.Token
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

      final case class `,`(span: Dfa.Token.Span) extends Token
      final case class `(`(span: Dfa.Token.Span) extends Token
      final case class `)`(span: Dfa.Token.Span) extends Token
    }
    final case class _var(text: String, span: Dfa.Token.Span) extends Token
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

    Dfa(s0)
  }

  val stateMachine: StateMachine[Token, NonTerminal, NonTerminal.List] = {
    type State = StateMachine.State[Token, NonTerminal, NonTerminal.List]

    lazy val s0: State =
      StateMachine.State(
        id = 0,
        acceptF = Some {
          case \/-(_: NonTerminal.List) => s1
          case -\/(_: Token.__.`(`) => s2
        },
        returnFs = Nil,
        finalReturn = None, // TODO : ...
      )

    lazy val s1: State =
      StateMachine.State(
        id = 1,
        acceptF = Some {
          case -\/(Token.EOF) => s3
        },
        returnFs = Nil,
        finalReturn = None,
      )

    lazy val s2: State =
      StateMachine.State(
        id = 2,
        acceptF = Some {
          case -\/(_: Token._var) => s4
          case \/-(_: NonTerminal.AnonList1) => s5
        },
        returnFs = List(
          {
            case list @ ((s, _) :: _) =>
              // AnonList1[2] : 
              (s, NonTerminal.AnonList1._2.right) :: list
          },
        ),
        finalReturn = None,
      )

    lazy val s3: State =
      StateMachine.State(
        id = 3,
        acceptF = None,
        returnFs = List(
          {
            case list =>
              // __Start[1] : NonTerminal(List), Terminal(EOF.type)
              ???
          },
        ),
        finalReturn = None,
      )

    lazy val s4: State =
      StateMachine.State(
        id = 4,
        acceptF = Some {
          case -\/(_: Token.__.`,`) => s6
          case \/-(_: NonTerminal.AnonList1_2) => s7
        },
        returnFs = List(
          {
            case list @ ((s, _) :: _) =>
              // AnonList1_2[2] : 
              (s, NonTerminal.AnonList1_2._2.right) :: list
          },
        ),
        finalReturn = None,
      )

    lazy val s5: State =
      StateMachine.State(
        id = 5,
        acceptF = Some {
          case -\/(_: Token.__.`)`) => s8
        },
        returnFs = Nil,
        finalReturn = None,
      )

    lazy val s6: State =
      StateMachine.State(
        id = 6,
        acceptF = Some {
          case -\/(_: Token._var) => s9
        },
        returnFs = Nil,
        finalReturn = None,
      )

    lazy val s7: State =
      StateMachine.State(
        id = 7,
        acceptF = None,
        returnFs = List(
          {
            case list =>
              // AnonList1[1] : Terminal(_var), NonTerminal(AnonList1_2)
              ???
          },
        ),
        finalReturn = None,
      )

    lazy val s8: State =
      StateMachine.State(
        id = 8,
        acceptF = None,
        returnFs = List(
          {
            case list =>
              // List[1] : Raw("("), NonTerminal(AnonList1), Raw(")")
              ???
          },
        ),
        finalReturn = None,
      )

    lazy val s9: State =
      StateMachine.State(
        id = 9,
        acceptF = Some {
          case -\/(_: Token.__.`,`) => s6
          case \/-(_: NonTerminal.AnonList1_2) => s10
        },
        returnFs = List(
          {
            case list @ ((s, _) :: _) =>
              // AnonList1_2[2] : 
              (s, NonTerminal.AnonList1_2._2.right) :: list
          },
        ),
        finalReturn = None,
      )

    lazy val s10: State =
      StateMachine.State(
        id = 10,
        acceptF = None,
        returnFs = List(
          {
            case list =>
              // AnonList1_2[1] : Raw(","), Terminal(_var), NonTerminal(AnonList1_2)
              ???
          },
        ),
        finalReturn = None,
      )

    StateMachine(s0)
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
