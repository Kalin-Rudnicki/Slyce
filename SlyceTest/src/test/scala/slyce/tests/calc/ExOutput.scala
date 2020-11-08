package slyce.tests.calc

import scala.io.Source

import scalaz.-\/
import scalaz.\/-

import slyce.parse._
import slyce.common.helpers._

object ExOutput extends App {

  sealed trait Token extends Dfa.Token
  object Token {
    case object EOF extends Token
    final case class _var(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class addOp(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class float(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class int(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class multOp(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class powOp(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class raw(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
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
        _1: Token.raw,
        _2: NonTerminal.AnonList1,
      ) extends AnonList1

      case object _2 extends AnonList1

    }

    sealed trait Assign extends NonTerminal
    object Assign {

      final case class _1(
        _1: Token._var,
        _2: Token.raw,
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
        _1: Token.raw,
        _2: NonTerminal.Expr,
        _3: Token.raw,
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
          0x9.toChar -> Some(Lazy(s16)), // '\t'
          0xA.toChar -> Some(Lazy(s1)), // '\n'
          0x20.toChar -> Some(Lazy(s16)), // ' '
          0x28.toChar -> Some(Lazy(s1)), // '('
          0x29.toChar -> Some(Lazy(s1)), // ')'
          0x2A.toChar -> Some(Lazy(s9)), // '*'
          0x2B.toChar -> Some(Lazy(s3)), // '+'
          0x2D.toChar -> Some(Lazy(s14)), // '-'
          0x2F.toChar -> Some(Lazy(s7)), // '/'
          0x30.toChar -> Some(Lazy(s17)), // '0'
          0x31.toChar -> Some(Lazy(s17)), // '1'
          0x32.toChar -> Some(Lazy(s17)), // '2'
          0x33.toChar -> Some(Lazy(s17)), // '3'
          0x34.toChar -> Some(Lazy(s17)), // '4'
          0x35.toChar -> Some(Lazy(s17)), // '5'
          0x36.toChar -> Some(Lazy(s17)), // '6'
          0x37.toChar -> Some(Lazy(s17)), // '7'
          0x38.toChar -> Some(Lazy(s17)), // '8'
          0x39.toChar -> Some(Lazy(s17)), // '9'
          0x3D.toChar -> Some(Lazy(s1)), // '='
          0x5E.toChar -> Some(Lazy(s4)), // '^'
          0x5F.toChar -> Some(Lazy(s10)), // '_'
          0x61.toChar -> Some(Lazy(s10)), // 'a'
          0x62.toChar -> Some(Lazy(s10)), // 'b'
          0x63.toChar -> Some(Lazy(s10)), // 'c'
          0x64.toChar -> Some(Lazy(s10)), // 'd'
          0x65.toChar -> Some(Lazy(s10)), // 'e'
          0x66.toChar -> Some(Lazy(s10)), // 'f'
          0x67.toChar -> Some(Lazy(s10)), // 'g'
          0x68.toChar -> Some(Lazy(s10)), // 'h'
          0x69.toChar -> Some(Lazy(s10)), // 'i'
          0x6A.toChar -> Some(Lazy(s10)), // 'j'
          0x6B.toChar -> Some(Lazy(s10)), // 'k'
          0x6C.toChar -> Some(Lazy(s10)), // 'l'
          0x6D.toChar -> Some(Lazy(s10)), // 'm'
          0x6E.toChar -> Some(Lazy(s10)), // 'n'
          0x6F.toChar -> Some(Lazy(s10)), // 'o'
          0x70.toChar -> Some(Lazy(s10)), // 'p'
          0x71.toChar -> Some(Lazy(s10)), // 'q'
          0x72.toChar -> Some(Lazy(s10)), // 'r'
          0x73.toChar -> Some(Lazy(s10)), // 's'
          0x74.toChar -> Some(Lazy(s10)), // 't'
          0x75.toChar -> Some(Lazy(s10)), // 'u'
          0x76.toChar -> Some(Lazy(s10)), // 'v'
          0x77.toChar -> Some(Lazy(s10)), // 'w'
          0x78.toChar -> Some(Lazy(s10)), // 'x'
          0x79.toChar -> Some(Lazy(s10)), // 'y'
          0x7A.toChar -> Some(Lazy(s10)), // 'z'
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
              tokF = Token.raw.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
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
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.float.apply,
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
              tokF = Token.addOp.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s4: Dfa.State[Token] =
      Dfa.State(
        id = 4,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.powOp.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s5: Dfa.State[Token] =
      Dfa.State(
        id = 5,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s15)()),
      )
    lazy val s6: Dfa.State[Token] =
      Dfa.State(
        id = 6,
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
        ),
        elseTransition = None,
        yields = None,
      )
    lazy val s7: Dfa.State[Token] =
      Dfa.State(
        id = 7,
        transitions = Map(
          0x2A.toChar -> Some(Lazy(s5)), // '*'
          0x2F.toChar -> Some(Lazy(s13)), // '/'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.multOp.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s8: Dfa.State[Token] =
      Dfa.State(
        id = 8,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s9: Dfa.State[Token] =
      Dfa.State(
        id = 9,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.multOp.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s10: Dfa.State[Token] =
      Dfa.State(
        id = 10,
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
          0x41.toChar -> Some(Lazy(s10)), // 'A'
          0x42.toChar -> Some(Lazy(s10)), // 'B'
          0x43.toChar -> Some(Lazy(s10)), // 'C'
          0x44.toChar -> Some(Lazy(s10)), // 'D'
          0x45.toChar -> Some(Lazy(s10)), // 'E'
          0x46.toChar -> Some(Lazy(s10)), // 'F'
          0x47.toChar -> Some(Lazy(s10)), // 'G'
          0x48.toChar -> Some(Lazy(s10)), // 'H'
          0x49.toChar -> Some(Lazy(s10)), // 'I'
          0x4A.toChar -> Some(Lazy(s10)), // 'J'
          0x4B.toChar -> Some(Lazy(s10)), // 'K'
          0x4C.toChar -> Some(Lazy(s10)), // 'L'
          0x4D.toChar -> Some(Lazy(s10)), // 'M'
          0x4E.toChar -> Some(Lazy(s10)), // 'N'
          0x4F.toChar -> Some(Lazy(s10)), // 'O'
          0x50.toChar -> Some(Lazy(s10)), // 'P'
          0x51.toChar -> Some(Lazy(s10)), // 'Q'
          0x52.toChar -> Some(Lazy(s10)), // 'R'
          0x53.toChar -> Some(Lazy(s10)), // 'S'
          0x54.toChar -> Some(Lazy(s10)), // 'T'
          0x55.toChar -> Some(Lazy(s10)), // 'U'
          0x56.toChar -> Some(Lazy(s10)), // 'V'
          0x57.toChar -> Some(Lazy(s10)), // 'W'
          0x58.toChar -> Some(Lazy(s10)), // 'X'
          0x59.toChar -> Some(Lazy(s10)), // 'Y'
          0x5A.toChar -> Some(Lazy(s10)), // 'Z'
          0x5F.toChar -> Some(Lazy(s10)), // '_'
          0x61.toChar -> Some(Lazy(s10)), // 'a'
          0x62.toChar -> Some(Lazy(s10)), // 'b'
          0x63.toChar -> Some(Lazy(s10)), // 'c'
          0x64.toChar -> Some(Lazy(s10)), // 'd'
          0x65.toChar -> Some(Lazy(s10)), // 'e'
          0x66.toChar -> Some(Lazy(s10)), // 'f'
          0x67.toChar -> Some(Lazy(s10)), // 'g'
          0x68.toChar -> Some(Lazy(s10)), // 'h'
          0x69.toChar -> Some(Lazy(s10)), // 'i'
          0x6A.toChar -> Some(Lazy(s10)), // 'j'
          0x6B.toChar -> Some(Lazy(s10)), // 'k'
          0x6C.toChar -> Some(Lazy(s10)), // 'l'
          0x6D.toChar -> Some(Lazy(s10)), // 'm'
          0x6E.toChar -> Some(Lazy(s10)), // 'n'
          0x6F.toChar -> Some(Lazy(s10)), // 'o'
          0x70.toChar -> Some(Lazy(s10)), // 'p'
          0x71.toChar -> Some(Lazy(s10)), // 'q'
          0x72.toChar -> Some(Lazy(s10)), // 'r'
          0x73.toChar -> Some(Lazy(s10)), // 's'
          0x74.toChar -> Some(Lazy(s10)), // 't'
          0x75.toChar -> Some(Lazy(s10)), // 'u'
          0x76.toChar -> Some(Lazy(s10)), // 'v'
          0x77.toChar -> Some(Lazy(s10)), // 'w'
          0x78.toChar -> Some(Lazy(s10)), // 'x'
          0x79.toChar -> Some(Lazy(s10)), // 'y'
          0x7A.toChar -> Some(Lazy(s10)), // 'z'
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
    lazy val s11: Dfa.State[Token] =
      Dfa.State(
        id = 11,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s15)()),
      )
    lazy val s12: Dfa.State[Token] =
      Dfa.State(
        id = 12,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s13: Dfa.State[Token] =
      Dfa.State(
        id = 13,
        transitions = Map(
          0xA.toChar -> Some(Lazy(s8)), // '\n'
        ),
        elseTransition = Some(Lazy(s13)),
        yields = None,
      )
    lazy val s14: Dfa.State[Token] =
      Dfa.State(
        id = 14,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s17)), // '0'
          0x31.toChar -> Some(Lazy(s17)), // '1'
          0x32.toChar -> Some(Lazy(s17)), // '2'
          0x33.toChar -> Some(Lazy(s17)), // '3'
          0x34.toChar -> Some(Lazy(s17)), // '4'
          0x35.toChar -> Some(Lazy(s17)), // '5'
          0x36.toChar -> Some(Lazy(s17)), // '6'
          0x37.toChar -> Some(Lazy(s17)), // '7'
          0x38.toChar -> Some(Lazy(s17)), // '8'
          0x39.toChar -> Some(Lazy(s17)), // '9'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.addOp.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s15: Dfa.State[Token] =
      Dfa.State(
        id = 15,
        transitions = Map(
          0x2A.toChar -> Some(Lazy(s18)), // '*'
        ),
        elseTransition = Some(Lazy(s11)),
        yields = None,
      )
    lazy val s16: Dfa.State[Token] =
      Dfa.State(
        id = 16,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s17: Dfa.State[Token] =
      Dfa.State(
        id = 17,
        transitions = Map(
          0x2E.toChar -> Some(Lazy(s6)), // '.'
          0x30.toChar -> Some(Lazy(s17)), // '0'
          0x31.toChar -> Some(Lazy(s17)), // '1'
          0x32.toChar -> Some(Lazy(s17)), // '2'
          0x33.toChar -> Some(Lazy(s17)), // '3'
          0x34.toChar -> Some(Lazy(s17)), // '4'
          0x35.toChar -> Some(Lazy(s17)), // '5'
          0x36.toChar -> Some(Lazy(s17)), // '6'
          0x37.toChar -> Some(Lazy(s17)), // '7'
          0x38.toChar -> Some(Lazy(s17)), // '8'
          0x39.toChar -> Some(Lazy(s17)), // '9'
        ),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.int.apply,
              spanRange = (0,-1),
            ),
          ),
        ),
      )
    lazy val s18: Dfa.State[Token] =
      Dfa.State(
        id = 18,
        transitions = Map(
          0x2F.toChar -> Some(Lazy(s12)), // '/'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s15)()),
      )

    Dfa(s0)
  }

  {
    val source = Source.fromFile("res-test/calc/samples/ex1.txt")
    val str = source.mkString
    source.close

    dfa(str) match {
      case -\/(err) =>
        println("Error:")
        println()
        err.foreach(println)
      case \/-(toks) =>
        println("Success:")
        println()
        toks.foreach(println)
    }
  }

}
