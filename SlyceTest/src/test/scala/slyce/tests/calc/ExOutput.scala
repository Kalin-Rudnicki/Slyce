package slyce.tests.calc

import scala.io.Source

import scalaz.-\/
import scalaz.\/-

import slyce.parse._
import slyce.common.helpers._

object ExOutput extends App {

  sealed trait Token extends Dfa.Token
  object Token {
    final case class _var(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class addOp(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class float(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class int(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class multOp(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class powOp(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
    final case class raw(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token
  }

  val dfa: Dfa[Token] = {
    lazy val s0: Dfa.State[Token] =
      Dfa.State(
        id = 0,
        transitions = Map(
          0x9.toChar -> Some(Lazy(s12)), // '\t'
          0xa.toChar -> Some(Lazy(s16)), // '\n'
          0x20.toChar -> Some(Lazy(s12)), // ' '
          0x28.toChar -> Some(Lazy(s16)), // '('
          0x29.toChar -> Some(Lazy(s16)), // ')'
          0x2a.toChar -> Some(Lazy(s2)), // '*'
          0x2b.toChar -> Some(Lazy(s18)), // '+'
          0x2d.toChar -> Some(Lazy(s7)), // '-'
          0x2f.toChar -> Some(Lazy(s10)), // '/'
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
          0x3d.toChar -> Some(Lazy(s16)), // '='
          0x5e.toChar -> Some(Lazy(s9)), // '^'
          0x5f.toChar -> Some(Lazy(s8)), // '_'
          0x61.toChar -> Some(Lazy(s8)), // 'a'
          0x62.toChar -> Some(Lazy(s8)), // 'b'
          0x63.toChar -> Some(Lazy(s8)), // 'c'
          0x64.toChar -> Some(Lazy(s8)), // 'd'
          0x65.toChar -> Some(Lazy(s8)), // 'e'
          0x66.toChar -> Some(Lazy(s8)), // 'f'
          0x67.toChar -> Some(Lazy(s8)), // 'g'
          0x68.toChar -> Some(Lazy(s8)), // 'h'
          0x69.toChar -> Some(Lazy(s8)), // 'i'
          0x6a.toChar -> Some(Lazy(s8)), // 'j'
          0x6b.toChar -> Some(Lazy(s8)), // 'k'
          0x6c.toChar -> Some(Lazy(s8)), // 'l'
          0x6d.toChar -> Some(Lazy(s8)), // 'm'
          0x6e.toChar -> Some(Lazy(s8)), // 'n'
          0x6f.toChar -> Some(Lazy(s8)), // 'o'
          0x70.toChar -> Some(Lazy(s8)), // 'p'
          0x71.toChar -> Some(Lazy(s8)), // 'q'
          0x72.toChar -> Some(Lazy(s8)), // 'r'
          0x73.toChar -> Some(Lazy(s8)), // 's'
          0x74.toChar -> Some(Lazy(s8)), // 't'
          0x75.toChar -> Some(Lazy(s8)), // 'u'
          0x76.toChar -> Some(Lazy(s8)), // 'v'
          0x77.toChar -> Some(Lazy(s8)), // 'w'
          0x78.toChar -> Some(Lazy(s8)), // 'x'
          0x79.toChar -> Some(Lazy(s8)), // 'y'
          0x7a.toChar -> Some(Lazy(s8)), // 'z'
        ),
        elseTransition = None,
        yields = None,
      )
    lazy val s1: Dfa.State[Token] =
      Dfa.State(
        id = 1,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s5)), // '0'
          0x31.toChar -> Some(Lazy(s5)), // '1'
          0x32.toChar -> Some(Lazy(s5)), // '2'
          0x33.toChar -> Some(Lazy(s5)), // '3'
          0x34.toChar -> Some(Lazy(s5)), // '4'
          0x35.toChar -> Some(Lazy(s5)), // '5'
          0x36.toChar -> Some(Lazy(s5)), // '6'
          0x37.toChar -> Some(Lazy(s5)), // '7'
          0x38.toChar -> Some(Lazy(s5)), // '8'
          0x39.toChar -> Some(Lazy(s5)), // '9'
        ),
        elseTransition = None,
        yields = None,
      )
    lazy val s2: Dfa.State[Token] =
      Dfa.State(
        id = 2,
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
    lazy val s3: Dfa.State[Token] =
      Dfa.State(
        id = 3,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s4: Dfa.State[Token] =
      Dfa.State(
        id = 4,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s17)()),
      )
    lazy val s5: Dfa.State[Token] =
      Dfa.State(
        id = 5,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s5)), // '0'
          0x31.toChar -> Some(Lazy(s5)), // '1'
          0x32.toChar -> Some(Lazy(s5)), // '2'
          0x33.toChar -> Some(Lazy(s5)), // '3'
          0x34.toChar -> Some(Lazy(s5)), // '4'
          0x35.toChar -> Some(Lazy(s5)), // '5'
          0x36.toChar -> Some(Lazy(s5)), // '6'
          0x37.toChar -> Some(Lazy(s5)), // '7'
          0x38.toChar -> Some(Lazy(s5)), // '8'
          0x39.toChar -> Some(Lazy(s5)), // '9'
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
    lazy val s6: Dfa.State[Token] =
      Dfa.State(
        id = 6,
        transitions = Map(
          0xa.toChar -> Some(Lazy(s3)), // '\n'
        ),
        elseTransition = Some(Lazy(s6)),
        yields = None,
      )
    lazy val s7: Dfa.State[Token] =
      Dfa.State(
        id = 7,
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
    lazy val s8: Dfa.State[Token] =
      Dfa.State(
        id = 8,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s8)), // '0'
          0x31.toChar -> Some(Lazy(s8)), // '1'
          0x32.toChar -> Some(Lazy(s8)), // '2'
          0x33.toChar -> Some(Lazy(s8)), // '3'
          0x34.toChar -> Some(Lazy(s8)), // '4'
          0x35.toChar -> Some(Lazy(s8)), // '5'
          0x36.toChar -> Some(Lazy(s8)), // '6'
          0x37.toChar -> Some(Lazy(s8)), // '7'
          0x38.toChar -> Some(Lazy(s8)), // '8'
          0x39.toChar -> Some(Lazy(s8)), // '9'
          0x41.toChar -> Some(Lazy(s8)), // 'A'
          0x42.toChar -> Some(Lazy(s8)), // 'B'
          0x43.toChar -> Some(Lazy(s8)), // 'C'
          0x44.toChar -> Some(Lazy(s8)), // 'D'
          0x45.toChar -> Some(Lazy(s8)), // 'E'
          0x46.toChar -> Some(Lazy(s8)), // 'F'
          0x47.toChar -> Some(Lazy(s8)), // 'G'
          0x48.toChar -> Some(Lazy(s8)), // 'H'
          0x49.toChar -> Some(Lazy(s8)), // 'I'
          0x4a.toChar -> Some(Lazy(s8)), // 'J'
          0x4b.toChar -> Some(Lazy(s8)), // 'K'
          0x4c.toChar -> Some(Lazy(s8)), // 'L'
          0x4d.toChar -> Some(Lazy(s8)), // 'M'
          0x4e.toChar -> Some(Lazy(s8)), // 'N'
          0x4f.toChar -> Some(Lazy(s8)), // 'O'
          0x50.toChar -> Some(Lazy(s8)), // 'P'
          0x51.toChar -> Some(Lazy(s8)), // 'Q'
          0x52.toChar -> Some(Lazy(s8)), // 'R'
          0x53.toChar -> Some(Lazy(s8)), // 'S'
          0x54.toChar -> Some(Lazy(s8)), // 'T'
          0x55.toChar -> Some(Lazy(s8)), // 'U'
          0x56.toChar -> Some(Lazy(s8)), // 'V'
          0x57.toChar -> Some(Lazy(s8)), // 'W'
          0x58.toChar -> Some(Lazy(s8)), // 'X'
          0x59.toChar -> Some(Lazy(s8)), // 'Y'
          0x5a.toChar -> Some(Lazy(s8)), // 'Z'
          0x5f.toChar -> Some(Lazy(s8)), // '_'
          0x61.toChar -> Some(Lazy(s8)), // 'a'
          0x62.toChar -> Some(Lazy(s8)), // 'b'
          0x63.toChar -> Some(Lazy(s8)), // 'c'
          0x64.toChar -> Some(Lazy(s8)), // 'd'
          0x65.toChar -> Some(Lazy(s8)), // 'e'
          0x66.toChar -> Some(Lazy(s8)), // 'f'
          0x67.toChar -> Some(Lazy(s8)), // 'g'
          0x68.toChar -> Some(Lazy(s8)), // 'h'
          0x69.toChar -> Some(Lazy(s8)), // 'i'
          0x6a.toChar -> Some(Lazy(s8)), // 'j'
          0x6b.toChar -> Some(Lazy(s8)), // 'k'
          0x6c.toChar -> Some(Lazy(s8)), // 'l'
          0x6d.toChar -> Some(Lazy(s8)), // 'm'
          0x6e.toChar -> Some(Lazy(s8)), // 'n'
          0x6f.toChar -> Some(Lazy(s8)), // 'o'
          0x70.toChar -> Some(Lazy(s8)), // 'p'
          0x71.toChar -> Some(Lazy(s8)), // 'q'
          0x72.toChar -> Some(Lazy(s8)), // 'r'
          0x73.toChar -> Some(Lazy(s8)), // 's'
          0x74.toChar -> Some(Lazy(s8)), // 't'
          0x75.toChar -> Some(Lazy(s8)), // 'u'
          0x76.toChar -> Some(Lazy(s8)), // 'v'
          0x77.toChar -> Some(Lazy(s8)), // 'w'
          0x78.toChar -> Some(Lazy(s8)), // 'x'
          0x79.toChar -> Some(Lazy(s8)), // 'y'
          0x7a.toChar -> Some(Lazy(s8)), // 'z'
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
    lazy val s9: Dfa.State[Token] =
      Dfa.State(
        id = 9,
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
    lazy val s10: Dfa.State[Token] =
      Dfa.State(
        id = 10,
        transitions = Map(
          0x2a.toChar -> Some(Lazy(s4)), // '*'
          0x2f.toChar -> Some(Lazy(s6)), // '/'
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
    lazy val s11: Dfa.State[Token] =
      Dfa.State(
        id = 11,
        transitions = Map(
          0x2f.toChar -> Some(Lazy(s14)), // '/'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s17)()),
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
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s17)()),
      )
    lazy val s14: Dfa.State[Token] =
      Dfa.State(
        id = 14,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yields(s0)()),
      )
    lazy val s15: Dfa.State[Token] =
      Dfa.State(
        id = 15,
        transitions = Map(
          0x2e.toChar -> Some(Lazy(s1)), // '.'
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
    lazy val s16: Dfa.State[Token] =
      Dfa.State(
        id = 16,
        transitions = Map(),
        elseTransition = None,
        yields = Some(
          Dfa.State.Yields(s0)(
            Dfa.State.Yields.Yield(
              tokF = Token.raw.apply,
              spanRange = (0, -1),
            ),
          ),
        ),
      )
    lazy val s17: Dfa.State[Token] =
      Dfa.State(
        id = 17,
        transitions = Map(
          0x2a.toChar -> Some(Lazy(s11)), // '*'
        ),
        elseTransition = Some(Lazy(s13)),
        yields = None,
      )
    lazy val s18: Dfa.State[Token] =
      Dfa.State(
        id = 18,
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

    Dfa(s0)
  }

  {
    val source = Source.fromFile("res-test/calc/samples/ex1.txt")
    val str = source.mkString
    source.close

    dfa.parse(str) match {
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
