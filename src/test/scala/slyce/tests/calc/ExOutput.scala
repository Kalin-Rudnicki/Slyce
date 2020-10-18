package slyce.tests.calc

import scala.io.Source

import scalaz.-\/
import scalaz.\/-

import slyce.implementations.parsing._

object ExOutput extends App {

  sealed trait Token extends Dfa.Token
  object Token {
    final case class Text(text: String) extends Token
    final case class _var(text: String) extends Token
    final case class addOp(text: String) extends Token
    final case class float(text: String) extends Token
    final case class int(text: String) extends Token
    final case class multOp(text: String) extends Token
    final case class powOp(text: String) extends Token
  }

  val dfa: Dfa[Token] = {
    lazy val s0: Dfa.State[Token] =
      Dfa.State(
        id = 0,
        transitions = Map(
          0x9.toChar -> Some(Lazy(s14)), // '\t'
          0xa.toChar -> Some(Lazy(s7)), // '\n'
          0x20.toChar -> Some(Lazy(s14)), // ' '
          0x28.toChar -> Some(Lazy(s7)), // '('
          0x29.toChar -> Some(Lazy(s7)), // ')'
          0x2a.toChar -> Some(Lazy(s18)), // '*'
          0x2b.toChar -> Some(Lazy(s15)), // '+'
          0x2d.toChar -> Some(Lazy(s13)), // '-'
          0x2f.toChar -> Some(Lazy(s11)), // '/'
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
          0x3d.toChar -> Some(Lazy(s7)), // '='
          0x5e.toChar -> Some(Lazy(s4)), // '^'
          0x5f.toChar -> Some(Lazy(s16)), // '_'
          0x61.toChar -> Some(Lazy(s16)), // 'a'
          0x62.toChar -> Some(Lazy(s16)), // 'b'
          0x63.toChar -> Some(Lazy(s16)), // 'c'
          0x64.toChar -> Some(Lazy(s16)), // 'd'
          0x65.toChar -> Some(Lazy(s16)), // 'e'
          0x66.toChar -> Some(Lazy(s16)), // 'f'
          0x67.toChar -> Some(Lazy(s16)), // 'g'
          0x68.toChar -> Some(Lazy(s16)), // 'h'
          0x69.toChar -> Some(Lazy(s16)), // 'i'
          0x6a.toChar -> Some(Lazy(s16)), // 'j'
          0x6b.toChar -> Some(Lazy(s16)), // 'k'
          0x6c.toChar -> Some(Lazy(s16)), // 'l'
          0x6d.toChar -> Some(Lazy(s16)), // 'm'
          0x6e.toChar -> Some(Lazy(s16)), // 'n'
          0x6f.toChar -> Some(Lazy(s16)), // 'o'
          0x70.toChar -> Some(Lazy(s16)), // 'p'
          0x71.toChar -> Some(Lazy(s16)), // 'q'
          0x72.toChar -> Some(Lazy(s16)), // 'r'
          0x73.toChar -> Some(Lazy(s16)), // 's'
          0x74.toChar -> Some(Lazy(s16)), // 't'
          0x75.toChar -> Some(Lazy(s16)), // 'u'
          0x76.toChar -> Some(Lazy(s16)), // 'v'
          0x77.toChar -> Some(Lazy(s16)), // 'w'
          0x78.toChar -> Some(Lazy(s16)), // 'x'
          0x79.toChar -> Some(Lazy(s16)), // 'y'
          0x7a.toChar -> Some(Lazy(s16)) // 'z'
        ),
        elseTransition = None,
        yields = None
      )
    lazy val s1: Dfa.State[Token] =
      Dfa.State(
        id = 1,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(_ => None))
      )
    lazy val s2: Dfa.State[Token] =
      Dfa.State(
        id = 2,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s6)(_ => None))
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
          0x39.toChar -> Some(Lazy(s3)) // '9'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.float(s))))
      )
    lazy val s4: Dfa.State[Token] =
      Dfa.State(
        id = 4,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.powOp(s))))
      )
    lazy val s5: Dfa.State[Token] =
      Dfa.State(
        id = 5,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s6)(_ => None))
      )
    lazy val s6: Dfa.State[Token] =
      Dfa.State(
        id = 6,
        transitions = Map(
          0x2a.toChar -> Some(Lazy(s17)) // '*'
        ),
        elseTransition = Some(Lazy(s5)),
        yields = None
      )
    lazy val s7: Dfa.State[Token] =
      Dfa.State(
        id = 7,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.Text(s))))
      )
    lazy val s8: Dfa.State[Token] =
      Dfa.State(
        id = 8,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(_ => None))
      )
    lazy val s9: Dfa.State[Token] =
      Dfa.State(
        id = 9,
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
          0x39.toChar -> Some(Lazy(s3)) // '9'
        ),
        elseTransition = None,
        yields = None
      )
    lazy val s10: Dfa.State[Token] =
      Dfa.State(
        id = 10,
        transitions = Map(
          0x2e.toChar -> Some(Lazy(s9)), // '.'
          0x30.toChar -> Some(Lazy(s10)), // '0'
          0x31.toChar -> Some(Lazy(s10)), // '1'
          0x32.toChar -> Some(Lazy(s10)), // '2'
          0x33.toChar -> Some(Lazy(s10)), // '3'
          0x34.toChar -> Some(Lazy(s10)), // '4'
          0x35.toChar -> Some(Lazy(s10)), // '5'
          0x36.toChar -> Some(Lazy(s10)), // '6'
          0x37.toChar -> Some(Lazy(s10)), // '7'
          0x38.toChar -> Some(Lazy(s10)), // '8'
          0x39.toChar -> Some(Lazy(s10)) // '9'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.int(s))))
      )
    lazy val s11: Dfa.State[Token] =
      Dfa.State(
        id = 11,
        transitions = Map(
          0x2a.toChar -> Some(Lazy(s2)), // '*'
          0x2f.toChar -> Some(Lazy(s12)) // '/'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.multOp(s))))
      )
    lazy val s12: Dfa.State[Token] =
      Dfa.State(
        id = 12,
        transitions = Map(
          0xa.toChar -> Some(Lazy(s8)) // '\n'
        ),
        elseTransition = Some(Lazy(s12)),
        yields = None
      )
    lazy val s13: Dfa.State[Token] =
      Dfa.State(
        id = 13,
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
          0x39.toChar -> Some(Lazy(s10)) // '9'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.addOp(s))))
      )
    lazy val s14: Dfa.State[Token] =
      Dfa.State(
        id = 14,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(_ => None))
      )
    lazy val s15: Dfa.State[Token] =
      Dfa.State(
        id = 15,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.addOp(s))))
      )
    lazy val s16: Dfa.State[Token] =
      Dfa.State(
        id = 16,
        transitions = Map(
          0x30.toChar -> Some(Lazy(s16)), // '0'
          0x31.toChar -> Some(Lazy(s16)), // '1'
          0x32.toChar -> Some(Lazy(s16)), // '2'
          0x33.toChar -> Some(Lazy(s16)), // '3'
          0x34.toChar -> Some(Lazy(s16)), // '4'
          0x35.toChar -> Some(Lazy(s16)), // '5'
          0x36.toChar -> Some(Lazy(s16)), // '6'
          0x37.toChar -> Some(Lazy(s16)), // '7'
          0x38.toChar -> Some(Lazy(s16)), // '8'
          0x39.toChar -> Some(Lazy(s16)), // '9'
          0x41.toChar -> Some(Lazy(s16)), // 'A'
          0x42.toChar -> Some(Lazy(s16)), // 'B'
          0x43.toChar -> Some(Lazy(s16)), // 'C'
          0x44.toChar -> Some(Lazy(s16)), // 'D'
          0x45.toChar -> Some(Lazy(s16)), // 'E'
          0x46.toChar -> Some(Lazy(s16)), // 'F'
          0x47.toChar -> Some(Lazy(s16)), // 'G'
          0x48.toChar -> Some(Lazy(s16)), // 'H'
          0x49.toChar -> Some(Lazy(s16)), // 'I'
          0x4a.toChar -> Some(Lazy(s16)), // 'J'
          0x4b.toChar -> Some(Lazy(s16)), // 'K'
          0x4c.toChar -> Some(Lazy(s16)), // 'L'
          0x4d.toChar -> Some(Lazy(s16)), // 'M'
          0x4e.toChar -> Some(Lazy(s16)), // 'N'
          0x4f.toChar -> Some(Lazy(s16)), // 'O'
          0x50.toChar -> Some(Lazy(s16)), // 'P'
          0x51.toChar -> Some(Lazy(s16)), // 'Q'
          0x52.toChar -> Some(Lazy(s16)), // 'R'
          0x53.toChar -> Some(Lazy(s16)), // 'S'
          0x54.toChar -> Some(Lazy(s16)), // 'T'
          0x55.toChar -> Some(Lazy(s16)), // 'U'
          0x56.toChar -> Some(Lazy(s16)), // 'V'
          0x57.toChar -> Some(Lazy(s16)), // 'W'
          0x58.toChar -> Some(Lazy(s16)), // 'X'
          0x59.toChar -> Some(Lazy(s16)), // 'Y'
          0x5a.toChar -> Some(Lazy(s16)), // 'Z'
          0x5f.toChar -> Some(Lazy(s16)), // '_'
          0x61.toChar -> Some(Lazy(s16)), // 'a'
          0x62.toChar -> Some(Lazy(s16)), // 'b'
          0x63.toChar -> Some(Lazy(s16)), // 'c'
          0x64.toChar -> Some(Lazy(s16)), // 'd'
          0x65.toChar -> Some(Lazy(s16)), // 'e'
          0x66.toChar -> Some(Lazy(s16)), // 'f'
          0x67.toChar -> Some(Lazy(s16)), // 'g'
          0x68.toChar -> Some(Lazy(s16)), // 'h'
          0x69.toChar -> Some(Lazy(s16)), // 'i'
          0x6a.toChar -> Some(Lazy(s16)), // 'j'
          0x6b.toChar -> Some(Lazy(s16)), // 'k'
          0x6c.toChar -> Some(Lazy(s16)), // 'l'
          0x6d.toChar -> Some(Lazy(s16)), // 'm'
          0x6e.toChar -> Some(Lazy(s16)), // 'n'
          0x6f.toChar -> Some(Lazy(s16)), // 'o'
          0x70.toChar -> Some(Lazy(s16)), // 'p'
          0x71.toChar -> Some(Lazy(s16)), // 'q'
          0x72.toChar -> Some(Lazy(s16)), // 'r'
          0x73.toChar -> Some(Lazy(s16)), // 's'
          0x74.toChar -> Some(Lazy(s16)), // 't'
          0x75.toChar -> Some(Lazy(s16)), // 'u'
          0x76.toChar -> Some(Lazy(s16)), // 'v'
          0x77.toChar -> Some(Lazy(s16)), // 'w'
          0x78.toChar -> Some(Lazy(s16)), // 'x'
          0x79.toChar -> Some(Lazy(s16)), // 'y'
          0x7a.toChar -> Some(Lazy(s16)) // 'z'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token._var(s))))
      )
    lazy val s17: Dfa.State[Token] =
      Dfa.State(
        id = 17,
        transitions = Map(
          0x2f.toChar -> Some(Lazy(s1)) // '/'
        ),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s6)(_ => None))
      )
    lazy val s18: Dfa.State[Token] =
      Dfa.State(
        id = 18,
        transitions = Map(),
        elseTransition = None,
        yields = Some(Dfa.State.Yield(s0)(s => Some(Token.multOp(s))))
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
