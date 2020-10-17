package slyce.implementations.generation.lexer

import scalaz.\/

import slyce.architecture.generation.lexer.{Lexer => LexerF}

object Lexer extends LexerF[Data, Err, Dfa] {

  override def apply(input: Data): Err \/ Dfa =
    LexerF.apply(DataToNfa, NfaToDfa)(input)

}
