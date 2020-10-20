package slyce.generate.lexer

import scalaz.\/

import slyce.generate.architecture.{lexer => arch}

object Lexer extends arch.Lexer[Data, Err, Dfa] {

  override def apply(input: Data): Err \/ Dfa =
    arch.Lexer.apply(DataToNfa, NfaToDfa)(input)

}
