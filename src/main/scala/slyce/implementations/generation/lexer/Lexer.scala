package slyce.implementations.generation.lexer

import scalaz.\/

import slyce.architecture.generation.{lexer => arch}

object Lexer extends arch.Lexer[Data, Err, Dfa] {

  override def apply(input: Data): Err \/ Dfa =
    arch.Lexer.apply(DataToNfa, NfaToDfa)(input)

}
