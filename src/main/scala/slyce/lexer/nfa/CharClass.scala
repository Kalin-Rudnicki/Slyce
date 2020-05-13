package slyce.lexer.nfa

sealed trait CharClass

object CharClass {
  
  case class Unmatched() extends CharClass
  
  case class Only(chars: List[Char]) extends CharClass
  
  case class Except(chars: List[Char]) extends CharClass
  
}
