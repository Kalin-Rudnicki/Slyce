package slyce.lexer.nfa

sealed trait CharClass {
  
}

object CharClass {
  
  // =====| CharClass |=====
  
  case class Only(chars: Set[Char]) extends CharClass
  
  case class Except(chars: Set[Char]) extends CharClass
  
  // =====| Helpers |=====
  
  def only(chars: Char*): CharClass =
    Only(chars.toSet)
  
  def only(chars: String): CharClass =
    Only(chars.toCharArray.toSet)
  
  def except(chars: Char*): CharClass =
    Except(chars.toSet)
  
  def except(chars: String): CharClass =
    Except(chars.toCharArray.toSet)
  
  def exceptAscii(chars: Char*): CharClass =
    Only(0.toChar.to(127.toChar).toSet) - Only(chars.toSet)
  
  def exceptAscii(chars: String): CharClass =
    Only(0.toChar.to(127.toChar).toSet) - Only(chars.toCharArray.toSet)
  
}
