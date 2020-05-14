package slyce.lexer.nfa.helpers

import slyce.lexer.Action

object ImplicitHelpers {
  
  implicit def intToActionHelper(i: Int): ActionHelper =
    new ActionHelper(Action.Action(i))
  
  implicit def charToCharList(char: Char): Set[Char] =
    Set(char)
  
}
