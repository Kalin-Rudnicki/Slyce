package slyce.lexer.nfa.helpers

import slyce.lexer.nfa.{CharClass, State}
import slyce.tree.GeneralToken

class PartialTransition[T <: GeneralToken](val state: State[T], val chars: Set[Char]) {
  
  def >>(toState: State[T]): Unit =
    state.transitions << (CharClass.Only(chars), toState)
  
  def !>>(toState: State[T]): Unit =
    state.transitions << (CharClass.Except(chars), toState)
  
}
