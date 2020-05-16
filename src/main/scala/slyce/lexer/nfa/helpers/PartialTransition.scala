package slyce.lexer.nfa.helpers

import slyce.lexer.nfa.Regex.{CharClass => CC}
import slyce.lexer.nfa.State
import slyce.tree.GeneralToken

class PartialTransition[T <: GeneralToken](val state: State[T], val chars: Set[Char]) {

  def >>(toState: State[T]): Unit =
    state.transitions << (CC.Only(chars), toState)

  def !>>(toState: State[T]): Unit =
    state.transitions << (CC.Except(chars), toState)

}
