package slyce.lexer.dfa.runtime

import slyce.tree.GeneralToken

class State[T <: GeneralToken](val id: Int, transitionMap: TransitionMap[T], val action: Option[Action[T]]) {

  def apply(char: Char): Option[State[T]] =
    transitionMap(char)

}
