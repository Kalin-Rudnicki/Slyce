package slyce.lexer.nfa

import slyce.tree.GeneralToken

class Mode[T <: GeneralToken](val name: String) {
  
  private var counter: Int = 0
  val initialState: State[T] = new State(0)
  
  def newState: State[T] = {
    counter += 1
    new State(counter)
  }
  
}
