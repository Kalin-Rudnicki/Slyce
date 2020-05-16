package slyce.lexer.nfa

import slyce.tree.GeneralToken

class Mode[T <: GeneralToken](val name: String) {

  private var counter: Int = 0
  val initialState: State[T] = new State(this, 0)

  def newState: State[T] = {
    counter += 1
    new State(this, counter)
  }

  def start: State[T] = {
    val ns: State[T] = newState
    initialState |== ns
    ns
  }

}

object Mode {

  implicit def modeToNewState[T <: GeneralToken](mode: Mode[T]): State[T] =
    mode.start

}
