package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import todo_move_tree.GeneralToken

class Mode(val name: String) {

  private var counter: Int = -1
  private val states: MList[State] = MList()
  val initialState: State = newState

  def newState: State = {
    counter += 1
    val ns: State = new State(this, counter)
    states.append(ns)
    ns
  }

  def start: State = {
    val ns: State = newState
    initialState |== ns
    ns
  }

}

object Mode {

  implicit def modeToNewState[T <: GeneralToken](mode: Mode): State =
    mode.start

}
