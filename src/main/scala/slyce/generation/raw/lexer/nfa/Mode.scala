package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import todo_move_tree.GeneralToken

class Mode[T <: GeneralToken](val name: String) {

  private var counter: Int = -1
  private val states: MList[State[T]] = MList()
  val initialState: State[T] = newState

  def newState: State[T] = {
    counter += 1
    val ns: State[T] = new State(this, counter)
    states.append(ns)
    ns
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
