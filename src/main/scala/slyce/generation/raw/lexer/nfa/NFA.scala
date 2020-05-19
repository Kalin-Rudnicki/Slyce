package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import todo_move_tree.GeneralToken

import klib.handling.MessageAccumulator
import slyce.generation.GenerationMessage

class NFA[T <: GeneralToken](initialModeName: String = "General") {

  val initialMode: Mode[T] = new Mode[T](initialModeName)
  val modes: MList[Mode[T]] = MList(initialMode)

  def newMode(name: String): Mode[T] = {
    val mode: Mode[T] = new Mode[T](name)
    modes.append(mode)
    mode
  }

  /**
    * @return (Compiled DFA, List of (Unused line, Lines that override it))
    */
  def compile: MessageAccumulator[GenerationMessage, DFA[T]] = { // TODO (KR) : Reference correct DFA
    // TODO (KR) : Implement
    ???
  }

}
