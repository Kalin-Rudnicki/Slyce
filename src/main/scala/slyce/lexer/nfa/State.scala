package slyce.lexer.nfa

import slyce.lexer.{Action, TokenSpec}

import scala.collection.mutable.{ListBuffer => MList}
import slyce.tree.GeneralToken
import slyce.lexer.nfa.helpers.{ActionHelper, PartialTransition}
import slyce.lexer.regex.CharClass

class State[T <: GeneralToken](val mode: Mode[T], val id: Int) {
  
  val transitions: TransitionMap[T] = new TransitionMap()
  val actions: MList[Action] = MList()
  
  // =====| Epsilon Transition |=====
  
  def ~=(other: State[T]): Unit =
    transitions ~= other
  
  // =====| Transitions |=====
  
  def +(chars: Set[Char]): PartialTransition[T] =
    new PartialTransition[T](this, chars)
  
  def <<(chars: Set[Char]): State[T] =
    this <<< CharClass.Only(chars)
  
  def <<(str: String): State[T] =
    str.foldLeft(this)((s, c) => s << Set(c))
  
  def <<!(chars: Set[Char]): State[T] =
    this <<< CharClass.Except(chars)
  
  def <<<(charClass: CharClass): State[T] = {
    val ns: State[T] = mode.newState
    transitions << (charClass, ns)
    ns
  }
  
  // =====| Actions |=====
  
  def >>(actionHelper: ActionHelper): Unit =
    this.actions.append(actionHelper.action)
  
}
