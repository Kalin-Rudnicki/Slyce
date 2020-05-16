package slyce.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import slyce.lexer.Action
import slyce.lexer.nfa.Regex.{CharClass => CC}
import slyce.lexer.nfa.helpers.{ActionHelper, PartialTransition}
import slyce.tree.GeneralToken

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
    this <<< CC.Only(chars)

  def <<(str: String): State[T] =
    str.foldLeft(this)((s, c) => s << Set(c))

  def <<!(chars: Set[Char]): State[T] =
    this <<< CC.Except(chars)

  def <<<(charClass: CC): State[T] = {
    val ns: State[T] = mode.newState
    transitions << (charClass, ns)
    ns
  }

  // =====| Actions |=====

  def >>(actionHelper: ActionHelper): Unit =
    this.actions.append(actionHelper.action)

}
