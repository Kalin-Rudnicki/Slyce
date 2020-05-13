package slyce.lexer.nfa

import slyce.lexer.Action

import scala.collection.mutable.{ListBuffer => MList}
import slyce.tree.GeneralToken
import slyce.tree.GeneralToken.Stats

class State[T <: GeneralToken](val id: Int) {
  
  val transitions: TransitionMap[T] = new TransitionMap()
  val actions: MList[Action[T]] = MList()
  
  
  def @@(lineNo: Int): PartialAction =
    new PartialAction(lineNo)
  
  def +(chars: List[Char]): PartialTransition =
    new PartialTransition(chars)
  
  def <+>(other: State[T]): Unit =
    transitions <+> other
  
  
  class PartialTransition(val chars: List[Char]) {
    
    def >>(toState: State[T]): Unit =
      transitions << (CharClass.Only(chars), toState)
    
  }
  
  class PartialAction(val lineNo: Int) {
    
    def >>(action: Stats => List[T]): Unit =
      actions.append(Action(lineNo, action))
    
  }
  
}
