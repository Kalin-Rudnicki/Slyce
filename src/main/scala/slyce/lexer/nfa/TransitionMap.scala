package slyce.lexer.nfa

import slyce.lexer.nfa.Regex.{CharClass => CC}
import slyce.tree.GeneralToken

import scala.collection.mutable.{ListBuffer => MList, Map => MMap}

class TransitionMap[T <: GeneralToken] {
  
  val transitions: MMap[
    Char,
    MList[State[T]]
  ] = MMap()
  
  val epsilonTransitions: MList[State[T]] = MList()
  val unspecified: MList[State[T]] = MList()
  
  def <<(charClass: CC, transitionType: State[T]): Unit = charClass match {
    case CC.Only(chars) =>
      chars.foreach {
        c =>
          transitions
            .getOrElseUpdate(c, unspecified.clone)
            .append(transitionType)
      }
    case CC.Except(chars) =>
      transitions.foreach {
        case (k, v) =>
          if (!chars.contains(k))
            v.append(transitionType)
      }
      unspecified.append(transitionType)
  }
  
  def ~= (other: State[T]): Unit =
    epsilonTransitions.append(other)
  
}
