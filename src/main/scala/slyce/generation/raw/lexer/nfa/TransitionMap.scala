package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}
import scala.collection.mutable.{Map => MMap}

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
import slyce.generation.GenerationMessage._
import slyce.generation.raw.lexer.nfa.Regex.{CharClass => CC}
import slyce.generation.generated.lexer.dfa

class TransitionMap {

  val transitions: MMap[
    Char,
    MList[State]
  ] = MMap()

  val epsilonTransitions: MList[State] = MList()
  // Unspecified has to do with not having to list out every UTF-8 char for [^a-z]
  val unspecified: MList[State] = MList()

  def apply(char: Char): Set[State] = transitions.getOrElse(char, unspecified).toSet

  def <<(charClass: CC, transitionType: State): Unit =
    charClass match {
      case CC.Only(chars) =>
        chars.foreach { c =>
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

  def ~=(other: State): Unit =
    epsilonTransitions.append(other)

}

object TransitionMap {

  def preJoin(maps: Set[TransitionMap]): ??[(Map[Char, Set[State]], Set[State])] =
    if (maps.isEmpty)
      FatalError.canNotJoinEmptyMaps.dead
    else {
      (
        maps.flatMap(_.transitions.keySet).map(c => (c, maps.flatMap(_(c)))).toMap,
        maps.flatMap(_.epsilonTransitions.toSet)
      )._lift[??]
    }

}
