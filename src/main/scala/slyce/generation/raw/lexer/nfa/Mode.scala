package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

import scalaz.std.option.optionSyntax._
import todo_move_tree.GeneralToken

import klib.handling.Implicits._
import slyce.generation.GenerationMessage._
import slyce.generation.generated.lexer.dfa

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

  def compile(myStart: dfa.State, modes: Map[String, dfa.State]): ??[Unit] = {
    var counter: Int = 0
    val cache: MMap[Set[State], dfa.State] = MMap()
    val shadowMap: MMap[Action, MSet[Action]] = MMap()
    val joinedStart: Set[State] = epsilons(Set(initialState))

    def epsilons(states: Set[State]): Set[State] = {
      def loop(state: State): Set[State] =
        Set(state) ++ state.transitions.epsilonTransitions.toSet.flatMap(loop)

      states.flatMap(loop).filter(!_.trivial_?)
    }

    // TODO (KR) : Implement
    def join(dfaState: dfa.State, states: Set[State]): ??[dfa.State] = {
      // TODO (KR) : Implement
      val transitionMap: ??[dfa.TransitionMap] = ???
      val action: ??[Option[Action]] =
        states.flatMap(_.actions.toList).toList.sortBy(_.lineNo) match {
          case Nil =>
            None
          case head :: Nil =>
            head.some
          case head :: tail =>
            shadowMap.getOrElseUpdate(head, MSet()).addAll(tail)
            head.some
        }

      transitionMap
        .flatMap { tm =>
          dfaState.transitions = tm
          action.flatMap { a =>
            a.map(_.toDfaAction(myStart, modes))
          }
        }
        .map { _ =>
          dfaState
        }
    }

    def loop(preGrouped: Set[State]): ??[dfa.State] = {
      val grouped: Set[State] = epsilons(preGrouped)
      cache.get(grouped) match {
        case Some(dfaState) =>
          dfaState
        case None =>
          counter += 1
          val dfaState = join(new dfa.State(name, counter), grouped)
          dfaState.forEach(cache.put(grouped, _))
          dfaState
      }

    }

    val res: ??[dfa.State] = join(myStart, joinedStart)

    validate(myStart)
  }

  // TODO (KR) : Implement
  def validate(dfaStateStart: dfa.State): ??[Unit] = ???

}

object Mode {

  implicit def modeToNewState[T <: GeneralToken](mode: Mode): State =
    mode.start

}
