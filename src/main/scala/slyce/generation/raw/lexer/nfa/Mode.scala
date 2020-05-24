package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

import scalaz.Scalaz._

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
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

  def compile(myStart: dfa.State, modes: Map[String, (Mode, dfa.State)]): ??[Unit] = {
    var counter: Int = 0
    val cache: MMap[Set[State], dfa.State] = MMap()
    val shadowMap: MMap[Int, MSet[Int]] = MMap()
    val joinedStart: Set[State] = State.epsilons(Set(initialState), _.transitions.epsilonTransitions.toSet)

    def join(
        states: Set[State],
        dfaState: dfa.State
    ): ??[dfa.State] = {
      cache.put(states, dfaState)
      for {
        tmp <- TransitionMap.preJoin(states.map(_.transitions))
        (charToStateSet, epsilonSet) = tmp
        epsilon <- epsilonSet.isEmpty.fold(
          Alive(None),
          loop(epsilonSet).map(Some(_))
        )
        listCharState <-
          charToStateSet
            ._map(loop)
            .toList
            .map(t => t._2.map(s => (t._1, s)))
            ._invert
        mapCharState = listCharState.toMap
        oActions = Action.join(states.toList.flatMap(_.actions))
        dfaOActions <- oActions.map(oA => oA._1.toDfaAction(myStart, modes).map((_, oA._2))) match {
          case None =>
            Alive(None)
          case Some(a) =>
            a.map(Some(_))
        }
      } yield {
        dfaState.transitions = new dfa.TransitionMap(mapCharState, epsilon)
        dfaState.action = dfaOActions match {
          case None =>
            None
          case Some((action, Nil)) =>
            Some(action)
          case Some((action, shadowed)) =>
            shadowMap.getOrElseUpdate(action.lineNo, MSet()).addAll(shadowed.map(_.lineNo))
            Some(action)
        }
        dfaState
      }
    }

    def loop(preGrouped: Set[State]): ??[dfa.State] = {
      val grouped: Set[State] = State.epsilons(preGrouped, _.transitions.epsilonTransitions.toSet)
      cache.get(grouped) match {
        case Some(dfaState) =>
          Alive(dfaState)
        case None =>
          counter += 1
          val dfaState = join(grouped, new dfa.State(name, counter))
          dfaState.forEach(cache.put(grouped, _))
          dfaState
      }
    }

    val shadowedBy: List[(Int, Set[Int])] =
      shadowMap.toSet
        .flatMap((t: (Int, MSet[Int])) => t._2.toSet.map((_t: Int) => (_t, t._1)))
        .groupMap(_._1)(_._2)
        .toList

    for {
      res0 <- join(joinedStart, myStart)
      res1 <- Alive(res0, shadowedBy.map(t => CompletelyShadowedRegex(t._1, t._2.toList)): _*).asInstanceOf[??[dfa.State]]
      accessibleStates = State.epsilons(
        Set(initialState),
        s =>
          s.transitions.epsilonTransitions.toSet | s.transitions.transitions.toSet.flatMap((_s: (Char, MList[State])) =>
            _s._2.toSet
          )
      )
    } yield ()
  }

}

object Mode {

  implicit def modeToNewState(mode: Mode): State =
    mode.start

}
