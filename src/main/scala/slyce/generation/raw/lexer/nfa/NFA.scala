package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import scalaz.Scalaz.ToBooleanOpsFromBoolean

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
import slyce.generation.GenerationMessage._
import slyce.generation.generated.lexer.dfa
import slyce.generation.generated.lexer.dfa.DFA

class NFA private (initialModeName: String) {

  val initialMode: Mode = new Mode(initialModeName)
  val modes: MList[Mode] = MList(initialMode)

  def newMode(name: String): Mode = {
    val mode: Mode = new Mode(name)
    modes.append(mode)
    mode
  }

  def compile: ??[DFA] = {
    for {
      strModeMap <- modes.toList.foldLeft[??[Map[String, Mode]]](
        Alive(Map())
      ) { (map_?, mode) =>
        map_?.flatMap { map =>
          map
            .contains(mode.name)
            .fold(
              Alive(map + ((mode.name, mode))),
              Alive(map) << NonFatal.DuplicateModeIgnored(mode.name)
            )
        }
      }
      // TODO (KR) : Possible place for optimizations
      strModeStateMap = strModeMap._map(m => (m, new dfa.State(m.name, 0)))
      _ <- strModeStateMap.values.toList.map(ms => ms._1.compile(ms._2, strModeStateMap)).invert
      initialState = strModeStateMap(initialModeName)._2
    } yield new DFA(initialState)
  }

}

object NFA {

  def apply(initialModeName: String = "main"): ??[NFA] =
    if (initialModeName.isEmpty || !initialModeName.matches("^[A-Za-z][A-Za-z0-9_]*$"))
      Fatal.BadModeName(initialModeName).dead
    else
      new NFA(initialModeName).lift[??]

}
