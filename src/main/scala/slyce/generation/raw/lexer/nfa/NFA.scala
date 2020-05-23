package slyce.generation.raw.lexer.nfa

import scala.collection.mutable.{ListBuffer => MList}

import scalaz.Scalaz._

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
import slyce.generation.GenerationMessage._
import slyce.generation.generated.lexer.dfa
import slyce.generation.generated.lexer.dfa.DFA

class NFA(initialModeName: String = "General") {

  val initialMode: Mode = new Mode(initialModeName)
  val modes: MList[Mode] = MList(initialMode)

  def newMode(name: String): Mode = {
    val mode: Mode = new Mode(name)
    modes.append(mode)
    mode
  }

  def compile: ??[DFA] = {
    val modeMap: ??[Map[String, Mode]] =
      modes.toList.foldLeft[??[Map[String, Mode]]](
        Alive(Map())
      ) { (map_?, mode) =>
        // TODO (KR) : Here
        map_?.flatMap { map =>
          map
            .contains(mode.name)
            .fold(
              Alive(map + ((mode.name, mode))),
              Alive(map) << DuplicateModeIgnored(mode.name)
            )
        }
      }

    val genModeMap: ??[Map[String, (Mode, dfa.State)]] =
      modeMap.map { map =>
        map.map {
          case (k, v) =>
            (
              k,
              (v, new dfa.State(v.name, 0))
            )
        }
      }

    // TODO (KR) : genModeMap.flatMap {}
    ???
  }

}
