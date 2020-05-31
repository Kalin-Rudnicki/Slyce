package slyce.generation.raw.lexer.nfa

import scala.language.implicitConversions

import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption

import klib.fp.instances.{given _}
import klib.fp.ops.{given _}
import slyce.generation.GenerationMessage._
import slyce.generation.TokenSpec
import slyce.generation.generated.lexer.dfa

case class Action(lineNo: Int, tokenSpecs: List[TokenSpec], mode: Option[String]) {

  def <|<(t: List[TokenSpec]): Action =
    new Action(lineNo, t, mode)

  def >|>(m: String): Action =
    new Action(lineNo, tokenSpecs, Some(m))

  def toDfaAction(myHeadState: dfa.State, stateHeads: Map[String, (Mode, dfa.State)]): ??[dfa.Action] =
    mode match {
      case None =>
        dfa.Action(lineNo, tokenSpecs, myHeadState).lift[??]
      case Some(m) =>
        stateHeads
          .get(m)
          .cata(
            r => dfa.Action(lineNo, tokenSpecs, r._2).lift[??],
            Fatal.NoSuchModeToTransitionTo(lineNo, m).dead
          )
    }

}

object Action {

  implicit def intToAction(i: Int): Action =
    new Action(i, Nil, None)

  def join(actions: List[Action]): Option[(Action, List[Action])] =
    actions.sortWith((_1, _2) => _1.lineNo < _2.lineNo) match {
      case Nil =>
        None
      case head :: tail =>
        (head, tail).some
    }

}
