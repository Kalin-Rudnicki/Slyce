package slyce.generation.raw.lexer.nfa

import scalaz.std.option.optionSyntax._

import klib.fp.instances._
import klib.fp.ops._
import slyce.generation.GenerationMessage._
import slyce.generation.GenerationMessage.FatalError
import slyce.generation.TokenSpec
import slyce.generation.generated.lexer.dfa

case class Action(lineNo: Int, tokenSpecs: List[TokenSpec], mode: Option[String]) {

  def <<(t: List[TokenSpec]): Action =
    new Action(lineNo, t, mode)

  def >>(m: String): Action =
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
            FatalError.noSuchModeToTransitionTo(lineNo, m).dead
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
