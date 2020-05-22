package slyce.generation.raw.lexer.nfa

import scalaz.std.option.optionSyntax._

import klib.handling.Implicits._
import slyce.generation.GenerationMessage._
import slyce.generation.TokenSpec
import slyce.generation.generated.lexer.dfa

case class Action(lineNo: Int, tokenSpecs: List[TokenSpec], mode: Option[String]) {

  def <<(t: List[TokenSpec]): Action =
    new Action(lineNo, t, mode)

  def >>(m: String): Action =
    new Action(lineNo, tokenSpecs, Some(m))

  def toDfaAction(myHeadState: dfa.State, stateHeads: Map[String, dfa.State]): ??[dfa.Action] =
    mode match {
      case None =>
        dfa.Action(lineNo, tokenSpecs, myHeadState)
      case Some(m) =>
        stateHeads
          .get(m)
          .cata(
            dfa.Action(lineNo, tokenSpecs, _),
            NoSuchModeToTransitionTo(lineNo, m)
          )
    }

}

object Action {

  implicit def intToAction(i: Int): Action =
    new Action(i, Nil, None)

}
