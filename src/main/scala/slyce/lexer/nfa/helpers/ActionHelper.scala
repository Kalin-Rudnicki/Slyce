package slyce.lexer.nfa.helpers

import slyce.lexer.Action._
import slyce.lexer.{TokenSpec, Action => Act}

/*
      TODO : Could maybe use some work, but its just nicities to use by hand
 */
class ActionHelper(val action: Act) {

  def +(tokenSpecs: List[TokenSpec]): ActionHelper =
    new ActionHelper(
      action match {
        case Action(ln) =>
          ActionTokens(ln, tokenSpecs)
        case ActionTokens(ln, ts) =>
          ActionTokens(ln, ts ::: tokenSpecs)
        case ActionMode(ln, m) =>
          ActionTokensMode(ln, tokenSpecs, m)
        case ActionTokensMode(ln, ts, m) =>
          ActionTokensMode(ln, ts ::: tokenSpecs, m)
      }
    )

  def +(mode: String): ActionHelper =
    this + Some(mode)

  def +(mode: Option[String]): ActionHelper =
    new ActionHelper(
      action match {
        case Action(ln) =>
          ActionMode(ln, mode)
        case ActionTokens(ln, ts) =>
          ActionTokensMode(ln, ts, mode)
        case ActionMode(ln, _) =>
          ActionMode(ln, mode)
        case ActionTokensMode(ln, ts, _) =>
          ActionTokensMode(ln, ts, mode)
      }
    )

}
