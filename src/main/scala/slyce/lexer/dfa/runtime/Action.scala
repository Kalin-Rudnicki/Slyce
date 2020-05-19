package slyce.lexer.dfa.runtime

import slyce.tree.GeneralToken

// TODO (KR) : Really a function from (TBD) => T
class Action[T <: GeneralToken](val lineNo: Int, toToks: GeneralToken.Stats => List[T], toMode: Mode[T]) {

  def apply(stats: GeneralToken.Stats): (List[T], Mode[T]) =
    (toToks(stats), toMode)

}
