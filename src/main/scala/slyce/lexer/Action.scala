package slyce.lexer

import slyce.tree.GeneralToken
import slyce.tree.GeneralToken.Stats

case class Action[T <: GeneralToken](lineNo: Int, action: Stats => List[T]) {
  
  def apply(stats: Stats): List[T] =
    action(stats)
  
}
