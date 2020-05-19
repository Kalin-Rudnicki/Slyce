package slyce.generation.raw.lexer.nfa

sealed trait TokenSpec {

  def textRange: (Int, Int)

  def spanRange: (Int, Int)

}

object TokenSpec {

  case class __$(name: String, textRange: (Int, Int) = (0, -1), spanRange: (Int, Int) = (0, -1)) extends TokenSpec

  case class __@(text: Option[String], textRange: (Int, Int) = (0, -1), spanRange: (Int, Int) = (0, -1)) extends TokenSpec

}
