package slyce.lexer

sealed trait TokenSpec {
  
  def textRange: (Int, Int)
  
  def spanRange: (Int, Int)
  
}

object TokenSpec {
  
  case class __$(name: String, textRange: (Int, Int) = (0, -1), spanRange: (Int, Int) = (0, -1)) extends TokenSpec
  
  case class __@(textRange: (Int, Int) = (0, -1), spanRange: (Int, Int) = (0, -1)) extends TokenSpec
  
}
