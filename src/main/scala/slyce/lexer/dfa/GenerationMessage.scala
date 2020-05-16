package slyce.lexer.dfa

sealed trait GenerationMessage {
  def message: String
}

object GenerationMessage {

  case class RegexCompleteOnNoInput(lineNo: Int) extends GenerationMessage {
    override def message: String =
      s"Regex on line #$lineNo can be satisfied with no input"
  }

  case class RegexIsCompletelyShadowed(lineNo: Int, shadowingLines: List[Int])
      extends GenerationMessage {
    override def message: String =
      s"Regex on line #$lineNo is completely shadowed by regexes on lines: ${shadowingLines.mkString(", ")}"
  }

}
