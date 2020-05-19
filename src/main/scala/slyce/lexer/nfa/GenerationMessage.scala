package slyce.lexer.nfa

import klib.handling.Message
import org.scalactic.source.Position

sealed trait GenerationMessage extends Message

object GenerationMessage {

  case class EmptyStringCompletesRegex(lineNo: Int)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Regex on line #$lineNo can be satisfied with no input"
  }

  case class CompletelyShadowedRegex(lineNo: Int, shadowingLines: List[Int])(implicit val pos: Position)
      extends GenerationMessage {
    override def message: String =
      s"Regex on line #$lineNo is completely shadowed by regexes on lines: ${shadowingLines.mkString(", ")}"
  }

  case class RepeatMinNegative(min: Int)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Tried to repeat a regex a negative ($min) amount of times"
  }

  case class RepeatMaxNonPositive(max: Int)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Tried to repeat a regex a non-positive ($max) amount of times"
  }

  case class RepeatMaxMin(min: Int, max: Int)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Tried to repeat a regex where max times ($max) was less than min times ($min)"
  }

}
