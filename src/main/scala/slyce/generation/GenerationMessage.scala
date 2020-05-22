package slyce.generation

import org.scalactic.source.Position

import klib.handling.Message
import klib.handling.MessageAccumulator

sealed trait GenerationMessage extends Message

object GenerationMessage {

  type ??[T] =
    MessageAccumulator[GenerationMessage, T]

  // =====| ... |=====

  case class NoSuchModeToTransitionTo(lineNo: Int, modeName: String)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Unable to transition to non-existent mode '$modeName' from regex on line #$lineNo"
  }

  case class DuplicateModeIgnored(modeName: String)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Mode $modeName was duplicated, and will be ignored"
  }

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
