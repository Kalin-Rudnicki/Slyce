package slyce.generation

import org.scalactic.source.Position

import klib.handling.Message
import klib.handling.MessageAccumulator

sealed trait GenerationMessage extends Message {

  override def toString(): String =
    message

}

object GenerationMessage {

  type ??[T] =
    MessageAccumulator[GenerationMessage, T]

  // =====| Fatal |=====

  case class FatalError(message: String)(implicit val pos: Position) extends GenerationMessage

  object FatalError {

    def canNotJoinEmptyMaps(implicit pos: Position): GenerationMessage =
      FatalError("Can not join empty set of transition maps")

    def noSuchModeToTransitionTo(lineNo: Int, modeName: String)(implicit pos: Position): GenerationMessage =
      FatalError(s"Unable to transition to non-existent mode '$modeName' from regex on line #$lineNo")

    def emptyStringCompletesRegex(lineNo: Int)(implicit pos: Position): GenerationMessage =
      FatalError(s"Regex on line #$lineNo can be satisfied with no input")

    // TODO (KR) : lineNo?
    def repeatMinNegative(min: Int)(implicit pos: Position): GenerationMessage =
      FatalError(s"Tried to repeat a regex a negative ($min) amount of times")

    def repeatMaxNonPositive(max: Int)(implicit pos: Position): GenerationMessage =
      FatalError(s"Tried to repeat a regex a non-positive ($max) amount of times")

    def repeatMaxMin(min: Int, max: Int)(implicit pos: Position): GenerationMessage =
      FatalError(s"Tried to repeat a regex where max times ($max) was less than min times ($min)")

    def badModeName(name: String)(implicit pos: Position): GenerationMessage =
      FatalError(s"Bad mode name: '$name'")

  }

  // =====| NonFatal |=====

  case class GenericWarning(message: String)(implicit val pos: Position) extends GenerationMessage

  case class DuplicateModeIgnored(modeName: String)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"Mode $modeName was duplicated, and will be ignored"
  }

  case class CompletelyShadowedRegex(lineNo: Int, shadowingLines: List[Int])(implicit val pos: Position)
      extends GenerationMessage {
    override def message: String =
      s"Regex on line #$lineNo is completely shadowed by regexes on lines: ${shadowingLines.mkString(", ")}"
  }

  case class InaccessibleNfaState(mode: String, stateNo: Int)(implicit val pos: Position) extends GenerationMessage {
    override def message: String =
      s"State #$stateNo in mode $mode is inaccessible"
  }

}
