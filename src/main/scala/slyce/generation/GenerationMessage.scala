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

  object Fatal {

    trait Message extends GenerationMessage

    case class BasicMessage(message: String)(implicit val pos: Position) extends Message

    def canNotJoinEmptyMaps(implicit pos: Position): Message =
      BasicMessage("Can not join empty set of transition maps")

    case class NoSuchModeToTransitionTo(lineNo: Int, modeName: String)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Unable to transition to non-existent mode '$modeName' from regex on line #$lineNo"
    }

    case class EmptyStringCompletesRegex(lineNo: Int)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Regex on line #$lineNo can be satisfied with no input"
    }

    // TODO (KR) : lineNo?
    case class RepeatMinNegative(min: Int)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Tried to repeat a regex a negative ($min) amount of times"
    }

    // TODO (KR) : lineNo?
    case class RepeatMaxNonPositive(max: Int)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Tried to repeat a regex a non-positive ($max) amount of times"
    }

    // TODO (KR) : lineNo?
    case class RepeatMaxMin(min: Int, max: Int)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Tried to repeat a regex where max times ($max) was less than min times ($min)"
    }

    // TODO (KR) : lineNo?
    case class BadModeName(name: String)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Bad mode name: '$name'"
    }

    case class BadElementName(name: String)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Bad element name: $name"
    }

    case class NtAlreadyExists(name: String)(implicit val pos: Position) extends Message {
      override def message: String =
        s"NonTerminal already exists: $name"
    }

  }

  // =====| NonFatal |=====

  object NonFatal {

    trait Message extends GenerationMessage

    case class BasicMessage(message: String)(implicit val pos: Position) extends Message

    case class DuplicateModeIgnored(modeName: String)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Mode $modeName was duplicated, and will be ignored"
    }

    case class DuplicateNTIgnored(ntName: String)(implicit val pos: Position) extends Message {
      override def message: String =
        s"Mode $ntName was duplicated, and will be ignored"
    }

    case class CompletelyShadowedRegex(lineNo: Int, shadowingLines: List[Int])(implicit val pos: Position) extends Message {
      override def message: String =
        s"Regex on line #$lineNo is completely shadowed by regexes on lines: ${shadowingLines.mkString(", ")}"
    }

    case class InaccessibleNfaState(mode: String, stateNo: Int)(implicit val pos: Position) extends Message {
      override def message: String =
        s"State #$stateNo in mode $mode is inaccessible"

    }

  }

}
