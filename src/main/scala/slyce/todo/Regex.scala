package slyce.todo

import scala.language.implicitConversions

import scalaz.NonEmptyList

import slyce.todo.Regex._

sealed trait Regex {

  def canBeEmpty_? : Boolean

  // =====| prettyStr |=====

  private val INDENT = "|   "
  private val MARKER = "|-> "

  def prettyStr: String = {
    val buffer: StringBuilder = new StringBuilder
    prettyStr("", buffer)
    buffer.toString
  }

  protected def prettyStr(indent: String, buffer: StringBuilder): Unit = {
    lazy val nextIndent = s"$indent$INDENT"
    this match {
      case g: Group =>
        buffer.append(indent).append(MARKER).append("Group").append('\n')
        g.foreach(_.prettyStr(nextIndent, buffer))
      case s: Sequence =>
        buffer.append(indent).append(MARKER).append("Sequence").append('\n')
        s.foreach(_.prettyStr(nextIndent, buffer))
      case cc: CharClass =>
        buffer.append(indent).append(MARKER).append(cc.toString).append('\n')
      case repeat: Repeat =>
        buffer.append(indent).append(MARKER).append(s"Repeat : ${repeat.rangeStr}").append('\n')
        repeat.regex.prettyStr(nextIndent, buffer)
    }
  }

}

object Regex {

  sealed trait Repeat extends Regex {

    def rangeStr: String =
      this match {
        case Repeat.Between(min, max, _) =>
          s"$min-$max"
        case Repeat.Infinite(min, _) =>
          s"$min-Infinity"
      }

    override def canBeEmpty_? : Boolean =
      min == 0 || regex.canBeEmpty_?

    def min: Int

    def regex: Regex

  }

  object Repeat {

    case class Between(min: Int, max: Int, regex: Regex) extends Repeat {

      override def toString: String =
        s"Repeat.Between($min, $max, $regex)"

    }

    case class Infinite(min: Int, regex: Regex) extends Repeat {

      override def toString: String =
        s"Repeat.Infinite($min, $regex)"

    }

  }

  /**
    * NOTE (KR) : Additions to a group are in reverse order (not as important as Sequence)
    */
  case class Group(options: NonEmptyList[Sequence]) extends Regex {

    override def canBeEmpty_? : Boolean =
      options.list.toList.indexWhere(!_.canBeEmpty_?) == -1

    override def toString: String =
      s"Group(${options.list.toList.reverse.mkString(", ")})"

    def foreach(f: Sequence => Unit): Unit =
      options.reverse.foreach(f)

  }

  /**
    * NOTE (KR) : Additions to a sequence are in reverse order
    */
  case class Sequence(list: List[Regex]) extends Regex {

    override def canBeEmpty_? : Boolean =
      list.isEmpty

    override def toString: String =
      s"Sequence(${list.reverse.mkString(", ")})"

    def foreach(f: Regex => Unit): Unit =
      list.reverse.foreach(f)

  }

  sealed trait CharClass extends Regex {

    override def canBeEmpty_? : Boolean =
      false

  }

  object CharClass {

    // =====| CharClass |=====

    case class Only(chars: Set[Char]) extends CharClass {

      override def toString: String =
        s"Only(${chars.toList.sorted.mkString(", ")})"

    }

    case class Except(chars: Set[Char]) extends CharClass {

      override def toString: String =
        s"Except(${chars.toList.sorted.mkString(", ")})"

    }

  }

}
