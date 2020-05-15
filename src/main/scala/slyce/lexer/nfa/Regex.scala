package slyce.lexer.nfa

import scalaz.NonEmptyList
import slyce.lexer.nfa.Regex.{CharClass => CC}

sealed trait Regex {
  
  import Regex._
  
  def * (repeat: (Int, Option[Int])): Regex =
    repeat match {
      case (min, None) =>
        Repeat.Infinite(min, this)
      case (min, Some(max)) =>
        Repeat.Between(min, max, this)
    }
  
  def >>(next: Regex): Regex =
    this match {
      case g: Group =>
        Sequence(NonEmptyList(next, g))
      case Sequence(list) =>
        Sequence(NonEmptyList.nel(next, list.list))
      case cc: CC =>
        Sequence(NonEmptyList(next, cc))
      case repeat: Repeat =>
        Sequence(NonEmptyList(next, repeat))
    }
  
  def @|(or: Regex): Regex = {
    val orSeq: Sequence =
      or match {
        case g: Group =>
          Sequence(NonEmptyList(g))
        case s: Sequence =>
          s
        case cc: CC =>
          Sequence(NonEmptyList(cc))
        case repeat: Repeat =>
          Sequence(NonEmptyList(repeat))
      }
    
    this match {
      case Group(options) =>
        Group(
          NonEmptyList.nel[Sequence](
            orSeq,
            options.list
          )
        )
      case s: Sequence =>
        Group(
          NonEmptyList(
            orSeq,
            s
          )
        )
      case cc: CC =>
        Group(
          NonEmptyList(
            orSeq,
            Sequence(NonEmptyList(cc))
          )
        )
      case repeat: Repeat =>
        Group(
          NonEmptyList(
            orSeq,
            Sequence(NonEmptyList(repeat))
          )
        )
    }
  }
  
  
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
      case cc: CC =>
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
    
    override def toString: String =
      s"Group(${options.list.toList.reverse.mkString(", ")})"
    
    def foreach(f: Sequence => Unit): Unit =
      options.reverse.foreach(f)
    
  }
  
  /**
   * NOTE (KR) : Additions to a sequence are in reverse order
   */
  case class Sequence(list: NonEmptyList[Regex]) extends Regex {
    
    override def toString: String =
      s"Sequence(${list.list.toList.reverse.mkString(", ")})"
  
    def foreach(f: Regex => Unit): Unit =
      list.reverse.foreach(f)
    
  }
  
  sealed trait CharClass extends Regex {
    
    import CC._
    
    def unary_! : CC =
      this match {
        case Only(chars) =>
          Except(chars)
        case Except(chars) =>
          Only(chars)
      }
    
    def |(other: CC): CC =
      this match {
        case Only(mChars) =>
          other match {
            case Only(oChars) =>
              Only(mChars | oChars)
            case Except(oChars) =>
              Except(oChars &~ mChars)
          }
        case Except(mChars) =>
          other match {
            case Only(oChars) =>
              Except(mChars &~ oChars)
            case Except(oChars) =>
              Except(mChars & oChars)
          }
      }
    
    def &(other: CC): CC =
      this match {
        case Only(mChars) =>
          other match {
            case Only(oChars) =>
              Only(mChars & oChars)
            case Except(oChars) =>
              Only(mChars &~ oChars)
          }
        case Except(mChars) =>
          other match {
            case Only(oChars) =>
              Only(oChars &~ mChars)
            case Except(oChars) =>
              Except(oChars | mChars)
          }
      }
    
    def -(other: CC): CC =
      this match {
        case Only(mChars) =>
          other match {
            case Only(oChars) =>
              Only(mChars &~ oChars)
            case Except(oChars) =>
              Only(mChars & oChars)
          }
        case Except(mChars) =>
          other match {
            case Only(oChars) =>
              Except(mChars | oChars)
            case Except(oChars) =>
              Only(oChars &~ mChars)
          }
      }
    
  }
  
  object CharClass {
    
    // =====| CC |=====
    
    case class Only(chars: Set[Char]) extends CC {
      
      override def toString: String =
        s"Only(${chars.toList.sorted.mkString(", ")})"
      
    }
    
    case class Except(chars: Set[Char]) extends CC {
      
      override def toString: String =
        s"Except(${chars.toList.sorted.mkString(", ")})"
      
    }
    
    // =====| Helpers |=====
    
    def only(chars: Char*): CC =
      Only(chars.toSet)
    
    def onlyR(start: Char, end: Char): CC =
      Only(start.to(end).toSet)
    
    def only(chars: String): CC =
      Only(chars.toCharArray.toSet)
    
    def except(chars: Char*): CC =
      Except(chars.toSet)
    
    def exceptR(start: Char, end: Char): CC =
      Except(start.to(end).toSet)
    
    def except(chars: String): CC =
      Except(chars.toCharArray.toSet)
    
    def exceptAscii(chars: Char*): CC =
      Only(0.toChar.to(127.toChar).toSet) - Only(chars.toSet)
    
    def exceptAscii(chars: String): CC =
      Only(0.toChar.to(127.toChar).toSet) - Only(chars.toCharArray.toSet)
    
    // =====| Common |=====
    
    object Common {
      
      val empty: CC = only()
      val all: CC = !empty
      
      val lowerLetters: CC = onlyR('a', 'z')
      val upperLetters: CC = onlyR('A', 'Z')
      val letters: CC = lowerLetters | upperLetters
      
      val __ : CC = only('_')
      
      val d: CC = onlyR('0', '9')
      
      val st: CC = only(' ', '\t')
      val n: CC = only('\n')
      val stn: CC = st | n
      
      val letterUnderscoreNumber: CC = letters | __ | d
      
    }
    
  }
  
}
