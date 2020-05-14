package slyce.lexer.regex

sealed trait CharClass {
  
  import CharClass._
  
  def unary_! : CharClass =
    this match {
      case Only(chars) =>
        Except(chars)
      case Except(chars) =>
        Only(chars)
    }
  
  def |(other: CharClass): CharClass =
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
  
  def &(other: CharClass): CharClass =
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
  
  def -(other: CharClass): CharClass =
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
  
  // =====| CharClass |=====
  
  case class Only(chars: Set[Char]) extends CharClass
  
  case class Except(chars: Set[Char]) extends CharClass
  
  // =====| Helpers |=====
  
  def only(chars: Char*): CharClass =
    Only(chars.toSet)
  
  def onlyR(start: Char, end: Char): CharClass =
    Only(start.to(end).toSet)
  
  def only(chars: String): CharClass =
    Only(chars.toCharArray.toSet)
  
  def except(chars: Char*): CharClass =
    Except(chars.toSet)
  
  def exceptR(start: Char, end: Char): CharClass =
    Except(start.to(end).toSet)
  
  def except(chars: String): CharClass =
    Except(chars.toCharArray.toSet)
  
  def exceptAscii(chars: Char*): CharClass =
    Only(0.toChar.to(127.toChar).toSet) - Only(chars.toSet)
  
  def exceptAscii(chars: String): CharClass =
    Only(0.toChar.to(127.toChar).toSet) - Only(chars.toCharArray.toSet)
  
  // =====| Common |=====
  
  object Common {
    
    import slyce.lexer.regex.{CharClass => CC}
    
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
