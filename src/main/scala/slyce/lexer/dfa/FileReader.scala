package slyce.lexer.dfa

import scalaz.std.option.optionSyntax._

import slyce.tree.GeneralToken.Stats.{GlobalSpan, RelativePos}

class FileReader(val lines: Iterator[Char]) {
  
  private var current: RelativePos = RelativePos(0, 0)
  private var spanStart: Int = 0
  private var start: RelativePos = current
  
  private var queue: Option[Char] = None
  
  // TODO (KR) : I am not sure if the '\n' handling happens automatically
  def getC: Option[Char] =
    queue match {
      case Some(c) =>
        queue = None
        c.some
      case None =>
        lines.nextOption match {
          case None =>
            None
          case Some('\r') =>
            lines.nextOption match {
              case None =>
                '\r'.some
              case Some('\n') =>
                '\n'.some
              case Some(c2) =>
                queue = c2.some
                '\r'.some
            }
          case Some(c) =>
            c.some
        }
    }
  
}
