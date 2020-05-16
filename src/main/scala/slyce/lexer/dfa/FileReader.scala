package slyce.lexer.dfa

import scalaz.std.option.optionSyntax._

import slyce.tree.GeneralToken.Stats.RelativePos

final class FileReader(val source: Iterator[Char]) {

  import FileReader._

  type Group =
    (Char, CharSpan, LocInFile)

  private var relative: RelativePos = RelativePos(0, 0)
  private var global: Int = 0

  private var queue: List[Either[Char, Group]] = Nil

  def getC: Option[Group] = {
    def getFromQueue: Option[Either[Char, Group]] =
      queue match {
        case Nil =>
          None
        case head :: tail =>
          queue = tail
          head.some
      }

    def getFromSource: Option[Char] =
      source.nextOption

    def nextRaw: Option[Either[Char, Group]] =
      getFromQueue.cata(
        fQ => fQ.some,
        getFromSource.map(Left(_))
      )

    def nextOnlyChar: Option[Char] =
      queue match {
        case Left(c) :: tail =>
          queue = tail
          c.some
        case Right(_) :: _ =>
          None
        case Nil =>
          getFromSource
      }

    def nextSmart: Option[Group] =
      nextRaw.map {
        case Left('\r') =>
          nextOnlyChar match {
            case None =>
              newLine
            case Some('\n') =>
              newLine
            case Some(c2) =>
              queue = Left(c2) :: queue
              newLine
          }
        case Left(c) =>
          nextInLine(c)
        case Right(cgr) =>
          cgr
      }

    nextSmart
  }

  def nextInLine(c: Char): Group = {
    val cs = CharSpan(global, global)
    val res = LocInFile(relative.lineNo, CharSpan(relative.posInLine, relative.posInLine))
    global += 1
    relative = relative.nextInLine
    (c, cs, res)
  }

  def newLine: Group = {
    val cs = CharSpan(global, global)
    val res = LocInFile(relative.lineNo, CharSpan(relative.posInLine, relative.posInLine))
    global += 1
    relative = relative.newLine
    ('\n', cs, res)
  }

  def newLine2: Group = {
    val cs = CharSpan(global, global + 1)
    val res = LocInFile(relative.lineNo, CharSpan(relative.posInLine, relative.posInLine + 1))
    global += 2
    relative = relative.newLine
    ('\n', cs, res)
  }

  /**
    * @param list "Group"'s you want to give back, in the order you want to receive them
    */
  def pushBack(list: List[Group]): Unit =
    queue = list.map(Right(_)) ::: queue

}

object FileReader {

  final case class CharSpan(start: Int, end: Int)

  final case class LocInFile(lineNo: Int, charSpanInLine: CharSpan)

}
