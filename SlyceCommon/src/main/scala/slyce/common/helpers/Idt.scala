package slyce.common.helpers

import scalaz.Scalaz.ToOptionIdOps

sealed trait Idt {

  def build(indent: String): String = {
    val builder: StringBuilder = new StringBuilder()

    // TODO (KR) : Be more efficient?
    def loop(n: Int, idt: Idt): Unit =
      idt match {
        case Idt.Break =>
          builder.append('\n')
        case Idt.Str(str) =>
          0.until(n).foreach(_ => builder.append(indent))
          builder.append(str)
          builder.append('\n')
        case Idt.Indented(idts) =>
          idts.foreach(loop(n + 1, _))
        case Idt.Group(idts) =>
          idts.foreach(loop(n, _))
      }

    loop(0, this)
    builder.toString
  }

}

object Idt {

  case object Break extends Idt

  final case class Str(str: String) extends Idt

  final class Indented(val idts: List[Idt]) extends Idt
  object Indented {

    def apply(idts: Idt*): Indented =
      new Indented(idts.toList)

    def unapply(arg: Indented): Option[List[Idt]] =
      arg.idts.some

  }

  final class Group(val idts: List[Idt]) extends Idt
  object Group {

    def apply(idts: Idt*): Group =
      new Group(idts.toList)

    def unapply(arg: Group): Option[List[Idt]] =
      arg.idts.some

  }

  implicit def stringToStr(str: String): Str =
    Str(str)

  implicit def idtListToGroup(list: List[Idt]): Group =
    new Group(list)

}
