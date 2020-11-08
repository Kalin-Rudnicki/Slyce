package slyce.generate.grammar

import scalaz.{NonEmptyList, \/}
import scalaz.Scalaz.{ToBooleanOpsFromBoolean, ToOptionIdOps}
import slyce.common.helpers._

case class SimpleData(
    augmentedStart: SimpleData.ReductionList,
    reductionLists: List[SimpleData.ReductionList],
)

// TODO (KR) : Is this really the right name for this?
object SimpleData {

  sealed trait Identifier {

    def str: String = {
      val name = this.getClass.getName.split("[\\.$]").toList.last

      val text =
        this match {
          case Identifier.Raw(text) =>
            text.unesc
          case Identifier.Terminal(name) =>
            name
          case Identifier.NonTerminal(name) =>
            name.str
        }

      s"$name($text)"
    }

  }

  object Identifier {

    val EofName: String = "EOF"
    val Eof: Terminal = Terminal(s"$EofName.type")

    final case class Raw(text: String) extends Identifier {

      override def toString: String =
        s"Raw(${text.unesc})"

    }

    final case class Terminal(name: String) extends Identifier

    final case class NonTerminal(name: Name) extends Identifier

    def apply(id: Data.Identifier): Identifier =
      id match {
        case Data.Identifier.Raw(text) =>
          Raw(text)
        case Data.Identifier.Terminal(name) =>
          Terminal(name)
        case Data.Identifier.NonTerminal(name) =>
          NonTerminal(Name.Named(name))
      }

  }

  sealed trait Name {

    def next: Name =
      this match {
        case Name.AnonList(num, idx) =>
          Name.AnonList(num = num, idx = idx + 1)
        case Name.Named(name, idx) =>
          Name.Named(name, idx + 1)
      }

    // This serves the purpose of trying to resolve duplicates
    def standardize(parent: Name): Name =
      (parent, this) match {
        case (Name.AnonList(pNum, _), Name.AnonList(tNum, idx)) if pNum == tNum =>
          Name.AnonList(0, idx)
        case _ =>
          this
      }

    def str: String =
      this match {
        case Name.AnonList(num, idx) =>
          s"AnonList$num${(idx == 0).fold("", s"_${idx + 1}")}"
        case Name.Named(name, idx) =>
          s"$name${(idx == 0).fold("", s"_${idx + 1}")}"
      }

  }

  object Name {

    val Start: Name = Named("__Start")

    final case class AnonList private (num: Int, idx: Int) extends Name
    object AnonList {

      final class Generator private[AnonList] {

        private var counter = 0

        def apply(): AnonList = {
          counter += 1
          AnonList(num = counter, idx = 0)
        }

      }

      def generator: Generator = new Generator

    }

    final case class Named private (name: String, idx: Int) extends Name
    object Named {

      def apply(name: String): Named =
        Named(name, 0)

    }

  }

  final case class ReductionList(
      name: Name,
      reductions: NonEmptyList[ReductionList.Reduction],
      // TODO (KR) : Ignored
      // TODO (KR) : List
      // TODO (KR) : Assoc
  ) {

    def standardized: Option[(Name.AnonList, ReductionList, List[List[Identifier]])] =
      name match {
        case name: Name.AnonList =>
          (
            name,
            this,
            reductions.list.toList.map {
              _.elements.map {
                case Identifier.NonTerminal(name) =>
                  Identifier.NonTerminal(name.standardize(this.name))
                case i =>
                  i
              }
            },
          ).some
        case _ =>
          None
      }

  }

  object ReductionList {

    final case class Reduction(
        idx: Int,
        elements: List[Identifier],
    )
    object Reduction {

      def apply(idx: Int, elems: Identifier*): Reduction =
        Reduction(idx, elems.toList)

    }

    def apply(name: Name)(r0: Reduction, rN: Reduction*): ReductionList =
      ReductionList(
        name,
        NonEmptyList(r0, rN: _*),
      )

  }

}
