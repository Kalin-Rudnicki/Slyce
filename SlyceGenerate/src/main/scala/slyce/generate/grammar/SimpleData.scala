package slyce.generate.grammar

import scalaz.NonEmptyList
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToOptionIdOps
import slyce.common.helpers._

case class SimpleData(
    startNt: String,
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

    // TODO (KR) : Possibly move this somewhere else?
    //           : Might be causing undesirable dependencies

    val EofName: String = "EOF"
    val Eof: Terminal = Terminal(s"$EofName.type")

    val RawName: String = "__"

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

  sealed trait NonOptionalName
  sealed trait Name {

    def next: Name =
      this match {
        case Name.AnonList(num, idx) =>
          Name.AnonList(num = num, idx = idx + 1)
        case Name.Named(name, idx) =>
          Name.Named(name, idx + 1)
        case _ =>
          // TODO (KR) : This should never happen?
          this
      }

    // This serves the purpose of trying to resolve duplicates
    def standardize(parent: Name): Name =
      (parent, this) match {
        case (Name.AnonList(pNum, _), Name.AnonList(tNum, idx)) if pNum == tNum =>
          Name.AnonList(0, idx)
        case _ =>
          this
      }

    def str: String = {
      def idxToS(i: Int): String =
        (i == 0).fold("", s"_${i + 1}")

      this match {
        case Name.AnonList(num, idx) =>
          s"AnonList$num${idxToS(idx)}"
        case Name.Optional(id) =>
          id match {
            case Identifier.Raw(text) =>
              s"`Optional${text.map(_.unesc).mkString}`"
            case Identifier.Terminal(name) =>
              s"Optional$name"
            case Identifier.NonTerminal(name) =>
              s"Optional${name.str}"
          }
        case Name.Named(name, idx) =>
          s"$name${idxToS(idx)}"
      }
    }

  }

  object Name {

    val Start: Name = Named("__Start")

    final case class AnonList private (num: Int, idx: Int) extends Name with NonOptionalName
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

    final case class Optional(id: Identifier) extends Name

    final case class Named private (name: String, idx: Int) extends Name with NonOptionalName
    object Named {

      def apply(name: String): Named =
        Named(name, 0)

    }

  }

  final case class ReductionList(
      name: Name,
      reductions: NonEmptyList[ReductionList.Reduction],
      simplifiers: ReductionList.Simplifiers,
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

    final case class Simplifiers(
        optional: Option[Identifier],
        // TODO (KR) : List
        // TODO (KR) : Assoc
    ) {

      // TODO (KR) : Make sure to update this as new simplifiers are added
      def nonEmpty: Boolean = {
        println(this)
        optional.nonEmpty
      }

    }
    object Simplifiers {
      def empty: Simplifiers =
        Simplifiers(
          optional = None,
        )
    }

    final case class Reduction(
        idx: Int,
        elements: List[Identifier],
    )
    object Reduction {

      def apply(idx: Int, elems: Identifier*): Reduction =
        Reduction(idx, elems.toList)

    }

    def apply(
        name: Name,
        optional: Option[Identifier] = None,
    )(r0: Reduction, rN: Reduction*): ReductionList =
      ReductionList(
        name = name,
        reductions = NonEmptyList(r0, rN: _*),
        simplifiers = Simplifiers(
          optional = optional,
        ),
      )

  }

}
