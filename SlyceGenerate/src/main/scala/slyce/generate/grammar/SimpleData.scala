package slyce.generate.grammar

import scalaz.NonEmptyList
import scalaz.Scalaz.ToBooleanOpsFromBoolean

import slyce.common.helpers._

case class SimpleData(
    startNT: String,
)

// TODO (KR) : Is this really the right name for this?
object SimpleData {

  sealed trait Identifier {

    def str: String =
      this match {
        case Identifier.Raw(text) =>
          text.unesc
        case Identifier.Terminal(name) =>
          name
        case Identifier.NonTerminal(name) =>
          name.str
      }

  }

  object Identifier {

    final case class Raw(text: String) extends Identifier

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

    def str: String =
      this match {
        case Name.AnonList(num, idx) =>
          s"AnonList$num${(idx == 0).fold("", s"_$idx")}"
        case Name.Named(name, idx) =>
          s"$name${(idx == 0).fold("", s"_$idx")}"
      }

  }

  object Name {

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
  )
  object ReductionList {

    final case class Reduction(elements: List[Identifier])
    object Reduction {

      def apply(elems: Identifier*): Reduction =
        Reduction(elems.toList)

    }

    def apply(name: Name)(r0: Reduction, rN: Reduction*): ReductionList =
      ReductionList(
        name,
        NonEmptyList(r0, rN: _*),
      )

  }

}
