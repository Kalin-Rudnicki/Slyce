package slyce.generate.grammar

import scalaz.NonEmptyList
import scalaz.\/

case class Data(
    startNT: String,
    nts: List[Data.NonTerminal],
)

object Data {

  // TODO (KR) : This could possibly use some refactoring/renaming to be more understandable

  final case class NonTerminal(
      name: String,
      nt: NT,
  )

  sealed trait Element

  sealed trait Identifier extends Element
  object Identifier {
    final case class Terminal private[Identifier] (name: String) extends Identifier
    final case class NonTerminal private[Identifier] (name: String) extends Identifier
    final case class Raw private[Identifier] (text: String) extends Identifier

    def raw(text: String): Identifier =
      Raw(text)

    def apply(str: String): Identifier =
      str.toList match {
        case Nil =>
          raw(str)
        case c :: _ =>
          if (c.isUpper)
            NonTerminal(str)
          else if (c.isLower)
            Terminal(str)
          else
            raw(str)
      }

  }

  sealed trait NT
  object NT {

    type ElementList = List[(Boolean, Element)]

    final case class IgnoredList(
        before: List[Element],
        unignored: Element,
        after: List[Element],
    ) {

      def toList: List[Element] =
        before ::: unignored :: after

    }

    object IgnoredList {

      def apply(before: Element*)(unignored: Element)(after: Element*): IgnoredList =
        IgnoredList(before.toList, unignored, after.toList)

      // TODO (KR) :
      def fromElementList(list: ElementList): List[String] \/ IgnoredList =
        ???

    }

    sealed trait StandardNT extends NT
    object StandardNT {

      final case class `:`(elements: NonEmptyList[ElementList]) extends StandardNT
      object `:` {

        def apply(elements0: ElementList, elementsN: ElementList*): `:` =
          `:`(NonEmptyList(elements0, elementsN: _*))
      }

      final case class ^(elements: NonEmptyList[IgnoredList]) extends StandardNT
      object ^ {
        def apply(elements0: IgnoredList, elementsN: IgnoredList*): ^ =
          ^(NonEmptyList(elements0, elementsN: _*))
      }

    }

    sealed trait ListNT extends Element with NT
    object ListNT {

      final case class *(
          before: IgnoredList,
          after: Option[IgnoredList],
      ) extends ListNT

      final case class +(
          before: IgnoredList,
          after: Option[IgnoredList],
      ) extends ListNT

    }

    final case class AssocNT(
        assocElements: NonEmptyList[AssocNT.AssocElement],
        base: StandardNT,
    ) extends NT
    object AssocNT {

      sealed trait AssocElement
      object AssocElement {
        final case class <(element: Element) extends AssocElement
        final case class >(element: Element) extends AssocElement
      }

    }

  }

}
