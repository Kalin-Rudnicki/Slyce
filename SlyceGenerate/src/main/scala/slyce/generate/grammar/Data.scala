package slyce.generate.grammar

import scalaz.IList
import scalaz.NonEmptyList

import klib.CharStringOps._
import scala.annotation.tailrec

import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
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

  final case class Optional(id: Identifier) extends Element

  sealed trait Identifier extends Element
  object Identifier {
    final case class Terminal private[Identifier] (name: String) extends Identifier
    final case class NonTerminal private[Identifier] (name: String) extends Identifier
    final case class Raw private[Identifier] (text: String) extends Identifier {
      override def toString: String =
        s"Raw(${text.unesc})"
    }

    def raw(text: String): Identifier =
      Raw(text)

    def apply(str: String): Identifier = {
      @tailrec
      def identify(chars: List[Char]): Identifier =
        chars match {
          case Nil =>
            raw(str)
          case c :: rest =>
            if (c == '_')
              identify(rest)
            else if (c.isUpper)
              NonTerminal(str)
            else if (c.isLower)
              Terminal(str)
            else
              raw(str)
        }

      identify(str.toList)
    }

  }

  sealed trait NT
  object NT {

    final case class IgnoredList(
        before: List[Element],
        unignored: Element,
        after: List[Element],
    ) {

      def toList: List[Element] =
        before ::: unignored :: after

      def toNel: NonEmptyList[Element] =
        IList(before: _*) <::: NonEmptyList(unignored, after: _*)

    }

    object IgnoredList {

      def apply(before: Element*)(unignored: Element)(after: Element*): IgnoredList =
        IgnoredList(before.toList, unignored, after.toList)

      // Boolean signifies whether element is `un-ignored`
      def fromElementList(elements: List[(Boolean, Element)]): List[String] \/ IgnoredList =
        elements match {
          case Nil =>
            List("No elements to build IgnoredList").left
          case (_, elem) :: Nil =>
            IgnoredList()(elem)().right
          case _ =>
            @tailrec
            def loop(
                queue: List[(Boolean, Element)],
                stack: List[Element],
            ): List[String] \/ IgnoredList =
              queue match {
                case Nil =>
                  List("Could not find element to un-ignore").left
                case head :: tail =>
                  head match {
                    case (false, elem) =>
                      loop(
                        tail,
                        elem :: stack,
                      )
                    case (true, elem) =>
                      if (tail.forall(!_._1))
                        IgnoredList(
                          before = stack.reverse,
                          unignored = elem,
                          after = tail.map(_._2),
                        ).right
                      else
                        List("Tried to un-ignore more than 1 element").left
                  }
              }

            loop(
              elements,
              Nil,
            )
        }

    }

    sealed trait StandardNT extends NT
    object StandardNT {

      final case class `:`(elements: NonEmptyList[List[Element]]) extends StandardNT
      object `:` {

        def apply(elements0: List[Element], elementsN: List[Element]*): `:` =
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
      object * {

        def simple(elem: Element): ListNT.* =
          ListNT.*(
            before = IgnoredList()(elem)(),
            after = None,
          )

        def beforeAfter(elem: Element, beforeAfter: Element*): ListNT.* =
          ListNT.*(
            before = IgnoredList()(elem)(),
            after = IgnoredList(beforeAfter: _*)(elem)().some,
          )

      }

      final case class +(
          before: IgnoredList,
          after: Option[IgnoredList],
      ) extends ListNT
      object + {

        def simple(elem: Element): ListNT.+ =
          ListNT.+(
            before = IgnoredList()(elem)(),
            after = None,
          )

        def beforeAfter(elem: Element, beforeAfter: Element*): ListNT.+ =
          ListNT.+(
            before = IgnoredList()(elem)(),
            after = IgnoredList(beforeAfter: _*)(elem)().some,
          )

      }

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
