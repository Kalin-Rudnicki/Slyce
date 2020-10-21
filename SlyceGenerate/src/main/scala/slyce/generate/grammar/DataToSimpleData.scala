package slyce.generate.grammar

import scalaz.NonEmptyList
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.\/

import slyce.common.helpers._
import slyce.generate.architecture.{grammar => arch}
import slyce.generate.grammar.Data.NT

object DataToSimpleData extends arch.DataToSimpleData[Data, Err, SimpleData] {

  /*
   * ~ Data.NonTerminal => SimpleData.NonTerminal
   *
   * Challenges:
   * - Needing to create an extra NT above the start
   * - How to handle EOF token. (case object `$` extends Token[Nothing] (?))
   * - Handling AnonymousList's
   * - Handling Assoc's
   *
   * What is going to be challenging about this?
   * - Realistically, all these are are a condensed List/Set (?) of hidden basic reductions
   * - Therefore, the challenge is:
   *   - How to convert from the condensed to the expanded
   *   - How to redirect something that was pointing at the condensed to the expanded instead
   *
   * Data.Element =
   *    Data.Identifier
   *    | Data.NT.ListNT
   *
   * Data.NT =
   *    Data.NT.StandardNT
   *    | Data.NT.ListNT
   *    | Data.AssocNT
   *
   */

  sealed trait Element
  object Element {

    final case class Identifier(id: Data.Identifier) extends Element

    final case class ReductionList(reductions: NonEmptyList[ReductionList.Reduction]) extends Element
    object ReductionList {
      final case class Reduction(elements: List[Pointer[Element]])
    }

  }

  override def apply(input: Data): Err \/ SimpleData = {
    // TODO (KR) : Check for duplicates
    val ntMap: Map[String, Pointer[Element]] = input.nts.map {
      case Data.NonTerminal(name, nt) =>
        name -> (
          nt match {
            case nt: NT.StandardNT =>
              standardNT(nt)
            case nt: NT.ListNT =>
              listNT(nt)
            case nt: NT.AssocNT =>
              assocNT(nt)
          }
        )
    }.toMap

    def standardNT(nt: NT.StandardNT): Pointer[Element] =
      Pointer(
        nt match {
          case NT.StandardNT.`:`(elements) =>
            Element.ReductionList(
              elements.map { e1 =>
                Element.ReductionList.Reduction(
                  e1.map(e2 => element(e2._2)),
                )
              },
            )
          case NT.StandardNT.^(elements) =>
            Element.ReductionList(
              elements.map { e1 =>
                Element.ReductionList.Reduction(
                  e1.toList.map(element),
                )
              },
            )
        },
      )

    def listNT(nt: NT.ListNT): Pointer[Element] = {
      def repeatThis(`this`: Data.NT.IgnoredList): Pointer[Element] =
        Pointer.withSelf { self =>
          Pointer(
            Element.ReductionList(
              NonEmptyList(
                Element.ReductionList.Reduction(
                  `this`.toList.map(element) ::: self :: Nil,
                ),
                Element.ReductionList.Reduction(Nil),
              ),
            ),
          )
        }

      def thisThenThat(
          `this`: Data.NT.IgnoredList,
          that: Pointer[Element],
          canBeEmpty: Boolean,
      ): Pointer[Element] =
        Pointer(
          Element.ReductionList(
            NonEmptyList(
              Element.ReductionList.Reduction(
                `this`.toList.map(element) ::: that :: Nil,
              ),
              canBeEmpty
                .option(
                  Element.ReductionList.Reduction(Nil),
                )
                .toList: _*,
            ),
          ),
        )

      nt match {
        case NT.ListNT.*(before, after) =>
          after match {
            case None =>
              repeatThis(before)
            case Some(after) =>
              thisThenThat(
                before,
                repeatThis(after),
                true,
              )
          }
        case NT.ListNT.+(before, after) =>
          after match {
            case None =>
              thisThenThat(
                before,
                repeatThis(before),
                false,
              )
            case Some(after) =>
              thisThenThat(
                before,
                repeatThis(after),
                false,
              )
          }
      }
    }

    def assocNT(nt: NT.AssocNT): Pointer[Element] =
      nt match {
        case NT.AssocNT(assocElements, base) =>
          assocElements.list.toList.reverse.foldLeft(standardNT(base)) {
            case (next, assoc) =>
              assoc match {
                case NT.AssocNT.AssocElement.<(assocElement) =>
                  Pointer.withSelf { self =>
                    Pointer(
                      Element.ReductionList(
                        NonEmptyList(
                          Element.ReductionList.Reduction(
                            self :: element(assocElement) :: next :: Nil,
                          ),
                          Element.ReductionList.Reduction(
                            next :: Nil,
                          ),
                        ),
                      ),
                    )
                  }
                case NT.AssocNT.AssocElement.>(assocElement) =>
                  Pointer.withSelf { self =>
                    Pointer(
                      Element.ReductionList(
                        NonEmptyList(
                          Element.ReductionList.Reduction(
                            next :: element(assocElement) :: self :: Nil,
                          ),
                          Element.ReductionList.Reduction(
                            next :: Nil,
                          ),
                        ),
                      ),
                    )
                  }
              }
          }
      }

    def element(e: Data.Element): Pointer[Element] =
      e match {
        case i: Data.Identifier =>
          Pointer(Element.Identifier(i))
        case nt: NT.ListNT =>
          listNT(nt)
      }

    ???
  }

}
