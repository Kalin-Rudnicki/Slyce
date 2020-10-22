package slyce.generate.grammar

import scalaz.NonEmptyList
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/

import slyce.common.helpers._
import slyce.generate.architecture.{grammar => arch}
import slyce.generate.grammar.Data.Identifier
import slyce.generate.grammar.Data.NT
import slyce.generate.grammar.DataToSimpleData.Element.ReductionList

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
      object Reduction {

        def apply(elements: Pointer[Element]*): Reduction =
          new Reduction(elements.toList)

      }

      def apply(r0: Reduction, rN: Reduction*): ReductionList =
        new ReductionList(NonEmptyList(r0, rN: _*))

    }

  }

  override def apply(input: Data): Err \/ SimpleData = {
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
              Element.ReductionList.Reduction(
                `this`.toList.map(element) ::: self :: Nil,
              ),
              Element.ReductionList.Reduction(),
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
            Element.ReductionList.Reduction(
              `this`.toList.map(element) ::: that :: Nil,
            ),
            canBeEmpty
              .option(
                Element.ReductionList.Reduction(),
              )
              .toList: _*,
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
                        Element.ReductionList.Reduction(
                          self :: element(assocElement) :: next :: Nil,
                        ),
                        Element.ReductionList.Reduction(next),
                      ),
                    )
                  }
                case NT.AssocNT.AssocElement.>(assocElement) =>
                  Pointer.withSelf { self =>
                    Pointer(
                      Element.ReductionList(
                        Element.ReductionList.Reduction(
                          next :: element(assocElement) :: self :: Nil,
                        ),
                        Element.ReductionList.Reduction(next),
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

    def rListToSet(rList: ReductionList): Set[ReductionList] =
      rList.reductions.list.toList.flatMap { r =>
        r.elements.flatMap { (ptr: Pointer[Element]) =>
          ptr match {
            case Pointer(v) =>
              v match {
                case rList2: ReductionList =>
                  rList2.some
                case _ =>
                  None
              }
          }
        }
      }.toSet

    val initialSet: Set[ReductionList] =
      ntMap.toList
        .map(_._2)
        .toSet
        .flatMap { (ptr: Pointer[Element]) =>
          ptr match {
            case Pointer(v) =>
              v match {
                case rList: ReductionList =>
                  rList.some
                case _ =>
                  None
              }
          }
        }

    val reductionMap: Map[ReductionList, Int] =
      findAll(initialSet)(rListToSet).toList.zipWithIndex.toMap

    ntMap.foreach {
      case (name, elem) =>
        elem match {
          case Pointer(v) =>
            v match {
              case rList: ReductionList =>
                println(s"$name => reductionMap(${reductionMap(rList)})")
              case Element.Identifier(id) =>
                println(s"$name => $id")
            }
        }
    }

    println
    println

    reductionMap.foreach {
      case (rList, i) =>
        println(s"=====| $i |=====")
        rList.reductions.zipWithIndex.foreach {
          case (r, i2) =>
            println(s"--- $i2 (${r.elements.length}) ---")
            r.elements.foreach {
              case Pointer(v) =>
                v match {
                  case Element.Identifier(id) =>
                    id match {
                      case Identifier.NonTerminal(name) =>
                        ntMap(name) match {
                          case Pointer(v) =>
                            v match {
                              case rList: ReductionList =>
                                println(s"reductionMap(${reductionMap(rList)})")
                              case _ =>
                                println(v)
                            }
                        }
                      case _ =>
                        println(id)
                    }
                  case rList2: ReductionList =>
                    println(s"reductionMap(${reductionMap(rList2)})")
                }
            }
            println
        }
        println
    }

    Nil.left
  }

}
