package slyce.generate.grammar

import scala.annotation.tailrec

import scalaz.NonEmptyList
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/

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

  override def apply(input: Data): Err \/ SimpleData = {
    val anonGenerator = SimpleData.Name.AnonList.generator

    @tailrec
    def loop(
        reductionLists: List[SimpleData.ReductionList],
        todo: List[Data.NonTerminal],
    ): List[SimpleData.ReductionList] = {
      def elementList(elements: List[Data.Element]): (List[SimpleData.Identifier], List[SimpleData.ReductionList]) =
        elements
          .map(mapElement(anonGenerator(), _))
          .foldRight((Nil: List[SimpleData.Identifier], Nil: List[SimpleData.ReductionList])) {
            // TODO (KR) : Make sure this doesnt come out backwards
            case ((id, extras1), (idList, extras2)) =>
              (id :: idList, extras1 ::: extras2)
          }

      def reductionList(
          name: SimpleData.Name,
          reduction: NonEmptyList[List[Data.Element]],
      ): (SimpleData.ReductionList, List[SimpleData.ReductionList]) = {
        val r0: NonEmptyList[(List[SimpleData.Identifier], List[SimpleData.ReductionList])] =
          reduction.map(elementList)

        val r1: NonEmptyList[SimpleData.ReductionList.Reduction] =
          r0.map(r => SimpleData.ReductionList.Reduction(r._1))

        val r2: List[SimpleData.ReductionList] =
          r0.list.toList.flatMap(_._2)

        (SimpleData.ReductionList(name, r1), r2)
      }

      def mapElement(
          name: => SimpleData.Name,
          element: Data.Element,
      ): (SimpleData.Identifier, List[SimpleData.ReductionList]) =
        element match {
          case id: Data.Identifier =>
            (SimpleData.Identifier(id), Nil)
          case lnt: NT.ListNT =>
            lnt match {
              case NT.ListNT.*(before, after) =>
                after match {
                  case None =>
                    val (elems1, extras1) = elementList(before.toList)

                    val n1 = name
                    val id1 = SimpleData.Identifier.NonTerminal(n1)

                    val rl1 = SimpleData.ReductionList(n1)(
                      SimpleData.ReductionList.Reduction(elems1 ::: id1 :: Nil),
                      SimpleData.ReductionList.Reduction(),
                    )

                    (id1, rl1 :: extras1)
                  case Some(after) =>
                    val (elems1, extras1) = elementList(before.toList)
                    val (elems2, extras2) = elementList(after.toList)

                    val n1 = name
                    val id1 = SimpleData.Identifier.NonTerminal(n1)
                    val n2 = n1.next
                    val id2 = SimpleData.Identifier.NonTerminal(n2)

                    def rlFrom(n: SimpleData.Name, elems: List[SimpleData.Identifier]): SimpleData.ReductionList =
                      SimpleData.ReductionList(n)(
                        SimpleData.ReductionList.Reduction(elems ::: id2 :: Nil),
                        SimpleData.ReductionList.Reduction(),
                      )

                    val rl1 = rlFrom(n1, elems1)
                    val rl2 = rlFrom(n2, elems2)

                    (id1, rl1 :: rl2 :: extras1 ::: extras2)
                }
              case NT.ListNT.+(before, after) =>
                val (elems1, extras1) = elementList(before.toList)
                val (elems2, extras2) =
                  after.fold(
                    (elems1, Nil: List[SimpleData.ReductionList]),
                  )(a => elementList(a.toList))

                val n1 = name
                val id1 = SimpleData.Identifier.NonTerminal(n1)
                val n2 = n1.next
                val id2 = SimpleData.Identifier.NonTerminal(n2)

                val rl1 = SimpleData.ReductionList(n1)(
                  SimpleData.ReductionList.Reduction(elems1 ::: id2 :: Nil),
                )
                val rl2 = SimpleData.ReductionList(n2)(
                  SimpleData.ReductionList.Reduction(elems2 ::: id2 :: Nil),
                  SimpleData.ReductionList.Reduction(),
                )

                (id1, rl1 :: rl2 :: extras1 ::: extras2)
            }
        }

      def standardNT(
          name: SimpleData.Name,
          nt: Data.NT.StandardNT,
      ): (SimpleData.Identifier, List[SimpleData.ReductionList]) =
        (
          SimpleData.Identifier.NonTerminal(name),
          nt match {
            case NT.StandardNT.`:`(elements) =>
              reductionList(name, elements.map(_.map(_._2)))._2
            case NT.StandardNT.^(elements) =>
              reductionList(name, elements.map(_.toList))._2
          },
        )

      todo match {
        case Nil =>
          reductionLists.reverse
        case Data.NonTerminal(name, nt) :: rest =>
          nt match {
            case nt: NT.StandardNT =>
              loop(
                standardNT(SimpleData.Name.Named(name), nt)._2 ::: reductionLists,
                rest,
              )
            case nt: NT.ListNT =>
              loop(
                mapElement(SimpleData.Name.Named(name), nt)._2 ::: reductionLists,
                rest,
              )
            case NT.AssocNT(assocElements, base) =>
              @tailrec
              def loop2(
                  name: SimpleData.Name,
                  assoc: List[Data.NT.AssocNT.AssocElement],
                  extras: List[SimpleData.ReductionList],
              ): List[SimpleData.ReductionList] =
                assoc match {
                  case Nil =>
                    standardNT(name, base)._2 ::: extras
                  case head :: tail =>
                    val nextName: SimpleData.Name = name.next
                    val myId = SimpleData.Identifier.NonTerminal(name)
                    val nextId = SimpleData.Identifier.NonTerminal(nextName)

                    // TODO (KR) : Make sure associativity is correct
                    val moreExtras = head match {
                      case NT.AssocNT.AssocElement.<(element) =>
                        val (assocId, extras) = mapElement(anonGenerator(), element)
                        SimpleData.ReductionList(name)(
                          SimpleData.ReductionList.Reduction(myId, assocId, nextId),
                          SimpleData.ReductionList.Reduction(nextId),
                        ) :: extras
                      case NT.AssocNT.AssocElement.>(element) =>
                        val (assocId, extras) = mapElement(anonGenerator(), element)
                        SimpleData.ReductionList(name)(
                          SimpleData.ReductionList.Reduction(nextId, assocId, myId),
                          SimpleData.ReductionList.Reduction(nextId),
                        ) :: extras
                    }

                    loop2(
                      nextName,
                      tail,
                      moreExtras ::: extras,
                    )
                }

              loop(
                loop2(SimpleData.Name.Named(name), assocElements.list.toList, Nil) ::: reductionLists,
                rest,
              )
          }
      }
    }

    val rls: List[SimpleData.ReductionList] =
      loop(
        Nil,
        input.nts,
      )

    rls.foreach { rl =>
      println(rl.name.str)
      rl.reductions.foreach { r =>
        println("    > " + r.elements.map(_.str).mkString(" "))
      }
      println
    }

    Nil.left
  }

}
