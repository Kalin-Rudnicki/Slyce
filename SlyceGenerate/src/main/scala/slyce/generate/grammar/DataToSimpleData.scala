package slyce.generate.grammar

import scala.annotation.tailrec

import scalaz.NonEmptyList
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption
import scalaz.\/

import slyce.generate.architecture.{grammar => arch}
import slyce.generate.grammar.Data.NT
import slyce.generate.grammar.SimpleData.{Identifier, Name}

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
            case ((id, extras1), (idList, extras2)) =>
              (id :: idList, extras1 ::: extras2)
          }

      def reductionList(
          name: SimpleData.Name,
          reduction: NonEmptyList[List[Data.Element]],
          exprSimp: Option[SimpleData.ReductionList.Simplifiers.ExprSimplifier],
      ): List[SimpleData.ReductionList] = {
        val mapped =
          reduction
            .map(elementList)

        val added =
          SimpleData.ReductionList(
            name = name,
            reductions = mapped.zipWithIndex
              .map {
                case ((r, _), idx) =>
                  SimpleData.ReductionList.Reduction(idx + 1, r)
              },
            simplifiers = SimpleData.ReductionList.Simplifiers.empty.copy(expr = exprSimp),
          )

        val extras =
          mapped.list.toList
            .flatMap(_._2)

        added :: extras
      }

      def mapElement(
          name: => SimpleData.Name,
          element: Data.Element,
      ): (SimpleData.Identifier, List[SimpleData.ReductionList]) =
        element match {
          case id: Data.Identifier =>
            (SimpleData.Identifier(id), Nil)
          case Data.Optional(id) =>
            val optName = SimpleData.Name.Optional(SimpleData.Identifier(id))
            val (elems, extras) = elementList(id :: Nil)
            val rl = SimpleData.ReductionList(optName, optional = SimpleData.Identifier(id).some)(
              SimpleData.ReductionList.Reduction(1, elems),
              SimpleData.ReductionList.Reduction(2),
            )

            (SimpleData.Identifier.NonTerminal(optName), rl :: extras)
          case lnt: NT.ListNT =>
            import SimpleData.ReductionList.Simplifiers.ListSimplifier

            def makePositions(il: Data.NT.IgnoredList): ListSimplifier.Positions =
              ListSimplifier.Positions(
                il.before.size,
                il.before.size + il.after.size,
              )

            lnt match {
              case NT.ListNT.*(before, after) =>
                after match {
                  case None =>
                    val (elems1, extras1) = elementList(before.toList)

                    val n1 = name
                    val id1 = SimpleData.Identifier.NonTerminal(n1)

                    val rl1 = SimpleData.ReductionList(
                      n1,
                      list = ListSimplifier(
                        `type` = elems1(before.before.size),
                        _1CanBeEmpty = true,
                        _1 = makePositions(before),
                        _2 = None,
                      ).some,
                    )(
                      SimpleData.ReductionList.Reduction(1, elems1 ::: id1 :: Nil),
                      SimpleData.ReductionList.Reduction(2),
                    )

                    (id1, rl1 :: extras1)
                  case Some(after) =>
                    val (elems1, extras1) = elementList(before.toList)
                    val (elems2, extras2) = elementList(after.toList)

                    val n1 = name
                    val id1 = SimpleData.Identifier.NonTerminal(n1)
                    val n2 = n1.next
                    val id2 = SimpleData.Identifier.NonTerminal(n2)

                    def rlFrom(
                        n: SimpleData.Name,
                        isFirst: Boolean,
                        elems: List[SimpleData.Identifier],
                    ): SimpleData.ReductionList =
                      SimpleData.ReductionList(
                        n,
                        list = isFirst.option(
                          ListSimplifier(
                            `type` = elems1(before.before.size),
                            _1CanBeEmpty = true,
                            _1 = makePositions(before),
                            _2 = makePositions(after).some,
                          ),
                        ),
                      )(
                        SimpleData.ReductionList.Reduction(1, elems ::: id2 :: Nil),
                        SimpleData.ReductionList.Reduction(2),
                      )

                    val rl1 = rlFrom(n1, true, elems1)
                    val rl2 = rlFrom(n2, false, elems2)

                    (id1, rl1 :: rl2 :: extras1 ::: extras2)
                }
              case NT.ListNT.+(before, after) =>
                val (elems1, extras1) = elementList(before.toList)
                val (elems2, extras2) =
                  after.cata(
                    a => elementList(a.toList),
                    (elems1, Nil),
                  )

                val n1 = name
                val id1 = SimpleData.Identifier.NonTerminal(n1)
                val n2 = n1.next
                val id2 = SimpleData.Identifier.NonTerminal(n2)

                val rl1 = SimpleData.ReductionList(
                  n1,
                  list = ListSimplifier(
                    `type` = elems1(before.before.size),
                    _1CanBeEmpty = false,
                    _1 = makePositions(before),
                    _2 = makePositions(after.getOrElse(before)).some,
                  ).some,
                )(
                  SimpleData.ReductionList.Reduction(1, elems1 ::: id2 :: Nil),
                )
                val rl2 = SimpleData.ReductionList(n2)(
                  SimpleData.ReductionList.Reduction(1, elems2 ::: id2 :: Nil),
                  SimpleData.ReductionList.Reduction(2),
                )

                (id1, rl1 :: rl2 :: extras1 ::: extras2)
            }
        }

      def standardNT(
          name: SimpleData.Name,
          nt: Data.NT.StandardNT,
          exprSimp: Option[SimpleData.ReductionList.Simplifiers.ExprSimplifier],
      ): List[SimpleData.ReductionList] =
        nt match {
          case NT.StandardNT.`:`(elements) =>
            reductionList(name, elements.map(_.map(_._2)), exprSimp)
          case NT.StandardNT.^(elements) =>
            reductionList(name, elements.map(_.toList), exprSimp)
        }

      todo match {
        case Nil =>
          reductionLists.reverse
        case Data.NonTerminal(name, nt) :: rest =>
          nt match {
            case nt: NT.StandardNT =>
              loop(
                standardNT(SimpleData.Name.Named(name), nt, None) ::: reductionLists,
                rest,
              )
            case nt: NT.ListNT =>
              loop(
                mapElement(SimpleData.Name.Named(name), nt)._2 ::: reductionLists,
                rest,
              )
            case NT.AssocNT(assocElements, base) =>
              val rootName = SimpleData.Name.Named(name)
              val assocElementsList = assocElements.list.toList.reverse

              val baseName: SimpleData.Name = {
                @tailrec
                def loop(
                    currentName: SimpleData.Name,
                    assoc: List[Data.NT.AssocNT.AssocElement],
                ): SimpleData.Name =
                  assoc match {
                    case Nil =>
                      currentName
                    case _ :: tail =>
                      loop(currentName.next, tail)
                  }

                loop(rootName, assocElementsList)
              }

              @tailrec
              def loop2(
                  name: SimpleData.Name,
                  assoc: List[Data.NT.AssocNT.AssocElement],
                  extras: List[SimpleData.ReductionList],
              ): List[SimpleData.ReductionList] =
                assoc match {
                  case Nil =>
                    // TODO (KR) : Might need special treatment for `^`?
                    standardNT(
                      name,
                      base,
                      SimpleData.ReductionList.Simplifiers
                        .ExprSimplifier(
                          rootName = rootName,
                          opId = None,
                          baseName = baseName,
                        )
                        .some,
                    ) ::: extras
                  case head :: tail =>
                    val nextName: SimpleData.Name = name.next
                    val myId = SimpleData.Identifier.NonTerminal(name)
                    val nextId = SimpleData.Identifier.NonTerminal(nextName)

                    def simp(opId: SimpleData.Identifier): SimpleData.ReductionList.Simplifiers.ExprSimplifier =
                      SimpleData.ReductionList.Simplifiers.ExprSimplifier(
                        rootName = rootName,
                        opId = opId.some,
                        baseName = baseName,
                      )

                    // TODO (KR) : Make sure associativity is correct
                    val moreExtras = head match {
                      case NT.AssocNT.AssocElement.<(element) =>
                        val (assocId, extras) = mapElement(anonGenerator(), element)
                        SimpleData.ReductionList(
                          name,
                          expr = simp(assocId).some,
                        )(
                          SimpleData.ReductionList.Reduction(1, myId, assocId, nextId),
                          SimpleData.ReductionList.Reduction(2, nextId),
                        ) :: extras
                      case NT.AssocNT.AssocElement.>(element) =>
                        val (assocId, extras) = mapElement(anonGenerator(), element)
                        SimpleData.ReductionList(
                          name,
                          expr = simp(assocId).some,
                        )(
                          SimpleData.ReductionList.Reduction(1, nextId, assocId, myId),
                          SimpleData.ReductionList.Reduction(2, nextId),
                        ) :: extras
                    }

                    loop2(
                      nextName,
                      tail,
                      moreExtras ::: extras,
                    )
                }

              loop(
                loop2(
                  rootName,
                  assocElementsList,
                  Nil,
                ) ::: reductionLists,
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

    val (
      namedRls: List[SimpleData.ReductionList],
      anon: List[(Name.AnonList, SimpleData.ReductionList, List[List[Identifier]])],
    ) =
      rls.foldLeft(
        (
          Nil: List[SimpleData.ReductionList],
          Nil: List[(SimpleData.Name.AnonList, SimpleData.ReductionList, List[List[SimpleData.Identifier]])],
        ),
      ) {
        case ((named, anon), rl) =>
          rl.standardized match {
            case None =>
              (rl :: named, anon)
            case Some(std) =>
              (named, std :: anon)
          }
      } match {
        case (_1, _2) =>
          (_1.distinct, _2)
      }

    val (
      anonRls: List[SimpleData.ReductionList],
      toReduce: List[Int],
    ) =
      anon
        .groupBy(_._3)
        .toList
        .map(_._2)
        .foldLeft(
          (
            Nil: List[SimpleData.ReductionList],
            Nil: List[Int],
          ),
        ) {
          case (prev @ (aRls, tIgn), grouped) =>
            grouped.sortBy(_._1.num) match {
              case Nil =>
                prev
              case head :: tail =>
                (head._2 :: aRls, tail.map(_._1.num) ::: tIgn)
            }
        }

    val reduceName: SimpleData.Name => SimpleData.Name = {
      case name: SimpleData.Name.AnonList =>
        name.copy(num = name.num - toReduce.count(_ <= name.num))
      case name =>
        name
    }

    val reduceId: SimpleData.Identifier => SimpleData.Identifier = {
      case Identifier.NonTerminal(name) =>
        Identifier.NonTerminal(reduceName(name))
      case id =>
        id
    }

    val newRls: List[SimpleData.ReductionList] =
      (namedRls ::: anonRls)
        .sortBy(_.name.str)
        .map {
          case SimpleData.ReductionList(name, reductions, simps) =>
            SimpleData.ReductionList(
              name = reduceName(name),
              reductions = reductions.map {
                case SimpleData.ReductionList.Reduction(idx, elements) =>
                  SimpleData.ReductionList.Reduction(
                    idx,
                    elements.map(reduceId),
                  )
              },
              simplifiers = simps,
            )
        }

    val allExprSimps = newRls.flatMap(_.simplifiers.expr)
    val extendsOps: Map[SimpleData.Identifier, Set[SimpleData.Name]] =
      allExprSimps
        .flatMap(e => e.opId.map(_ -> e.rootName))
        .groupMap(_._1)(_._2)
        .map {
          case (k, v) =>
            k -> v.toSet
        }

    {
      import slyce.common.helpers.Idt._

      print(
        Group(
          "ExprSimps:",
          Indented(
            allExprSimps.map { s =>
              Group(
                ">",
                Indented(
                  s"rooName:  ${s.rootName.str}",
                  s"opId:     ${s.opId}",
                  s"baseName: ${s.baseName.str}",
                ),
              )
            },
          ),
          "ExtendsOps:",
          Indented(
            extendsOps.toList.map {
              case (k, v) =>
                s"$k -> ${v.map(_.str)}"
            },
          ),
        ).build("    "),
      )
    }

    val augmentedStart: SimpleData.ReductionList =
      SimpleData.ReductionList(SimpleData.Name.Start)(
        SimpleData.ReductionList.Reduction(
          1,
          SimpleData.Identifier.NonTerminal(SimpleData.Name.Named(input.startNT)),
          SimpleData.Identifier.Eof,
        ),
      )

    SimpleData(
      input.startNT,
      augmentedStart,
      augmentedStart :: newRls,
      extendsOps,
    ).right
  }

}
