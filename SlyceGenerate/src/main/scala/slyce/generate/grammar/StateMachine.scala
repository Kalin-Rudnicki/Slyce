package slyce.generate.grammar

import scala.annotation.tailrec

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToOptionIdOps

final case class StateMachine(
    startNt: String,
    rlMap: Map[StateMachine.ReductionList, StateMachine.State],
)

object StateMachine {

  // TODO (KR) : There is a decent amount of obviously correct things going on that are not necessarily pure
  //           : Might make sense to clean this up when the time comes

  final case class ReductionList(
      reductions: Set[ReductionList.Reduction],
  ) {

    val accepts: Map[SimpleData.Identifier, ReductionList] =
      reductions
        .flatMap(_.advance)
        .groupMap(_._1)(_._2)
        .map {
          case (k, v) =>
            k -> ReductionList(v)
        }

    accepts
      .exists {
        case (SimpleData.Identifier.Terminal(name), _) if name.contains(SimpleData.Identifier.EofName) =>
          true
        case _ =>
          false
      }
      .option(())
      .foreach { _ =>
        println
        accepts.foreach(println)
      }

    val returns: Set[(SimpleData.Name, Int, List[SimpleData.Identifier])] =
      reductions.flatMap {
        case ReductionList.Reduction(name, idx, elements, Nil, true) =>
          (name, idx, elements).some
        case _ =>
          None
      }

    def advance(
        nameMap: Map[SimpleData.Name, ReductionList],
        canPassThrough: Map[SimpleData.Name, Boolean],
    ): Map[SimpleData.Identifier, ReductionList] =
      accepts
        .map {
          case (k, v) =>
            k -> v.expand(nameMap, canPassThrough)
        }

    def expand(
        nameMap: Map[SimpleData.Name, ReductionList],
        canPassThrough: Map[SimpleData.Name, Boolean],
    ): ReductionList =
      ReductionList.build(reductions, nameMap, canPassThrough)

  }

  object ReductionList {

    final case class Reduction(
        name: SimpleData.Name,
        idx: Int,
        seen: List[SimpleData.Identifier],
        unseen: List[SimpleData.Identifier],
        canReturn: Boolean,
    ) {

      def advance: Option[(SimpleData.Identifier, Reduction)] =
        unseen match {
          case Nil =>
            None
          case head :: tail =>
            (
              head,
              Reduction(
                name,
                idx,
                seen :+ head,
                tail,
                true, // TODO (KR) : This could very likely be `canReturn`
              ),
            ).some
        }

      def str: String =
        s"${name.str}[$idx] : ${seen.map(_.str).mkString(" ")} . ${unseen.map(_.str).mkString(" ")}"

    }

    def build(
        reductions: Set[Reduction],
        nameMap: Map[SimpleData.Name, ReductionList],
        canPassThrough: Map[SimpleData.Name, Boolean],
    ): ReductionList = {
      @tailrec
      def loop(
          todo: Set[Reduction],
          alreadySeen: Set[Reduction],
      ): ReductionList = {
        val newRls = todo &~ alreadySeen
        if (newRls.isEmpty)
          ReductionList(alreadySeen)
        else {
          // TODO (KR) : Need a way to say that you can not actually return on something you were passed
          loop(
            newRls.flatMap { r =>
              r.advance match {
                case Some((SimpleData.Identifier.NonTerminal(name), passed)) =>
                  canPassThrough(name).option(passed.copy(canReturn = false)).toList ::: nameMap(name).reductions.toList
                case _ =>
                  Nil
              }
            },
            todo | alreadySeen,
          )
        }
      }

      loop(
        reductions,
        Set.empty,
      )
    }

  }

  final case class State(
      id: Int,
      reductionList: ReductionList,
  ) {

    val children: Set[ReductionList] =
      reductionList.accepts.values.toSet

  }

}
