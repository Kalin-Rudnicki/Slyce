package slyce.generate.grammar

import scala.annotation.tailrec

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

    val returns: Set[(SimpleData.Name, Int, List[SimpleData.Identifier])] =
      reductions.flatMap {
        case ReductionList.Reduction(name, idx, elements, Nil) =>
          (name, idx, elements).some
        case _ =>
          None
      }

    def advance(nameMap: Map[SimpleData.Name, ReductionList]): Map[SimpleData.Identifier, ReductionList] =
      accepts
        .map {
          case (k, v) =>
            k -> v.expand(nameMap)
        }

    def expand(nameMap: Map[SimpleData.Name, ReductionList]): ReductionList =
      ReductionList.build(reductions, nameMap)

  }

  object ReductionList {

    final case class Reduction(
        name: SimpleData.Name,
        idx: Int,
        seen: List[SimpleData.Identifier],
        unseen: List[SimpleData.Identifier],
    ) {

      def nextName: Option[SimpleData.Name] =
        unseen match {
          case SimpleData.Identifier.NonTerminal(name) :: _ =>
            name.some
          case _ =>
            None
        }

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
              ),
            ).some
        }

      def str: String =
        s"${name.str}[$idx] : ${seen.map(_.str).mkString(" ")} . ${unseen.map(_.str).mkString(" ")}"

    }

    def build(
        reductions: Set[Reduction],
        nameMap: Map[SimpleData.Name, ReductionList],
    ): ReductionList = {
      @tailrec
      def loop(
          reductions: Set[Reduction],
          seen: Set[SimpleData.Name],
          todo: Set[SimpleData.Name],
      ): ReductionList =
        todo.toList match {
          case Nil =>
            ReductionList(reductions)
          case head :: tail =>
            val newSeen = seen + head
            val fromName = nameMap(head)
            val newReductions = reductions | fromName.reductions
            val newNames = newReductions.flatMap(_.nextName)
            loop(
              newReductions,
              newSeen,
              (tail.toSet | newNames) &~ newSeen,
            )
        }

      loop(
        reductions,
        Set.empty,
        reductions.flatMap(_.nextName),
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
