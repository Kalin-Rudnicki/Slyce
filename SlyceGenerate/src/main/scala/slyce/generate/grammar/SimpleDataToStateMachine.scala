package slyce.generate.grammar

import scala.annotation.tailrec

import scalaz.-\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.\/
import scalaz.\/-

import slyce.common.helpers._
import slyce.generate.architecture.{grammar => arch}

object SimpleDataToStateMachine extends arch.SimpleDataToStateMachine[SimpleData, StateMachine] {

  override def apply(input: SimpleData): List[String] \/ StateMachine = {
    def mapReductionList(
        reductionList: SimpleData.ReductionList,
    ): (SimpleData.Name, StateMachine.ReductionList) =
      reductionList match {
        case SimpleData.ReductionList(name, reductions) =>
          name ->
            StateMachine.ReductionList(
              reductions.list.toList.map {
                case SimpleData.ReductionList.Reduction(idx, elements) =>
                  StateMachine.ReductionList.Reduction(
                    name,
                    idx,
                    Nil,
                    elements,
                  )
              }.toSet,
            )
      }

    val nameMap: Map[SimpleData.Name, StateMachine.ReductionList] =
      input.reductionLists.map(mapReductionList).toMap

    val augmentedStart: StateMachine.ReductionList =
      mapReductionList(input.augmentedStart)._2.expand(nameMap)

    @tailrec
    def loop(
        map: Map[StateMachine.ReductionList, StateMachine.State],
        todo: Set[StateMachine.ReductionList],
    ): Map[StateMachine.ReductionList, StateMachine.State] =
      todo.toList match {
        case Nil =>
          map
        case head :: tail =>
          val rl = head.expand(nameMap)
          val state = StateMachine.State(map.size, rl)
          val newMap = map + (head -> state)
          val newTodo = (tail.toSet | state.children) &~ newMap.keySet
          loop(
            newMap,
            newTodo,
          )
      }

    val map = loop(Map(), Set(augmentedStart))

    // TODO (KR) : Debug
    val idt = Idt.Group(
      map.toList
        .map(_._2)
        .sortBy(_.id)
        .map {
          case StateMachine.State(id, rl) =>
            Idt.Group(
              s"${id.toString} =>",
              Idt.Indented(
                "reductions:",
                Idt.Indented(
                  rl.reductions.toList.map { r =>
                    Idt.Str(r.str)
                  },
                ),
                "accepts:",
                Idt.Indented(
                  rl.accepts.toList.map {
                    case (id, to) =>
                      Idt.Str(s"${id.str} => ${map(to).id}")
                  },
                ),
                "returns:",
                Idt.Indented(
                  rl.returns.toList.map {
                    case (name, idx, elements) =>
                      Idt.Str(s"${name.str}[$idx] (${elements.map(_.str).mkString(", ")})")
                  },
                ),
              ),
              Idt.Break,
            )
        },
    )

    println
    println(idt.build("|   "))

    StateMachine(input.startNt, map).right
  }

}
