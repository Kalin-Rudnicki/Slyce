package slyce.generate.grammar

import scala.annotation.tailrec
import scala.tools.nsc.backend.jvm.BackendReporting

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
        case SimpleData.ReductionList(name, reductions, _) =>
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

    {
      // DEBUG : (Start) ==================================================
      import klib.ColorString.syntax._
      import auto._
      import klib.Idt._
      import klib.Logger.GlobalLogger

      implicit val flags: Set[String] = Set("SimpleDataToStateMachine")

      GlobalLogger.break
      GlobalLogger.debug("=====| SimpleDataToStateMachine |=====")
      GlobalLogger.debug(
        Group(
          map.toList
            .map(_._2)
            .sortBy(_.id)
            .map {
              case StateMachine.State(id, rl) =>
                Group(
                  s"${id.toString} =>",
                  Indented(
                    "reductions:",
                    Indented(
                      rl.reductions.toList.map { r =>
                        Str(r.str)
                      },
                    ),
                    "accepts:",
                    Indented(
                      rl.accepts.toList.map {
                        case (id, to) =>
                          Str(s"${id.str} => ${map(to).id}")
                      },
                    ),
                    "returns:",
                    Indented(
                      rl.returns.toList.map {
                        case (name, idx, elements) =>
                          Str(s"${name.str}[$idx] (${elements.map(_.str).mkString(", ")})")
                      },
                    ),
                  ),
                  Break,
                )
            },
        ),
      )

      // DEBUG : (End) ==================================================
    }

    StateMachine(input.startNt, map).right
  }

}
