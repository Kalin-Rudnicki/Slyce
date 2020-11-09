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

    {
      @tailrec
      def loop(
          known: Map[SimpleData.Name, Boolean],
          searching: List[(SimpleData.Name, Set[List[SimpleData.Identifier]])],
          waiting: List[(SimpleData.Name, Set[List[SimpleData.Identifier]])],
      ): List[String] \/ Map[SimpleData.Name, Boolean] = {
        // TODO (KR) : Debug

        // println(searching.map(_._1.str).mkString(", "))
        // println(waiting.map(_._1.str).mkString(", "))
        // System.exit(1)

        def listToIdt(listName: String, list: List[(SimpleData.Name, Set[List[SimpleData.Identifier]])]): Idt =
          Idt.Group(
            s"$listName",
            Idt.Indented(
              list.map {
                case (n, l) =>
                  Idt.Group(
                    s"${n.str} =>",
                    Idt.Indented(
                      l.toList.map { l2 =>
                        Idt.Str(s"${l2.size} : ${l2.map(_.str).mkString(" ")}")
                      },
                    ),
                  )
              },
            ),
          )

        print(
          Idt
            .Group(
              Idt.Break,
              Idt.Break,
              ">",
              Idt.Indented(
                "known =>",
                Idt.Indented(
                  known.toList.map {
                    case (n, cpt) =>
                      Idt.Str(s"${n.str} => $cpt")
                  },
                ),
                listToIdt("searching", searching),
                listToIdt("waiting", waiting),
              ),
            )
            .build("    "),
        )

        searching match {
          case Nil =>
            if (waiting.isEmpty)
              known.right
            else
              List(s"Un-handleable circular reference: ${waiting.map(_._1.str).mkString(", ")}").left
          case (name, nexts) :: tail =>
            @tailrec
            def loop2(
                progress: Boolean,
                unseen: List[List[SimpleData.Identifier]],
                seen: List[List[SimpleData.Identifier]],
            ): Boolean \/ (Boolean, Set[List[SimpleData.Identifier]]) =
              unseen match {
                case Nil =>
                  if (seen.isEmpty)
                    false.left
                  else
                    (progress, seen.toSet).right
                case h :: t =>
                  h match {
                    case SimpleData.Identifier.NonTerminal(name) :: t2 =>
                      known.get(name) match {
                        case Some(canPass) =>
                          if (canPass) {
                            loop2(
                              true,
                              t,
                              t2 :: seen,
                            )
                          } else
                            loop2(
                              true,
                              t,
                              seen,
                            )
                        case None =>
                          loop2(
                            progress,
                            t,
                            h :: seen,
                          )
                      }
                    case Nil =>
                      true.left
                    case _ =>
                      loop2(
                        true,
                        t,
                        seen,
                      )
                  }
              }

            loop2(false, nexts.toList, Nil) match {
              case -\/(canPass) =>
                println(s"3 : ${name.str}, $canPass, ${(tail ::: waiting).map(_._1.str).mkString(", ")}")
                loop(
                  known + (name -> canPass),
                  tail ::: waiting,
                  Nil,
                )
              case \/-((progress, lines)) =>
                val me = (name, lines)

                if (progress) {
                  // TODO (KR) :
                  println(1)
                  if ((tail ::: waiting).exists(_._1 == name))
                    System.exit(1)

                  loop(
                    known,
                    me :: tail ::: waiting,
                    Nil,
                  )
                } else {
                  // TODO (KR) :
                  println(2)
                  println(me._1.str)
                  println(tail.map(_._1.str).mkString(", "))
                  println(searching.map(_._1.str).mkString(", "))
                  if ((tail ::: waiting).exists(_._1 == name))
                    System.exit(1)

                  loop(
                    known,
                    tail,
                    me :: waiting,
                  )
                }
            }
        }
      }

      loop(
        Map(),
        nameMap.toList.map {
          case (name, v) =>
            name -> v.reductions.map(_.unseen)
        },
        Nil,
      )
    }.flatMap { canPassThrough =>
      // TODO (KR) : Debug
      print(
        Idt.Indented(
          "canPassThrough =>",
          Idt.Indented(
            canPassThrough.toList.map {
              case (n, cpt) =>
                Idt.Str(s"${n.str} => $cpt")
            },
          ),
        ),
      )

      val augmentedStart: StateMachine.ReductionList =
        mapReductionList(input.augmentedStart)._2.expand(nameMap, canPassThrough)

      @tailrec
      def loop(
          map: Map[StateMachine.ReductionList, StateMachine.State],
          todo: Set[StateMachine.ReductionList],
      ): Map[StateMachine.ReductionList, StateMachine.State] =
        todo.toList match {
          case Nil =>
            map
          case head :: tail =>
            val rl = head.expand(nameMap, canPassThrough)
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

}
