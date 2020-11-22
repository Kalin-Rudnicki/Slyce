package slyce.generate.lexer

import scala.annotation.tailrec

import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToOptionIdOps

import slyce.common.helpers._
import scalaz.\/

import slyce.generate.architecture.{lexer => arch}

object DataToNfa extends arch.DataToNfa[Data, Err, Nfa] {

  override def apply(input: Data): Err \/ Nfa = {
    def makeMode(mode: Data.Mode): Err \/ Nfa.State =
      mode.lines
        .map(Nfa.State.fromLine)
        .traverseErrs
        .map(Nfa.State.collect)

    input.modes
      .map(m => makeMode(m).map((m, _)))
      .traverseErrs
      .map { m1 =>
        val nfa = Nfa(
          input.startMode,
          m1.map {
            case (m2, s) =>
              m2.name -> s
          }.toMap,
        )

        {
          // DEBUG : (Start) ==================================================
          import klib.ColorString.syntax._
          import auto._
          import klib.Idt._
          import klib.Logger.GlobalLogger

          implicit val flags: Set[String] = Set("DataToNfa")

          GlobalLogger.break
          GlobalLogger.debug("=====| DataToNfa |=====")

          val allStates = nfa.modes.toSet
            .flatMap { (t: (String, Nfa.State)) =>
              t._2.findAll
            }
            .toList
            .zipWithIndex
            .toMap

          def mapStates(state: Nfa.State, stateF: Nfa.State => Set[Nfa.State]): String =
            stateF(state).filterNot(_ == state).map(ets => s"#${allStates(ets)}").mkString(", ")

          def stateName(
              state: Nfa.State,
              showMemAddr: Boolean = true,
              showNonTrivial: Boolean = true,
              showCanEpsilon: Boolean = true,
          ): String = {
            val idx = allStates(state)
            val memAddr = showMemAddr
              .option {
                val tmp = s" [${state.toString.split("\\$").last}]"
                state.nonTrivial.isEmpty ? tmp.red.toString | tmp
              }
              .getOrElse("")
            val nonTrivial = showNonTrivial.option(s" (${mapStates(state, _.nonTrivial)})").getOrElse("")
            val canEpsilon = showCanEpsilon.option(s" {${mapStates(state, _.canEpsilon)}}").getOrElse("")
            s"#$idx$memAddr$nonTrivial$canEpsilon"
          }

          GlobalLogger.debug(
            Group(
              "modes:",
              Indented(
                nfa.modes.toList.map {
                  case m -> s =>
                    s"$m => #${allStates(s)}"
                },
              ),
              "states:",
              Indented(
                allStates.toList.sortBy(_._2).map {
                  case s -> i =>
                    Group(
                      s"${stateName(s)} =>",
                      Indented(
                        s"isTrivial => ${s.isTrivial}",
                        s"allCanEpsilonTrivial => ${s.nonTrivial.isEmpty}",
                        "joinedTransitions =>",
                        Indented(
                          s.nonTrivial.toList.map { s2 =>
                            Group(
                              s"${stateName(s2)}:",
                              Indented(
                                s2.transitions.map {
                                  case c -> t =>
                                    s"$c => ${stateName(t)}"
                                },
                              ),
                            )
                          },
                        ),
                        "joinedEnds =>",
                        Indented(
                          s.nonTrivial.toList.map { s2 =>
                            Group(
                              s"${stateName(s2)}:",
                              Indented(
                                s2.`end`.map(_.toString),
                              ),
                            )
                          },
                        ),
                        /*
                        "transitions =>",
                        Indented(
                          s.transitions.map {
                            case c -> t =>
                              s"$c => #${allStates(t)}"
                          },
                        ),
                         */
                        /*
                        "epsilonTransitions =>",
                        Indented(
                          s.epsilonTransitions.map { e =>
                            s"#${allStates(e)}"
                          },
                        ),
                         */
                      ),
                    )
                },
              ),
            ),
          )
          GlobalLogger.break
          import scala.collection.mutable.{Map => MMap}
          import scala.collection.mutable.{Set => MSet}

          def sName(s: Nfa.State, cIsNew: Boolean): String = {
            val sn = stateName(s)
            cIsNew ? sn | sn.magenta.toString
          }

          def recBuild(
              state: Nfa.State,
              map: MMap[Nfa.State, MSet[List[Int]]],
              prevPath: List[Int],
          ): Option[Idt] = {
            val mSet = map.getOrElseUpdate(state, MSet())
            val isNew = mSet.isEmpty
            mSet.add(prevPath.reverse)

            isNew.option {
              val currentPath = allStates(state) :: prevPath

              Indented(
                "{",
                Indented(
                  state.epsilonTransitions.map { s =>
                    val c = recBuild(s, map, currentPath)
                    Group(
                      s"_ ${"=>".cyan.toString} ${sName(s, c.nonEmpty)}",
                      c,
                    )
                  },
                  state.transitions.map {
                    case (cc, s) =>
                      val c = recBuild(s, map, currentPath)
                      Group(
                        s"$cc ${"=>".blue.toString} ${sName(s, c.nonEmpty)}",
                        c,
                      )
                  },
                  state.`end`.map(e => s"${"<=".yellow.toString} $e"),
                ),
                "}",
              )
            }
          }

          val mMap: MMap[Nfa.State, MSet[List[Int]]] = MMap()
          val idt = Group(
            nfa.modes.toList.map {
              case name -> s =>
                val c = recBuild(s, mMap, Nil)
                Group(
                  s"$name: ${sName(s, c.nonEmpty)}",
                  c,
                )
            },
          )
          GlobalLogger.debug(idt.build("|   ".green.toString))
          GlobalLogger.debug(
            Group(
              mMap.toList.map {
                case k -> v =>
                  Group(
                    Break,
                    s"${stateName(k)}:",
                    Indented(
                      v.toList.map(p => p.map(p2 => s"#$p2").mkString(" => ")),
                    ),
                  )
              },
            ).build("|   ".green.toString),
          )

          // DEBUG : (End) ==================================================
        }

        nfa
      }
  }

}
