package slyce.implementations.generation.lexer

import helpers.CharOps

import Yields.Yield

final case class Dfa(initialState: Dfa.State)

object Dfa {

  final class State {

    var transitions: Map[Set[Char], Option[State]] = _
    var elseTransition: Option[State] = _
    var yields: Option[State.Yields] = None

    def findAll: Set[State] =
      State.findAll(Set(this))

    def show: String = {
      val all = this.findAll.toList.zipWithIndex.toMap

      (
        s"initial-state: ${all(this)}" ::
          s"num-states:   ${all.size}" ::
          all.toList.flatMap {
            case (state, idx) =>
              List(
                s"$idx:" :: Nil,
                "\ttransitions:" :: Nil,
                state.transitions.map(s =>
                  s"\t\t(${s._1.toList.sorted.map(_.unescape).mkString(", ")}) => ${s._2.map(all)}"
                ),
                "\telse-transition:" :: Nil,
                state.elseTransition.map(s => s"\t\t${all(s)}"),
                "\tyields:" :: Nil,
                state.yields.toList.flatMap { y =>
                  List(
                    s"\t\t${all(y.toMode)}",
                    s"\t\t${y.yields}"
                  )
                }
              ).flatten
          }
      ).mkString("\n")
    }

  }

  object State {

    final case class Yields(
        yields: Option[Yield],
        toMode: State
    )

    def findAll(unseen: Set[State], seen: Set[State] = Set()): Set[State] =
      helpers.findAll(unseen, seen) { s =>
        s.transitions.toList.flatMap(_._2.toList).toSet |
          s.elseTransition.toSet |
          s.yields.map(_.toMode).toSet
      }

  }

}
