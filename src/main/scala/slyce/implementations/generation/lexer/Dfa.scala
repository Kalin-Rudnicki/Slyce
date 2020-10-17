package slyce.implementations.generation.lexer

import scalaz.Scalaz.ToBooleanOpsFromBoolean

import helpers.CharOps

import Yields.Yield

final case class Dfa(initialState: Dfa.State) {

  private val idxOf: Map[Dfa.State, Int] = {
    val tmp = initialState.findAll.toList.zipWithIndex.toMap

    tmp.map {
      case (s @ this.initialState, _) =>
        s -> 0
      case (s, 0) =>
        s -> tmp(initialState)
      case (s, i) =>
        s -> i
    }
  }

  def output(name: String, pkg: List[String], idt: String = "  "): String = {
    def idtStrs(strs: String*): List[String] =
      strs.toList.map(s => s"$idt$s")

    def idtLists(strLists: List[String]*): List[String] =
      strLists.toList.flatMap(idtStrs(_: _*))

    def tokString(tokName: String): String =
      s"final case class $tokName(text: String) extends Token"

    def stateName(state: Dfa.State): String =
      s"s${this.idxOf(state)}"

    def stateString(state: Dfa.State): String = {
      def lazyName(state: Dfa.State): String =
        s"Lazy(${stateName(state)})"

      def yieldsStr(yields: Dfa.State.Yields): String = {
        val lambdaParam: String = "s"

        def yStr(y: Yield): String =
          s"Token.${y.name}($lambdaParam)"

        s"Dfa.State.Yield(${stateName(yields.toMode)})($lambdaParam => ${yields.yields.map(yStr)})"
      }

      List(
        s"lazy val ${stateName(state)}: Dfa.State[Token] =" :: Nil,
        idtLists(
          "Dfa.State(" :: Nil,
          state.transitions.isEmpty.fold(
            idtStrs(
              "transition = Map(),"
            ),
            idtLists(
              "transition = Map(" :: Nil,
              idtStrs(
                state.transitions.toList
                  .flatMap {
                    case (ss, s) =>
                      ss.map(_ -> s)
                  }
                  .sortBy(_._1)
                  .map {
                    case (c, s) =>
                      s"${c.toInt} -> ${s.map(lazyName)}, // ${c.unescape}"
                  }: _*
              ),
              ")," :: Nil
            )
          ),
          idtStrs(
            s"elseTransition = ${state.elseTransition.map(lazyName)},",
            s"yields = ${state.yields.map(yieldsStr)},"
          ),
          ")" :: Nil
        )
      ).flatten.mkString("\n")
    }

    idxOf.toList
      .sortBy(_._2)
      .map {
        case (state, _) =>
          stateString(state)
      }
      .mkString("\n")
  }

}

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
