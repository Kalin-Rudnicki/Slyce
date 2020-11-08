package slyce.generate.formatting

import scalaz.\/

import slyce.common.helpers._
import slyce.common.helpers.Idt._
import slyce.generate.architecture.{formatting => arch}
import slyce.generate.lexer._
import Yields.Yield
import scalaz.Scalaz.{ToBooleanOpsFromBoolean, ToEitherOps}

object DfaLines extends arch.DfaLines[Dfa] {

  override def apply(dfa: Dfa): List[String] \/ Idt = {
    def stateName(state: Dfa.State): String =
      s"s${dfa.idxOf(state)}"

    def stateStringLines(state: Dfa.State): Idt = {
      def lazyName(state: Dfa.State): String =
        s"Lazy(${stateName(state)})"

      // TODO (KR) : Is this unused?
      def yieldsStr(yields: Dfa.State.Yields): String = {
        val lambdaParam: String = "s"

        def yStr(y: Yield): String =
          s"Token.${y.name}($lambdaParam)"

        s"Dfa.State.Yield(${stateName(yields.toMode)})(${yields.yields.nonEmpty.fold(lambdaParam, "_")} => ${yields.yields.map(yStr)})"
      }

      Group(
        s"lazy val ${stateName(state)}: Dfa.State[Token] =",
        Indented(
          "Dfa.State(",
          Indented(
            s"id = ${dfa.idxOf(state)},",
          ),
          state.transitions.isEmpty.fold(
            Indented(
              "transitions = Map(),",
            ),
            Indented(
              "transitions = Map(",
              Indented(
                state.transitions.toList
                  .flatMap {
                    case (ss, s) =>
                      ss.map(_ -> s)
                  }
                  .sortBy(_._1)
                  .map {
                    case (c, s) =>
                      // Seems like the safest way to avoid all sorts of weird character escapes
                      Str(s"0x${c.toInt.toHexString.toUpperCase}.toChar -> ${s.map(lazyName)}, // ${c.unescape}")
                  },
              ),
              "),",
            ),
          ),
          Indented(
            s"elseTransition = ${state.elseTransition.map(lazyName)},",
          ),
          state.yields.fold(
            Indented(
              "yields = None,",
            ),
          ) { yields =>
            yields.yields.isEmpty.fold(
              Indented(
                s"yields = Some(Dfa.State.Yields(${stateName(yields.toMode)})()),",
              ),
              Indented(
                s"yields = Some(",
                Indented(
                  s"Dfa.State.Yields(${stateName(yields.toMode)})(",
                  Indented(
                    yields.yields.map { y =>
                      Group(
                        "Dfa.State.Yields.Yield(",
                        Indented(
                          s"tokF = Token.${y.name}.apply,",
                          s"spanRange = ${y.spanRange},",
                        ),
                        "),",
                      )
                    },
                  ),
                  "),",
                ),
                "),",
              ),
            )
          },
          ")",
        ),
      )
    }

    Group(
      "val dfa: Dfa[Token] = {",
      Indented(dfa.idxOf.toList.sortBy(_._2).map(p => stateStringLines(p._1))),
      Break,
      Indented(s"Dfa(${stateName(dfa.initialState)})"),
      "}",
    ).right
  }

}
