package slyce.generate.lexer

import scalaz.\/
import slyce.common.helpers._
import slyce.generate.architecture.{lexer => arch}
import Yields.Yield
import scalaz.Scalaz.{ToBooleanOpsFromBoolean, ToEitherOps}

object DfaStateLines extends arch.DfaStateLines[Dfa] {

  override def apply(input: (Dfa, String)): List[String] \/ List[String] = {
    val dfa: Dfa = input._1
    implicit val idt: String = input._2

    def stateName(state: Dfa.State): String =
      s"s${dfa.idxOf(state)}"

    def stateStringLines(state: Dfa.State): List[String] = {
      def lazyName(state: Dfa.State): String =
        s"Lazy(${stateName(state)})"

      def yieldsStr(yields: Dfa.State.Yields): String = {
        val lambdaParam: String = "s"

        def yStr(y: Yield): String =
          s"Token.${y.name}($lambdaParam)"

        s"Dfa.State.Yield(${stateName(yields.toMode)})(${yields.yields.nonEmpty.fold(lambdaParam, "_")} => ${yields.yields.map(yStr)})"
      }

      List(
        s"lazy val ${stateName(state)}: Dfa.State[Token] =" :: Nil,
        idtLists(
          "Dfa.State(" :: Nil,
          idtStrs(
            s"id = ${dfa.idxOf(state)},",
          ),
          state.transitions.isEmpty.fold(
            idtStrs(
              "transitions = Map(),",
            ),
            idtLists(
              "transitions = Map(" :: Nil,
              idtStrs(
                state.transitions.toList
                  .flatMap {
                    case (ss, s) =>
                      ss.map(_ -> s)
                  }
                  .sortBy(_._1)
                  .map {
                    case (c, s) =>
                      // Seems like the safest way to avoid all sorts of weird character escapes
                      s"0x${c.toInt.toHexString.toUpperCase}.toChar -> ${s.map(lazyName)}, // ${c.unescape}"
                  }: _*,
              ),
              ")," :: Nil,
            ),
          ),
          idtStrs(
            s"elseTransition = ${state.elseTransition.map(lazyName)},",
          ),
          state.yields.fold(
            idtStrs(
              "yields = None,",
            ),
          ) { yields =>
            yields.yields.isEmpty.fold(
              idtStrs(
                s"yields = Some(Dfa.State.Yields(${stateName(yields.toMode)})()),",
              ),
              idtLists(
                s"yields = Some(" :: Nil,
                idtLists(
                  s"Dfa.State.Yields(${stateName(yields.toMode)})(" :: Nil,
                  idtLists(
                    yields.yields.map { y =>
                      List(
                        "Dfa.State.Yields.Yield(" :: Nil,
                        idtStrs(
                          s"tokF = Token.${y.name}.apply,",
                          s"spanRange = ${y.spanRange},",
                        ),
                        ")," :: Nil,
                      ).flatten
                    }: _*,
                  ),
                  ")," :: Nil,
                ),
                ")," :: Nil,
              ),
            )
          },
          ")" :: Nil,
        ),
      ).flatten
    }

    List(
      "val dfa: Dfa[Token] = {" :: Nil,
      idtLists(dfa.idxOf.toList.sortBy(_._2).map(p => stateStringLines(p._1)): _*),
      "" :: Nil,
      idtStrs(s"Dfa(${stateName(dfa.initialState)})"),
      "}" :: Nil,
    ).flatten.right
  }

}
