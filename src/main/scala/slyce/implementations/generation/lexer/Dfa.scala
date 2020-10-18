package slyce.implementations.generation.lexer

import scalaz.Scalaz.ToBooleanOpsFromBoolean

import helpers._

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

  def toksStr(implicit idt: String): List[String] = {
    def tokString(tokName: String): String =
      s"final case class $tokName(text: String, start: Dfa.Token.Pos, stop: Dfa.Token.Pos) extends Token"

    val names = idxOf.toList.flatMap(_._1.yields.toList.flatMap(_.yields.map(_.name))).distinct.sorted

    List(
      List(
        "sealed trait Token extends Dfa.Token",
        "object Token {"
      ),
      idtStrs(names.map(tokString): _*),
      "}" :: Nil
    ).flatten
  }

  def dfaStr(implicit idt: String): List[String] = {
    def stateName(state: Dfa.State): String =
      s"s${this.idxOf(state)}"

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
            s"id = ${idxOf(state)},"
          ),
          state.transitions.isEmpty.fold(
            idtStrs(
              "transitions = Map(),"
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
                  }: _*
              ),
              ")," :: Nil
            )
          ),
          idtStrs(
            s"elseTransition = ${state.elseTransition.map(lazyName)},"
          ),
          state.yields.fold(
            idtStrs(
              "yields = None,"
            )
          ) { yields =>
            yields.yields.isEmpty.fold(
              idtStrs(
                s"yields = Some(Dfa.State.Yields(${stateName(yields.toMode)})()),"
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
                          s"textRange = ${y.textRange},"
                        ),
                        ")," :: Nil
                      ).flatten
                    }: _*
                  ),
                  ")," :: Nil
                ),
                ")," :: Nil
              )
            )
          },
          ")" :: Nil
        )
      ).flatten
    }

    List(
      "val dfa: Dfa[Token] = {" :: Nil,
      idtLists(idxOf.toList.sortBy(_._2).map(p => stateStringLines(p._1)): _*),
      "" :: Nil,
      idtStrs(s"Dfa(${stateName(initialState)})"),
      "}" :: Nil
    ).flatten
  }

}

object Dfa {

  final class State {

    var transitions: Map[Set[Char], Option[State]] = _
    var elseTransition: Option[State] = _
    var yields: Option[State.Yields] = None

    def findAll: Set[State] =
      State.findAll(Set(this))

  }

  object State {

    final case class Yields(
        yields: List[Yield],
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
