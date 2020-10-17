package slyce.implementations.generation.lexer

import scala.annotation.tailrec

import scalaz.\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps

import helpers.TraverseOps
import slyce.architecture.generation.lexer.{NfaToDfa => NfaToDfaF}

object NfaToDfa extends NfaToDfaF[Nfa, Err, Dfa] {
  override def apply(input: Nfa): Err \/ Dfa = {
    final case class TmpDfaState(
        modeName: String,
        transitions: Map[Set[Char], Option[Set[Nfa.State]]],
        elseTransition: Option[Set[Nfa.State]],
        line: Option[Data.Mode.Line]
    ) {

      val stateSets: Set[Set[Nfa.State]] =
        (elseTransition.toList ::: transitions.toList.flatMap(_._2)).toSet

    }

    @tailrec
    def nonTrivial(unseen: Set[Nfa.State], seen: Set[Nfa.State] = Set()): Set[Nfa.State] =
      if (unseen.isEmpty)
        seen
      else {
        val nowSeen = unseen | seen
        nonTrivial(seen.flatMap(_.epsilonTransitions) &~ nowSeen, nowSeen)
      }

    def joinNfaStates(modeName: String, states: Set[Nfa.State]): TmpDfaState = {
      val nonTrivialStates = nonTrivial(states)
      val transitionPairs = nonTrivialStates.toList.flatMap(_.transitions)

      val specifiedChars: Set[Char] = transitionPairs.flatMap(_._1.chars).toSet
      val transitions: Map[Set[Char], Option[Set[Nfa.State]]] =
        specifiedChars.toList
          .map { c =>
            c -> nonTrivial(transitionPairs.filter(_._1.contains(c)).map(_._2).toSet)
          }
          .groupMap(_._2)(_._1)
          .toList
          .map {
            case (v, k) =>
              k.toSet -> v.nonEmpty.option(v)
          }
          .toMap
      val elseTransition: Option[Set[Nfa.State]] = {
        val s = nonTrivial(
          transitionPairs
            .filter {
              case (_: Regex.CharClass.Exclusive, _) =>
                true
              case _ =>
                false
            }
            .map(_._2)
            .toSet
        )
        s.nonEmpty.option(s)
      }
      val line: Option[Data.Mode.Line] =
        nonTrivialStates.toList.flatMap(_.end).minByOption(_.lineNo)

      TmpDfaState(
        modeName,
        transitions,
        elseTransition,
        line
      )
    }

    @tailrec
    def buildMap(
        modeName: String,
        unseen: Set[Set[Nfa.State]],
        map: Map[Set[Nfa.State], TmpDfaState] = Map(),
        seen: Set[Set[Nfa.State]] = Set()
    ): Map[Set[Nfa.State], TmpDfaState] =
      unseen.toList match {
        case Nil =>
          map
        case head :: tail =>
          val tmpDfa = joinNfaStates(modeName, head)
          val newSeen = seen + head
          buildMap(
            modeName,
            (tail.toSet | tmpDfa.stateSets) &~ newSeen,
            map + (head -> tmpDfa),
            newSeen
          )
      }

    val nfaModeMap: Map[String, (Set[Nfa.State], Map[Set[Nfa.State], TmpDfaState])] =
      input.modes.map {
        case (k, v) =>
          val initialStateSet: Set[Nfa.State] = nonTrivial(Set(v))
          val mapped: Map[Set[Nfa.State], TmpDfaState] = buildMap(k, Set(initialStateSet))
          k -> (initialStateSet -> mapped)
      }

    val modeStarts: Map[String, Set[Nfa.State]] =
      nfaModeMap.map {
        case (k, (v, _)) =>
          k -> v
      }

    val tmpMap: Map[Set[Nfa.State], TmpDfaState] =
      nfaModeMap.toList.flatMap {
        case (_, (_, v)) =>
          v.toList
      }.toMap

    val dfaMap: Map[TmpDfaState, Dfa.State] =
      tmpMap.map {
        case (_, v) =>
          v -> new Dfa.State
      }

    def dfaFromNfas(nfas: Set[Nfa.State]): Dfa.State =
      dfaMap(tmpMap(nfas))

    val modeStartCanNotYield =
      modeStarts.toList.map {
        case (m, s) =>
          // TODO (KR) : Find this earlier, its not very helpful to only know the mode, should know the line that causes it
          tmpMap(s).line.isDefined.fold(
            List(s"Mode $m can yield on no input").left,
            ().right
          )
      }.traverseErrs

    val toDfa =
      dfaMap.toList.map {
        case (tmpState, dfaState) =>
          dfaState.transitions = tmpState.transitions.map {
            case (on, to) =>
              on ->
                to.map(dfaFromNfas)
          }
          dfaState.elseTransition = tmpState.elseTransition.map(dfaFromNfas)
          tmpState.line match {
            case Some(Data.Mode.Line(lineNo, _, Yields(yields, toMode))) =>
              toMode match {
                case Some(toStr) =>
                  modeStarts.get(toStr) match {
                    case Some(to) =>
                      dfaState.yields = Dfa.State
                        .Yields(
                          yields,
                          dfaFromNfas(to)
                        )
                        .some
                      ().right
                    case None =>
                      List(s"{Line #$lineNo} Invalid mode '$toStr'").left
                  }
                case None =>
                  dfaState.yields = Dfa.State
                    .Yields(
                      yields,
                      dfaFromNfas(modeStarts(tmpState.modeName))
                    )
                    .some
                  ().right
              }
            case None =>
              ().right
          }
      }.traverseErrs

    for {
      _ <- modeStartCanNotYield
      _ <- toDfa
      dfa <- modeStarts.get(input.startMode) match {
        case Some(s) =>
          Dfa(dfaFromNfas(s)).right
        case None =>
          List("Invalid start mode name").left
      }
    } yield dfa
  }

}
