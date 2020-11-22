package slyce.generate.lexer

import scala.annotation.tailrec

import scalaz.\/
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps

import slyce.common.helpers._
import slyce.generate.architecture.{lexer => arch}

object NfaToDfa extends arch.NfaToDfa[Nfa, Err, Dfa] {
  override def apply(input: Nfa): Err \/ Dfa = {
    final case class TmpDfaState(
        modeName: String,
        transitions: Map[Set[Char], Option[Set[Nfa.State]]],
        elseTransition: Option[Set[Nfa.State]],
        line: Option[Data.Mode.Line],
    ) {

      val stateSets: Set[Set[Nfa.State]] =
        (elseTransition.toList ::: transitions.toList.flatMap(_._2)).toSet

    }

    def joinNfaStates(modeName: String, states: Set[Nfa.State]): TmpDfaState = {
      val nonTrivialStates = Nfa.State.nonTrivial(states)
      val transitionPairs = nonTrivialStates.toList.flatMap(_.transitions)

      val specifiedChars: Set[Char] = transitionPairs.flatMap(_._1.chars).toSet
      val (inclusiveChars, exclusiveChars) =
        transitionPairs.map(_._1).partitionMap {
          case Regex.CharClass.Inclusive(chars) =>
            chars.left.toEither
          case Regex.CharClass.Exclusive(chars) =>
            chars.right.toEither
        } match {
          case (i, e) =>
            val inc = i.toSet.flatten
            val exc = e.toSet.flatten
            (inc, exc)
        }
      val exclusiveNonInclusiveChars = exclusiveChars &~ inclusiveChars

      {
        // DEBUG : (Start) ==================================================
        import klib.ColorString.syntax._
        import auto._
        import klib.CharStringOps._
        import klib.Idt._
        import klib.Logger.GlobalLogger

        implicit val flags: Set[String] = Set("NfaToDfa")

        GlobalLogger.break
        GlobalLogger.debug("=====| NfaToDfa |=====")
        GlobalLogger.debug(
          Group(
            inclusiveChars.map(_.unesc).toString,
            exclusiveChars.map(_.unesc).toString,
            exclusiveNonInclusiveChars.map(_.unesc).toString,
          ),
        )

        // DEBUG : (End) ==================================================
      }

      val transitions: Map[Set[Char], Option[Set[Nfa.State]]] =
        specifiedChars.toList
          .map { c =>
            c -> Nfa.State.nonTrivial(transitionPairs.filter(_._1.contains(c)).map(_._2).toSet)
          }
          .groupMap(_._2)(_._1)
          .toList
          .map {
            case (v, k) =>
              k.toSet -> v.nonEmpty.option(v)
          }
          .toMap
      val elseTransition: Option[Set[Nfa.State]] = {
        val s = Nfa.State.nonTrivial(
          transitionPairs.collect {
            case (_: Regex.CharClass.Exclusive, ss) =>
              ss
          }.toSet,
        )
        s.nonEmpty.option(s)
      }
      val line: Option[Data.Mode.Line] =
        nonTrivialStates.toList.flatMap(_.end).minByOption(_.lineNo)

      TmpDfaState(
        modeName,
        transitions,
        elseTransition,
        line,
      )
    }

    @tailrec
    def buildMap(
        modeName: String,
        unseen: Set[Set[Nfa.State]],
        map: Map[Set[Nfa.State], TmpDfaState] = Map(),
        seen: Set[Set[Nfa.State]] = Set(),
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
            newSeen,
          )
      }

    val nfaModeMap: Map[String, (Set[Nfa.State], Map[Set[Nfa.State], TmpDfaState])] =
      input.modes.map {
        case (k, v) =>
          val initialStateSet: Set[Nfa.State] = v.nonTrivial
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
            ().right,
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
                          dfaFromNfas(to),
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
                      dfaFromNfas(modeStarts(tmpState.modeName)),
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
          List(s"Invalid start mode '${input.startMode}'").left
      }
    } yield dfa
  }

}
