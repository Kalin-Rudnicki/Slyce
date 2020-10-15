package slyce.implementations.generation.lexer

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer => MList}

import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.\/
import scalaz.\/-

import slyce.implementations.generation.lexer.Regex.CharClass
import helpers.TraverseOps

final case class Nfa(
    startMode: String,
    modes: List[(Data.Mode, Nfa.State)]
)

object Nfa {

  class State private {
    private val _transitions: MList[(CharClass, State)] = MList()
    private val _epsilonTransitions: MList[State] = MList()
    private var _end: Option[Data.Mode.Line] = None

    private def on(regex: Regex): Err \/ State =
      regex match {
        case chars: CharClass =>
          val newState = new State
          _transitions.append((chars, newState))
          newState.right
        case Regex.Sequence(seq) =>
          seq
            .foldLeft(this.right) {
              case (s, reg) =>
                s.flatMap(_.on(reg))
            }
            .map { s =>
              val newState = new State
              s._epsilonTransitions.append(newState)
              newState
            }
        case Regex.Group(seqs) =>
          seqs.list.toList
            .map(this.on)
            .traverseErrs
            .map { states =>
              val newState = new State
              states.foreach(_._epsilonTransitions.append(newState))
              newState
            }
        case Regex.Repeat(_, min, _) if min < 0 =>
          List(s"min ($min) < 0").left
        case Regex.Repeat(_, min, Some(max)) if max < min =>
          List(s"max ($max) < min ($min)").left
        case repeat: Regex.Repeat =>
          @tailrec
          def toMin(remaining: Int, current: State): Err \/ State =
            if (remaining > 0)
              current.on(repeat.reg) match {
                case err @ -\/(_) =>
                  err
                case \/-(res) =>
                  toMin(remaining - 1, res)
              }
            else
              current.right

          @tailrec
          def toMax(remaining: Int, current: State, past: List[State]): Err \/ State =
            if (remaining > 0)
              current.on(repeat.reg) match {
                case err @ -\/(_) =>
                  err
                case \/-(res) =>
                  toMax(remaining - 1, res, current :: past)
              }
            else {
              past.foreach(_._epsilonTransitions.append(current))
              current.right
            }

          (
            repeat match {
              case Regex.Repeat(_, min, None) if min == 0 =>
                for {
                  res <- this.on(repeat.reg)
                  _ = res._epsilonTransitions.append(this)
                } yield this
              case Regex.Repeat(_, min, None) =>
                for {
                  res1 <- toMin(min - 1, this)
                  res2 <- res1.on(repeat.reg)
                  _ = res2._epsilonTransitions.append(res1)
                } yield res2
              case Regex.Repeat(_, min, Some(max)) =>
                for {
                  res1 <- toMin(min, this)
                  res2 <- toMax(max - min, res1, Nil)
                } yield res2
            }
          ).map { res =>
            val newState = new State
            res._epsilonTransitions.append(newState)
            newState
          }
      }

    def transitions: List[(CharClass, State)] =
      _transitions.toList

    def epsilonTransitions: List[State] =
      _epsilonTransitions.toList

    def end: Option[Data.Mode.Line] =
      _end

  }

  object State {

    def fromLine(line: Data.Mode.Line): Err \/ State =
      for {
        _ <- ().right
        state = new State
        _ = state._end = line.some
        _ <- state.on(line.regex).leftMap(_.map(e => s"{line #${line.lineNo}} $e"))
      } yield state

    def join(states: List[State]): State = {
      val state = new State
      states.foreach(state._epsilonTransitions.append)
      state
    }

  }

}
