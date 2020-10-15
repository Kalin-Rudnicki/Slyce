package slyce.implementations.generation.lexer

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer => MList}

import scalaz.-\/
import scalaz.Scalaz.ToEitherOps
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption
import scalaz.\/
import scalaz.\/-

import slyce.architecture.generation.lexer.{DataToNfa => DataToNfaF}
import slyce.implementations.generation.lexer.Regex.CharClass
import helpers.TraverseOps

object DataToNfa extends DataToNfaF[Data, Err, Nfa] {

  override def apply(input: Data): Err \/ Nfa = {
    def makeMode(mode: Data.Mode): Err \/ Nfa.State =
      mode.lines
        .map(Nfa.State.fromLine)
        .traverseErrs
        .map(Nfa.State.join)

    val startMode: Err \/ Data.Mode =
      input.modes
        .find(_.name == input.startMode) \/>
        List("Could not find start mode")

    val modes: Err \/ List[(Data.Mode, Nfa.State)] =
      input.modes
        .map(m => makeMode(m).map((m, _)))
        .traverseErrs

    ???
  }

}
