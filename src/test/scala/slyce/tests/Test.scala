package slyce.tests

import scalaz.NonEmptyList

object Test extends App {

  sealed trait Identifier
  object Identifier {
    final case class Terminal(name: String) extends Identifier
    final case class NonTerminal(name: String) extends Identifier
  }

  final case class Tmp(before: NonEmptyList[Identifier], after: Option[NonEmptyList[Identifier]])

  val sets: List[(Set[Tmp], Int)] = List(
    Set(
      Tmp(
        before = NonEmptyList(
          Identifier.NonTerminal("A"),
          Identifier.Terminal("b"),
        ),
        after = None,
      ),
      Tmp(
        before = NonEmptyList(
          Identifier.Terminal("c"),
        ),
        after = None,
      ),
      Tmp(
        before = NonEmptyList(
          Identifier.Terminal("c"),
        ),
        after = Some(
          NonEmptyList(
            Identifier.NonTerminal("D"),
          ),
        ),
      ),
    ),
    Set(
      Tmp(
        before = NonEmptyList(
          Identifier.Terminal("c"),
        ),
        after = None,
      ),
      Tmp(
        before = NonEmptyList(
          Identifier.NonTerminal("A"),
          Identifier.Terminal("b"),
        ),
        after = None,
      ),
      Tmp(
        before = NonEmptyList(
          Identifier.Terminal("c"),
        ),
        after = Some(
          NonEmptyList(
            Identifier.NonTerminal("D"),
          ),
        ),
      ),
    ),
    Set(
      Tmp(
        before = NonEmptyList(
          Identifier.NonTerminal("A"),
          Identifier.Terminal("b"),
        ),
        after = None,
      ),
      Tmp(
        before = NonEmptyList(
          Identifier.Terminal("c"),
        ),
        after = None,
      ),
      Tmp(
        before = NonEmptyList(
          Identifier.NonTerminal("c"),
        ),
        after = Some(
          NonEmptyList(
            Identifier.NonTerminal("D"),
          ),
        ),
      ),
    ),
  ).zipWithIndex

  sets.foreach {
    case (s1, i1) =>
      println(s"=====| $i1 |=====")
      sets.foreach {
        case (s2, i2) =>
          println(s"$i2: ${s1 == s2}")
      }
      println()
  }

}
