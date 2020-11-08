package slyce.tests

import scalaz.Scalaz.ToOptionIdOps

import slyce.common.helpers.Matcher

object Test extends App {

  def opMatcher(f: (Int, Int) => Int): Matcher[(Int, Int), Int] = {
    case (_1, _2) =>
      f(_1, _2).some
  }

  val add: Matcher[(Int, Int), Int] =
    opMatcher((_1, _2) => _1 + _2)

  val subtract: Matcher[(Int, Int), Int] =
    opMatcher((_1, _2) => _1 - _2)

  val multiply: Matcher[(Int, Int), Int] =
    opMatcher((_1, _2) => _1 * _2)

  val tryDiv: Matcher[(Int, Int), Int] = {
    case (_1, _2) if _2 != 0 =>
      (_1 / _2).some
    case _ =>
      None
  }

  val pairs: List[(Int, Int)] =
    List(
      (1, 2),
      (3, 4),
      (1, 5),
      (25, 3),
      // (5, 0),
    )

  def run(matcher: Matcher[(Int, Int), Int]): Unit =
    println(
      pairs.map {
        case matcher(res) =>
          res
      },
    )

  run(add)
  run(subtract)
  run(multiply)
  run(tryDiv)

}
