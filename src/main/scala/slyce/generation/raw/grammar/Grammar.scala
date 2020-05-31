package slyce.generation.raw.grammar

import scala.annotation.tailrec

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
import slyce.generation.GenerationMessage._

class Grammar private (val initialNT: NonTerminal, map: Map[String, NonTerminal]) {}

object Grammar {

  def apply(initial: NonTerminal, others: NonTerminal*): ??[Grammar] = {
    @tailrec
    def loop(map: ??[Map[String, NonTerminal]], remaining: List[NonTerminal]): ??[Map[String, NonTerminal]] =
      remaining match {
        case Nil =>
          map
        case head :: tail =>
          val next: ??[Map[String, NonTerminal]] =
            map.flatMap { m =>
              m.get(head.name) match {
                case None =>
                  (m + ((head.name, head))).lift[??]
                case _: Some[NonTerminal] =>
                  Alive(m, NonFatal.DuplicateNTIgnored(head.name))
              }
            }
          loop(next, tail)
      }

    loop(
      Map[String, NonTerminal]().lift[??],
      initial :: others.toList
    ).map { m =>
      new Grammar(initial, m)
    }
  }

}
