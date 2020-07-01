package slyce.generation.raw.grammar

import scala.language.implicitConversions

import scalaz.Scalaz.ToOptionIdOps

import scala.annotation.tailrec

import klib.fp.instances.{given _}
import klib.fp.ops.{given _}
import klib.handling.MessageAccumulator._
import slyce.generation.GenerationMessage._
import slyce.generation.generated.{grammar => gen}

case class Grammar private (initialNT: String, map: Map[String, NonTerminal]) {
  
  
}

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
                  (m + ((head.name, head)))._lift[??]
                case _: Some[NonTerminal] =>
                  Alive(m, NonFatal.DuplicateNTIgnored(head.name))
              }
            }
          loop(next, tail)
      }

    loop(
      Map[String, NonTerminal]()._lift[??],
      initial :: others.toList
    ).map { m =>
      new Grammar(initial.name, m)
    }
  }

}
