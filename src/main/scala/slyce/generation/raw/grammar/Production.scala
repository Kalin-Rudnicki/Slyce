package slyce.generation.raw.grammar

import scalaz.\/

object Production {

  // Text \/ Terminal
  type TextOrTerm = String \/ String

  class BasicProduction private (
      val elements: List[TextOrTerm]
  )

  object BasicProduction {

    def apply(elements: TextOrTerm*): BasicProduction =
      new BasicProduction(elements.toList)

  }

  case class UnwrapProduction(
      preElements: List[String],
      unwrapElement: String,
      postElements: List[String]
  )

  sealed trait ListProduction

  object ListProduction {

    case class SimpleListAny(
        list0: UnwrapProduction
    ) extends ListProduction

    case class SimpleListMany(
        list0: UnwrapProduction
    ) extends ListProduction

    case class CompoundListAny(
        list0: UnwrapProduction,
        list1: UnwrapProduction
    ) extends ListProduction

    case class CompoundListMany(
        list0: UnwrapProduction,
        list1: UnwrapProduction
    ) extends ListProduction

  }

  case class OpProduction(
      `type`: OpProduction.Type,
      assoc: OpProduction.Assoc,
      assocElement: TextOrTerm
  )

  object OpProduction {

    sealed trait Type

    object Type {
      case object Unary extends Type
      case object Binary extends Type
    }

    sealed trait Assoc

    object Assoc {
      case object Left extends Assoc
      case object Right extends Assoc
    }

    // =====| Helpers |=====
    
    def binLeft(assocElement: TextOrTerm): OpProduction =
      OpProduction(
        Type.Binary,
        Assoc.Left,
        assocElement
      )
    
  }

}
