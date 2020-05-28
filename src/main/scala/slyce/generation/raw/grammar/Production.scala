package slyce.generation.raw.grammar

object Production {

  class BasicProduction(
      val elements: List[Element]
  )

  class UnwrapProduction(
      val preElements: List[Element],
      val unwrapElement: Element,
      val postElements: List[Element]
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

  class OpProduction(
      val tipe: OpProduction.Type,
      val assoc: OpProduction.Assoc,
      val assocElement: Element
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

  }

}
