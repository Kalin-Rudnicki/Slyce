package slyce.generation.raw.grammar

object Production {

  class BasicProduction private (
      val elements: List[Either[String, String]]
  )

  object BasicProduction {

    def apply(elements: Either[String, String]*): BasicProduction =
      new BasicProduction(elements.toList)

  }

  class UnwrapProduction(
      val preElements: List[String],
      val unwrapElement: String,
      val postElements: List[String]
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
      val `type`: OpProduction.Type,
      val assoc: OpProduction.Assoc,
      val assocElement: String
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
