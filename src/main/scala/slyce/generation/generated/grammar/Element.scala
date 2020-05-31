package slyce.generation.generated.grammar

import scalaz.std.option.optionSyntax._

import klib.fp.instances._
import klib.fp.ops._
import slyce.generation.GenerationMessage._

sealed trait Element

object Element {

  def apply(name: String): ??[Element] =
    if (name.length == 0)
      Fatal.BadElementName(name).dead
    else
      name.charAt(0) match {
        case c if c.isLower =>
          ElementT(name).^[??]
        case c if c.isUpper =>
          ElementNT(name).^[??]
        case _ =>
          Fatal.BadElementName(name).dead
      }

  case class ElementTText(name: String) extends Element

  class ElementT private (val name: String) extends Element

  object ElementT {

    private[Element] def apply(name: String): ElementT =
      new ElementT(name)

    def unapply(arg: ElementT): Option[String] =
      arg.name.some

  }

  class ElementNT private (val name: String) extends Element

  object ElementNT {

    private[Element] def apply(name: String): ElementNT =
      new ElementNT(name)

    def unapply(arg: ElementNT): Option[String] =
      arg.name.some

  }

}

object Tmp {}
