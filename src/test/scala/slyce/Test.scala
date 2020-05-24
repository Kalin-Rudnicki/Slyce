package slyce

import klib.fp.instances._
import klib.fp.ops._
import klib.handling.MessageAccumulator._
import slyce.generation.GenerationMessage._

object Test {

  def test[T](items: ??[T]*): Unit = {
    val list: List[??[T]] = items.toList
    println(list)
    println(list.invert)
    println(list.invertR)
    println
  }

  def main(args: Array[String]): Unit = {

    test(
      Alive(1, GenericMessage("~1.1"), GenericMessage("~1.2")),
      Alive(2, GenericMessage("~2.1"), GenericMessage("~2.2")),
      Alive(3, GenericMessage("~3.1"), GenericMessage("~3.2"))
    )

    test(
      Dead(GenericMessage("~1.1"), GenericMessage("~1.2")),
      Dead(GenericMessage("~2.1"), GenericMessage("~2.2")),
      Dead(GenericMessage("~3.1"), GenericMessage("~3.2"))
    )

    test(
      Alive(1, GenericMessage("~1.1"), GenericMessage("~1.2")),
      Dead(GenericMessage("~2.1"), GenericMessage("~2.2")),
      Alive(2, GenericMessage("~3.1"), GenericMessage("~3.2"))
    )

    println(
      Alive(1, GenericMessage("~1.1"), GenericMessage("~1.2"))
        .asInstanceOf[??[Int]]
        .flatMap(_ => Alive(2, GenericMessage("~2.1"), GenericMessage("~2.2")))
    )

  }

}
