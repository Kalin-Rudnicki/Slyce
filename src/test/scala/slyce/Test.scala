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
      Alive(1, GenericWarning("~1.1"), GenericWarning("~1.2")),
      Alive(2, GenericWarning("~2.1"), GenericWarning("~2.2")),
      Alive(3, GenericWarning("~3.1"), GenericWarning("~3.2"))
    )

    test(
      Dead(GenericWarning("~1.1"), GenericWarning("~1.2")),
      Dead(GenericWarning("~2.1"), GenericWarning("~2.2")),
      Dead(GenericWarning("~3.1"), GenericWarning("~3.2"))
    )

    test(
      Alive(1, GenericWarning("~1.1"), GenericWarning("~1.2")),
      Dead(GenericWarning("~2.1"), GenericWarning("~2.2")),
      Alive(2, GenericWarning("~3.1"), GenericWarning("~3.2"))
    )

    println(
      Alive(1, GenericWarning("~1.1"), GenericWarning("~1.2"))
        .asInstanceOf[??[Int]]
        .flatMap(_ => Alive(2, GenericWarning("~2.1"), GenericWarning("~2.2")))
    )

  }

}
