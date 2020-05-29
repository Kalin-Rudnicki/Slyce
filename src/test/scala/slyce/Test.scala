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
      Alive(1, NonFatal.BasicMessage("~1.1"), NonFatal.BasicMessage("~1.2")),
      Alive(2, NonFatal.BasicMessage("~2.1"), NonFatal.BasicMessage("~2.2")),
      Alive(3, NonFatal.BasicMessage("~3.1"), NonFatal.BasicMessage("~3.2"))
    )

    test(
      Dead(NonFatal.BasicMessage("~1.1"), NonFatal.BasicMessage("~1.2")),
      Dead(NonFatal.BasicMessage("~2.1"), NonFatal.BasicMessage("~2.2")),
      Dead(NonFatal.BasicMessage("~3.1"), NonFatal.BasicMessage("~3.2"))
    )

    test(
      Alive(1, NonFatal.BasicMessage("~1.1"), NonFatal.BasicMessage("~1.2")),
      Dead(NonFatal.BasicMessage("~2.1"), NonFatal.BasicMessage("~2.2")),
      Alive(2, NonFatal.BasicMessage("~3.1"), NonFatal.BasicMessage("~3.2"))
    )

    println(
      Alive(1, NonFatal.BasicMessage("~1.1"), NonFatal.BasicMessage("~1.2"))
        .asInstanceOf[??[Int]]
        .flatMap(_ => Alive(2, NonFatal.BasicMessage("~2.1"), NonFatal.BasicMessage("~2.2")))
    )

  }

}
