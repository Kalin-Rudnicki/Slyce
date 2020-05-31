package slyce

import klib.dynamic._
import klib.fp.ops._

object Test {

  case class Person(var firstName: String, var lastName: String, var age: Int) {

    def asStr(f: Person => String): String =
      f(this)

  }

  def main(args: Array[String]): Unit = {

    val person: ??[Person] = Person("First-1", "Last-1", 1).lift[??]

    println(person.firstName)
    println(person.lastName)
    println(person.age)

    person.firstName = "First"
    person.lastName = "Last"
    person.age = 2

    println(person)
    println(person.asStr((p: Person) => s"${p.lastName}, ${p.firstName}"))

    // Everything above this line is working how I would hope
    val _4: ??[Int] = person.age - 2
    println(_4)

  }

}
