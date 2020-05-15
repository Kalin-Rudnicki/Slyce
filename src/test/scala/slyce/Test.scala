package slyce

import slyce.errors.{MessageAccumulator => MA}
import MA.implicits._
import MA.{MessageType => MT}

object Test {
  
  def main(args: Array[String]): Unit = {
  
    import MA._
    
    val v1: MA[String, Int] = 5.value <#> (i => Alive(i + 1, "1", "2")) <#> (i => Alive(i + 1, "3", "4")) << ("5", "Oops") <#> (_ => Dead("0", "Oof"))
    val v2: MA[String, Int] = 5.value <#> ((i: Int) => i + 1)("1", "2") <#> ((i: Int) => i + 1)("3", "4") << ("5", "Oops") <#> (_ => Dead("0", "Oof"))
    val v3: MA[String, Int] = 5.value <#> ((i: Int) => i + 1)("Err")
    
    println(v1)
    println(v2)
    println
   
    implicit val sorter: MessageSorter[String] = _.toIntOption match {
      case None =>
        MT.Error
      case Some(i) =>
        i match {
          case _ if i >= 4 =>
            MT.Error
          case _ if i % 2 == 0 =>
            MT.Warning
          case _ =>
            MT.Info
        }
    }
    
    println(v1.sort)
    println(v2.sort)
    println(v3)
    println(v3.sort)
   
  }
  
}