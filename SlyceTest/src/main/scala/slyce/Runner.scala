package slyce

import scala.io.Source

import scalaz.-\/
import scalaz.\/-

import slyce.parse.architecture.Parser

object Runner {

  def run[RawTree](parser: Parser[String, List[String], RawTree], srcFile: String)(f: RawTree => Unit): Unit = {
    import klib.ColorString.syntax._
    import auto._
    import klib.Idt._
    import klib.Logger.GlobalLogger
    import klib.Logger.{LogLevel => LL}

    GlobalLogger.sources.stdOut(LL.Debug)
    GlobalLogger.flags.add("Runner")
    // GlobalLogger.flags.add("Parser")
    // GlobalLogger.flags.add("StateMachine.loop")

    implicit val flags: Set[String] = Set("Runner")

    val source = Source.fromFile(srcFile)
    val str = source.mkString
    source.close

    val res = parser(str)

    GlobalLogger.break
    GlobalLogger.debug("=====| Runner |=====")
    res match {
      case -\/(errs) =>
        GlobalLogger.fatal("Failure")
        errs.foreach(GlobalLogger.error(_))
      case \/-(rawTree) =>
        f(rawTree)
    }

  }

}
