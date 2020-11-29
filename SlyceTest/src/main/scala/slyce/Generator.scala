package slyce

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

import scalaz.-\/
import scalaz.\/-

import slyce.generate.Formatter
import slyce.generate.{lexer => lex}
import slyce.generate.{grammar => gram}

object Generator {

  final case class Settings(
      srcRoot: String = "SlyceTest/src/test/scala",
      subPkg: String,
  )

  def generate(
      lexerData: lex.Data,
      grammarData: gram.Data,
      settings: Settings,
  ): Unit = {
    import klib.ColorString.syntax._
    import auto._
    import klib.Idt._
    import klib.Logger.GlobalLogger
    import klib.Logger.{LogLevel => LL}

    GlobalLogger.sources.stdOut(LL.Debug)
    GlobalLogger.flags.add("Generator")
    // GlobalLogger.flags.add("DataToNfa")
    // GlobalLogger.flags.add("NfaToDfa")

    implicit val flags: Set[String] = Set("Generator")

    val formatter = for {
      dfa <- lex.Lexer(lexerData)
      simpleData <- gram.DataToSimpleData(grammarData)
      stateMachine <- gram.SimpleDataToStateMachine(simpleData)
      // Format
      fmtr <- Formatter(dfa, simpleData, stateMachine)
    } yield fmtr

    GlobalLogger.break
    GlobalLogger.debug("=====| Generator |=====")
    formatter match {
      case -\/(errs) =>
        GlobalLogger.fatal("Failure")
        errs.foreach(GlobalLogger.error(_))
        System.exit(1)
      case \/-(formatter) =>
        import slyce.generate.architecture.{Formatter => ArchFormatter}

        val fmtSettings = ArchFormatter.Settings(
          packageName = List("slyce", "tests", settings.subPkg),
          className = "Parser",
          indent = "  ",
        )

        val file = new File(s"${settings.srcRoot}/${fmtSettings.packageName.mkString("/")}/${fmtSettings.className}.scala")

        GlobalLogger.important("Success!")
        val writer = new BufferedWriter(new FileWriter(file))
        val src = formatter(fmtSettings)
        GlobalLogger.debug(src, Set("Generator:src"))
        writer.write(src)
        writer.close
    }

  }

}
