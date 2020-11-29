package slyce.metaSelf

import slyce.Generator
import slyce.generate.architecture.Formatter

object Generate extends App {

  // Grammar
  Generator.generate(
    GrammarData.lexerData,
    GrammarData.grammarData,
    Generator.Settings(
      srcRoot = "SlyceTest/src/main/scala",
      fmt = Formatter.Settings(
        packageName = List("slyce", "metaSelf"),
        className = "GrammarParser",
        indent = "  ",
      ),
    ),
  )

}
