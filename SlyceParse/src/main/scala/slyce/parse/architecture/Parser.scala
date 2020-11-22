package slyce.parse.architecture

import slyce.common.architecture.Stage

trait Parser[Src, Errs, RawTree] extends Stage[Src, Errs, RawTree]
object Parser {

  def apply[Errs, Src, Tok, RawTree](
      lexer: Lexer[Src, Errs, Tok],
      grammar: Grammar[Tok, Errs, RawTree],
  ): Parser[Src, Errs, RawTree] =
    (lexer.onO { toks =>
      {
        // DEBUG : (Start) ==================================================
        import klib.ColorString.syntax._
        import auto._
        import klib.Idt._
        import klib.Logger.GlobalLogger

        implicit val flags: Set[String] = Set("Parser")

        GlobalLogger.break
        GlobalLogger.debug("=====| Parser |=====")
        toks.foreach(GlobalLogger.debug(_))

        // DEBUG : (End) ==================================================
      }

    } >+> grammar)(_)

}
