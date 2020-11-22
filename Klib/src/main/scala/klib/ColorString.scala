package klib

import scala.annotation.tailrec

import scalaz.NonEmptyList
import scalaz.Scalaz.ToBooleanOpsFromBoolean
import scalaz.Scalaz.ToOptionIdOps
import scalaz.Scalaz.ToOptionOpsFromOption

import klib.ColorString.ansiEscape

sealed trait ColorString {

  def copy(cpF: ColorString.Color => ColorString.Color): ColorString

  // =====| Foreground |=====

  def black: ColorString =
    this.copy(_.copy(fg = Color.Named.Black.some))

  def red: ColorString =
    this.copy(_.copy(fg = Color.Named.Red.some))

  def green: ColorString =
    this.copy(_.copy(fg = Color.Named.Green.some))

  def yellow: ColorString =
    this.copy(_.copy(fg = Color.Named.Yellow.some))

  def blue: ColorString =
    this.copy(_.copy(fg = Color.Named.Blue.some))

  def magenta: ColorString =
    this.copy(_.copy(fg = Color.Named.Magenta.some))

  def cyan: ColorString =
    this.copy(_.copy(fg = Color.Named.Cyan.some))

  def white: ColorString =
    this.copy(_.copy(fg = Color.Named.White.some))

  def rgb(r: Int, g: Int, b: Int): ColorString =
    this.copy(_.copy(fg = Color.RGB(r, g, b).some))

  def dflt: ColorString =
    this.copy(_.copy(fg = Color.Default.some))

  def noFg: ColorString =
    this.copy(_.copy(fg = None))

  // =====| Background |=====

  def blackBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Black.some))

  def redBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Red.some))

  def greenBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Green.some))

  def yellowBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Yellow.some))

  def blueBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Blue.some))

  def magentaBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Magenta.some))

  def cyanBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Cyan.some))

  def whiteBg: ColorString =
    this.copy(_.copy(bg = Color.Named.White.some))

  def rgbBg(r: Int, g: Int, b: Int): ColorString =
    this.copy(_.copy(bg = Color.RGB(r, g, b).some))

  def dfltBg: ColorString =
    this.copy(_.copy(bg = Color.Default.some))

  def noBg: ColorString =
    this.copy(_.copy(bg = None))

  // =====| ... |=====

  def toColorString: ColorString =
    this

  def split(splitStr: String): List[ColorString] =
    this match {
      case ColorString.Simple(color, str) =>
        str.split(splitStr).map(ColorString.Simple(color, _)).toList
      case ColorString.Complex(color, pairs, tail) =>
        List(
          pairs.flatMap {
            case (oStr, cStr) =>
              List(
                oStr.toList.flatMap {
                  _.split(splitStr).map(ColorString.Simple(color, _))
                },
                cStr.split(splitStr),
              ).flatten
          },
          tail.toList.flatMap {
            _.split(splitStr).map(ColorString.Simple(color, _))
          },
        ).flatten
    }

  override def toString: String = {
    val stringBuilder: StringBuilder = new StringBuilder

    def append(
        cs: ColorString.ColorState,
        c: ColorString.Color,
        str: String,
    ): ColorString.ColorState =
      c.diffWithState(cs) match {
        case Some((ansi, ncs)) =>
          stringBuilder.append(ansi).append(str)
          ncs
        case None =>
          stringBuilder.append(str)
          cs
      }

    def rec(
        colorString: ColorString,
        colorState: ColorString.ColorState,
    ): ColorString.ColorState =
      colorString match {
        case ColorString.Simple(color, str) =>
          append(
            colorState,
            color,
            str,
          )
        case ColorString.Complex(color, pairs, tail) =>
          val afterPairs =
            pairs.foldLeft(colorState) {
              case (ccs, (oStr, cStr)) =>
                val afterOStr =
                  oStr.map(append(ccs, color, _)).getOrElse(ccs)
                val afterCStr =
                  rec(
                    cStr,
                    afterOStr,
                  )

                afterCStr
            }
          val afterTail =
            tail.map(append(afterPairs, color, _)).getOrElse(afterPairs)

          afterTail
      }

    val finalColorState = rec(
      this,
      ColorString.ColorState.Default,
    )
    ColorString.Color.Default.diffWithState(finalColorState).foreach {
      case (ansi, _) =>
        stringBuilder.append(ansi)
    }
    stringBuilder.toString
  }

}

object ColorString {
  import klib.{Color => RawColor}

  private def ansiEscape(codes: NonEmptyList[String]): String =
    s"\u001b[${codes.list.toList.mkString(";")}m"

  final case class Color(
      fg: Option[RawColor],
      bg: Option[RawColor],
  ) {

    // Does a diff, to make sure any coloring is needed
    def diffWithState(colorState: ColorState): Option[(String, ColorState)] =
      (fg.filterNot(_ == colorState.fg), bg.filterNot(_ == colorState.bg)) match {
        case (Some(fg), Some(bg)) =>
          (
            ansiEscape(NonEmptyList(fg.fgMod, bg.bgMod)),
            colorState.copy(fg = fg, bg = bg),
          ).some
        case (Some(fg), None) =>
          (
            ansiEscape(NonEmptyList(fg.fgMod)),
            colorState.copy(fg = fg),
          ).some
        case (None, Some(bg)) =>
          (
            ansiEscape(NonEmptyList(bg.bgMod)),
            colorState.copy(bg = bg),
          ).some
        case (None, None) =>
          None
      }

  }
  object Color {
    val Empty: Color = Color(fg = None, bg = None)
    val Default: Color = Color(fg = RawColor.Default.some, bg = RawColor.Default.some)
  }

  private[ColorString] final case class ColorState(
      fg: RawColor,
      bg: RawColor,
  ) {

    def toColor: Color = Color(fg = fg.some, bg = bg.some)

  }
  private[ColorString] object ColorState {
    val Default: ColorState = ColorState(RawColor.Default, RawColor.Default)
  }

  // =====| ... |=====

  final case class Simple private[ColorString] (
      color: Color,
      str: String,
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Simple(
        cpF(color),
        str,
      )

  }

  // color"someString${cString}${cString}someString${cString}someString"
  final case class Complex private[ColorString] (
      color: Color,
      pairs: List[(Option[String], ColorString)],
      tail: Option[String],
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Complex(
        cpF(color),
        pairs,
        tail,
      )

  }

  object syntax {

    implicit class ToColorStringOps(obj: Any) {

      def toColorString: ColorString =
        Simple(Color.Empty, obj.toString)

    }

    implicit class ColorStringInterpolator(sc: StringContext) {

      def color(args: ColorString*): ColorString = {
        // TODO (KR) :
        ???
      }

    }

    implicit class ColorStringListOps(csl: List[ColorString]) {

      def csMkString: ColorString =
        csMkString("")

      def csMkString(joinStr: String, color: Color = Color.Empty): ColorString = {
        val js = joinStr.nonEmpty ? joinStr.some | None

        val pairs: List[(Option[String], ColorString)] = {
          @tailrec
          def loop(
              queue: List[ColorString],
              stack: List[(Option[String], ColorString)],
          ): List[(Option[String], ColorString)] =
            queue match {
              case Nil =>
                stack.reverse
              case head :: tail =>
                loop(
                  tail,
                  (js, head) :: stack,
                )
            }

          csl match {
            case Nil =>
              Nil
            case head :: tail =>
              loop(tail, (None, head) :: Nil)
          }
        }

        ColorString.Complex(color, pairs, None)
      }

    }

    /*
      TODO (KR) : Make sure the scoping on this behaves how I would hope
                :
                : // Without auto
                : import klib.ColorString.syntax._
                : val tmp1: ColorString = obj.toColorString // yes
                : val tmp2: ColorString = obj               // no
                :
                : // With auto
                : import klib.ColorString.syntax._
                : import klib.ColorString.syntax.auto._
                : val tmp3: ColorString = obj.toColorString // yes
                : val tmp4: ColorString = obj               // yes
     */
    object auto {

      implicit def toSimpleColorString(obj: Any): ColorString =
        obj.toColorString

    }

  }

}
