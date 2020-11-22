package klib

import java.io.PrintStream

import scalaz.Scalaz.ToOptionIdOps

import klib.ColorString.{Color => ColorPair}
import klib.ColorString.syntax._

class Logger private (
    initialSources: Map[PrintStream, Logger.LogLevel],
    initialFlags: Set[String],
) {

  private var _sources: Map[PrintStream, Logger.Source] =
    initialSources.map {
      case k -> v =>
        k ->
          new Logger.Source(
            printStream = k,
            logTolerance = v,
          )
    }
  private var _flags: Set[String] = initialFlags

  @inline
  private def nonEmptySet[E](e0: E, eN: Seq[E]): Set[E] =
    eN.toSet + e0

  // =====| Sources |=====

  object sources {

    def setToleranceFor(logTolerance: Logger.LogTolerance, src0: PrintStream, srcN: PrintStream*): Unit =
      _sources = nonEmptySet(src0, srcN).foldLeft(_sources) {
        case (srcs, ps) =>
          srcs.updatedWith(ps) {
            case None =>
              new Logger.Source(
                printStream = ps,
                logTolerance = logTolerance,
              ).some
            case Some(src) =>
              src.setLogTolerance(logTolerance).some
          }
      }

    def remove(src0: PrintStream, srcN: PrintStream*): Unit = {
      val srcs: Set[PrintStream] = nonEmptySet(src0, srcN)

      _sources = _sources.filterNot {
        case k -> _ =>
          srcs.contains(k)
      }
    }

  }

  // =====| Flags |=====

  object flags {

    def add(flag0: String, flagN: String*): Unit =
      _flags = _flags | nonEmptySet(flag0, flagN)

    def remove(flag0: String, flagN: String*): Unit =
      _flags = _flags &~ nonEmptySet(flag0, flagN)

  }

  // =====| Log Events |=====

  object events {

    def run(events: List[Logger.LogEvent]): Unit =
      events.foreach { event =>
        _sources.values.foreach {
          _.attemptExecuteEvent(event, _flags)
        }
      }

  }

  // =====|  |=====

}
object Logger {

  // =====| ... |=====

  def apply(
      stdOutTolerance: Option[LogLevel] = LogLevel.Default.some,
      additionalSources: Map[PrintStream, LogLevel] = Map.empty,
      flags: Set[String] = Set.empty,
  ): Logger =
    new Logger(
      initialSources = stdOutTolerance match {
        case None =>
          additionalSources
        case Some(logTol) =>
          additionalSources + (System.out -> logTol)
      },
      initialFlags = flags,
    )

  // =====| ... |=====

  final class Source private[Logger] (
      private val printStream: PrintStream,
      private var logTolerance: LogTolerance,
  ) {

    private var breakState: Source.BreakState = Source.BreakState.None

    def setLogTolerance(logTolerance: LogTolerance): this.type = {
      this.logTolerance = logTolerance
      this
    }

    // TODO (KR) : Optimize to allow for bulk execution
    def attemptExecuteEvent(
        logEvent: LogEvent,
        loggerFlags: Set[String],
    ): Unit =
      logEvent match {
        case LogEvent.Log(level, content, flags) =>
          if (level.exceedsTolerance(logTolerance) && flags.forall(loggerFlags.contains)) {
            import auto._
            val marginSpace = "  " // TODO (KR) : Move elsewhere?
            val displayName = level.displayName.padTo(LogLevel.MaxDisplayLength, ' ')
            val label = color"[$displayName]".overwrite(level.labelColor)
            val idt = s"${" " * label.length}|"
            val split = content.split("\n").map(_.underwrite(level.messageColor))
            val tryGrabFirst =
              split match {
                case Nil =>
                  label :: Nil
                case head :: tail =>
                  label + marginSpace + head :: tail
              }
            val joined = tryGrabFirst.csMkString(idt + marginSpace, level.labelColor)

            breakState match {
              case Source.BreakState.BreakOnLog =>
                // TODO (KR) : Should this be displayed differently?
                printStream.append("\n")
                breakState = Source.BreakState.None
              case Source.BreakState.None => // Do nothing
            }
            printStream.println(joined.toString)
          }
        case LogEvent.Break =>
          breakState = Source.BreakState.BreakOnLog
      }

  }
  object Source {

    sealed trait BreakState
    object BreakState {
      case object BreakOnLog extends BreakState
      case object None extends BreakState
    }

  }

  sealed trait LogEvent
  object LogEvent {

    final case class Log(
        level: LogLevel,
        content: ColorString,
        flags: Set[String],
    ) extends LogEvent

    // TODO (KR) : Break types
    case object Break extends LogEvent

  }

  // =====| LogLevel |=====

  sealed abstract class LogLevel(
      val priority: Int,
      val name: String,
      val displayName: String,
      val labelColor: ColorPair,
      val messageColor: ColorPair,
  ) {

    def exceedsTolerance(logTolerance: LogLevel): Boolean =
      this.priority >= logTolerance.priority

  }
  type LogTolerance = LogLevel

  object LogLevel {

    private[Logger] val Default: LogLevel = Print

    case object Never
        extends LogLevel(
          priority = 0,
          name = "Never",
          displayName = "NEVER",
          labelColor = ColorPair(
            fg = Color.Named.Black.some,
            bg = Color.Named.White.some,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Debug
        extends LogLevel(
          priority = 1,
          name = "Debug",
          displayName = "DEBUG",
          labelColor = ColorPair(
            fg = Color(0x00, 0xb0, 0xff).some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Detailed
        extends LogLevel(
          priority = 2,
          name = "DETAILED",
          displayName = "DETLD",
          labelColor = ColorPair(
            fg = Color(0x00, 0xbf, 0xa5).some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Info
        extends LogLevel(
          priority = 3,
          name = "Info",
          displayName = "INFO",
          labelColor = ColorPair(
            fg = Color(0x64, 0xdd, 0x17).some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Print
        extends LogLevel(
          priority = 4,
          name = "Print",
          displayName = "PRINT",
          labelColor = ColorPair(
            fg = None,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Important
        extends LogLevel(
          priority = 5,
          name = "Important",
          displayName = "IMPRT",
          labelColor = ColorPair(
            fg = Color.RGB(0xff, 0xd6, 0x00).some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Warning
        extends LogLevel(
          priority = 6,
          name = "Warning",
          displayName = "WARN",
          labelColor = ColorPair(
            fg = Color(0xff, 0x91, 0x00).some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Error
        extends LogLevel(
          priority = 7,
          name = "Error",
          displayName = "ERROR",
          labelColor = ColorPair(
            fg = Color(0xdd, 0x2c, 0x00).some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Fatal
        extends LogLevel(
          priority = 8,
          name = "Fatal",
          displayName = "FATAL",
          labelColor = ColorPair(
            fg = Color.Named.Red.some,
            bg = None,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    case object Always
        extends LogLevel(
          priority = 9,
          name = "Always",
          displayName = "ALWYS",
          labelColor = ColorPair(
            fg = Color.Named.White.some,
            bg = Color.Named.Black.some,
          ),
          messageColor = ColorPair(
            fg = None,
            bg = None,
          ),
        )

    private[Logger] val MaxDisplayLength =
      List(
        Never,
        Debug,
        Detailed,
        Info,
        Print,
        Important,
        Warning,
        Error,
        Fatal,
        Always,
      ).maxBy(_.displayName.length).displayName.length

  }

}
