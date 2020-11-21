package slyce.generate.architecture

import slyce.common.architecture.Stage

trait Formatter[Dfa, SimpleData, StateMachine, Err]
    extends Stage[(Dfa, SimpleData, StateMachine), Err, Formatter.Settings => String]
object Formatter {

  final case class Settings(
      packageName: List[String],
      className: String,
      indent: String,
  )

}
