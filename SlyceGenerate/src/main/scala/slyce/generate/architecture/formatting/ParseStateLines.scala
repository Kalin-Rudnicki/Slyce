package slyce.generate.architecture.formatting

import slyce.common.architecture.Stage
import slyce.common.helpers.Idt

trait ParseStateLines[StateMachine] extends Stage[StateMachine, List[String], Idt]
