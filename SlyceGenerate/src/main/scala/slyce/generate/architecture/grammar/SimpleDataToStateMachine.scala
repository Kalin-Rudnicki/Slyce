package slyce.generate.architecture.grammar

import slyce.common.architecture.Stage

trait SimpleDataToStateMachine[SimpleData, StateMachine] extends Stage[SimpleData, List[String], StateMachine]
