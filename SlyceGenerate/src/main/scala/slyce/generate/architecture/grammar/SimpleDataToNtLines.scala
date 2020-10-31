package slyce.generate.architecture.grammar

import slyce.common.architecture.Stage

trait SimpleDataToNtLines[SimpleData] extends Stage[(SimpleData, String), List[String], List[String]]
