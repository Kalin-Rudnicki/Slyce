package slyce.generate.architecture.grammar

import slyce.common.architecture.Stage
import slyce.common.helpers.Idt

trait SimpleDataToNtLines[SimpleData] extends Stage[SimpleData, List[String], Idt]
