package slyce.generate.architecture.formatting

import slyce.common.architecture.Stage
import slyce.common.helpers.Idt

trait TokenLines[Dfa, SimpleData] extends Stage[(Dfa, SimpleData), List[String], Idt]
