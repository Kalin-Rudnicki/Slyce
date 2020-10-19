package slyce.architecture.generation.grammar

import slyce.architecture.Stage

trait DataToSimpleData[Data, Err, SimpleData] extends Stage[Data, Err, SimpleData]
