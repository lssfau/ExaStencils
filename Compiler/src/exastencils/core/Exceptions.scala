package exastencils.core

import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation

case class StrategyException(msg : String, stragety : Strategy) extends RuntimeException(msg)
case class TransformationException(msg : String, transformation : Transformation) extends RuntimeException(msg)
case class ValueSetException(msg : String) extends RuntimeException(msg)
