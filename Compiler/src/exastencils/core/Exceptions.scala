package exastencils.core

import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation

case class StrategyException(msg : String, strategy : Strategy) extends RuntimeException(msg)
case class TransformationException(msg : String, transformation : Transformation) extends RuntimeException(msg)
case class ValueSetException(msg : String) extends RuntimeException(msg)

case class TransactionException(msg : String) extends RuntimeException(msg)
case class CheckpointException(msg : String) extends RuntimeException(msg)
