package exastencils.core

import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation

case class TransformationException(msg : String, transformation : Transformation) extends RuntimeException(msg)

case class TransactionException(msg : String) extends RuntimeException(msg)
case class CheckpointException(msg : String, inner : Option[Throwable] = None) extends RuntimeException(msg)
