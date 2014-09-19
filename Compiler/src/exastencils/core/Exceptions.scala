package exastencils.core

import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation

/**
 * Exception that is thrown in the context of [[exastencils.datastructures.Transformation]].
 * 
 * @param msg A message describing the error.
 * @param transformation The [[exastencils.datastructures.Transformation]] causing the exception.
 */
case class TransformationException(msg : String, transformation : Transformation) extends RuntimeException(msg)

/**
 * Exception that is thrown in the context of Transactions.
 * 
 * @param msg A message describing the error.
 */
case class TransactionException(msg : String) extends RuntimeException(msg)

/**
 * Exception that is thrown in the context of Checkpoints.
 * 
 * @param msg A message describing the error.
 * @param inner The inner exception leading to the CheckpointException, or None.
 */
case class CheckpointException(msg : String, inner : Option[Throwable] = None) extends RuntimeException(msg)
