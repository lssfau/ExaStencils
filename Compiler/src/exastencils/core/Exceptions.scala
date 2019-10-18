//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.core

import exastencils.datastructures.Transformation

/**
  * Exception that is thrown in the context of [[exastencils.datastructures.Transformation]].
  *
  * @param msg            A message describing the error.
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
  * @param msg   A message describing the error.
  * @param inner The inner exception leading to the CheckpointException, or None.
  */
case class CheckpointException(msg : String, inner : Option[Throwable] = None) extends RuntimeException(msg)
