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

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.logger.Logger

/* ###################################################################################################################
################                 Implementation of tensor assignments                       ##########################
################################################################################################################### */

/** Objects transforms tensor assignments */
object IR_ResolveTensorAssignments extends DefaultStrategy("Resolve assignments tensors") {

  this += new Transformation("scalarize 1/2", {

    // Tensor 1 Assignment
    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype1] &&
        src.isInstanceOf[IR_TensorExpression1]                                                        =>
      val tmp = src.asInstanceOf[IR_TensorExpression1]
      if (tmp.dims != dest.datatype.asInstanceOf[IR_TensorDatatype1].dims) {
        Logger.error("Tensor1 assignment: source and destination has different dimensionality")
      }
      var newStmts = ListBuffer[IR_Statement]()
      for (x <- 0 until tmp.dims) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(x)), tmp.get(x))
      }
      newStmts

    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype1] &&
        !src.isInstanceOf[IR_TensorExpression1] && src.datatype.isInstanceOf[IR_TensorDatatype1]      =>
      var newStmts = ListBuffer[IR_Statement]()
      val tmp = src.datatype.asInstanceOf[IR_TensorDatatype1]
      if (tmp.dims != dest.datatype.asInstanceOf[IR_TensorDatatype1].dims) {
        Logger.error("Tensor1 assignment: source and destination has different dimensionality")
      }
      for (i <- 0 until tmp.dims) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i)),
          IR_HighDimAccess(src, IR_ExpressionIndex(i)))
      }
      newStmts

    // Tensor 2 Assignment
    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype2] &&
        src.isInstanceOf[IR_TensorExpression2]                                                        =>
      val tmp = src.asInstanceOf[IR_TensorExpression2]
      if (tmp.dims != dest.datatype.asInstanceOf[IR_TensorDatatype2].dims) {
        Logger.error("Tensor2 assignment: source and destination has different dimensionality")
      }
      var newStmts = ListBuffer[IR_Statement]()
      for (y <- 0 until tmp.dims) {
        for (x <- 0 until tmp.dims) {
          newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(x + y *3)), tmp.get(x, y))
        }
      }
      newStmts

    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype2] &&
        !src.isInstanceOf[IR_TensorExpression2] && src.datatype.isInstanceOf[IR_TensorDatatype2]      =>
      var newStmts = ListBuffer[IR_Statement]()
      val tmp = src.datatype.asInstanceOf[IR_TensorDatatype2]
      if (tmp.dims != dest.datatype.asInstanceOf[IR_TensorDatatype2].dims) {
        Logger.error("Tensor2 assignment: source and destination has different dimensionality")
      }
      for (i <- 0 until tmp.dims*tmp.dims) {
          newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i)),
            IR_HighDimAccess(src, IR_ExpressionIndex(i)))
      }
      newStmts

    // Tensor N Assignment
    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatypeN] &&
        src.isInstanceOf[IR_TensorExpressionN]                                                        =>
      val tmp_src = src.asInstanceOf[IR_TensorExpressionN]
      val tmp_dest = dest.asInstanceOf[IR_TensorDatatypeN]
      if (tmp_src.dims != tmp_dest.dims) {
        Logger.error("TensorN assignment: source and destination has different dimensionality")
      }
      if (tmp_src.order != tmp_dest.order) {
        Logger.error("TensorN assignment: source and destination has different order")
      }
      var newStmts = ListBuffer[IR_Statement]()
      for (x <- tmp_src.expressions.indices) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(x)), tmp_src.getDirect(x))
      }
      newStmts

    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatypeN] &&
        !src.isInstanceOf[IR_TensorExpressionN] && src.datatype.isInstanceOf[IR_TensorDatatypeN]      =>
      val tmp_src = src.asInstanceOf[IR_TensorExpressionN]
      val tmp_dest = dest.asInstanceOf[IR_TensorDatatypeN]
      if (tmp_src.dims != tmp_dest.dims) {
        Logger.error("TensorN assignment: source and destination has different dimensionality")
      }
      if (tmp_src.order != tmp_dest.order) {
        Logger.error("TensorN assignment: source and destination has different order")
      }
      var newStmts = ListBuffer[IR_Statement]()
      for (i <- 0 until scala.math.pow(tmp_src.dims.toDouble, tmp_src.order.toDouble).toInt) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i)),
          IR_HighDimAccess(src, IR_ExpressionIndex(i)))
      }
      newStmts

  })
}