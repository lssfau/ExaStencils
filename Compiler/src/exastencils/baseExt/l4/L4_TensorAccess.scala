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

package exastencils.baseExt.l4

import scala.math.pow

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_TensorExpression2
import exastencils.baseExt.ir.IR_TensorExpression1
import exastencils.baseExt.ir.IR_TensorExpressionN
import exastencils.logger.Logger
import exastencils.prettyprinting._

case class L4_TensorEntry(var index : List[Int], var coefficient : L4_Expression) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << index.mkString(",") << "]" << ":=" << coefficient
  override def progress = null

  def convertConstants(dt : L4_Datatype) : Unit = (coefficient, dt) match {
    case (c : L4_IntegerConstant, L4_RealDatatype | L4_FloatDatatype | L4_DoubleDatatype) => L4_RealConstant(c.v)
    case (c : L4_RealConstant, L4_IntegerDatatype)                                        => L4_IntegerConstant(c.v.toInt)
    case (_, _)                                                                           => coefficient
  }
}

/// L4_TensorExpression1

case class L4_TensorExpression1(
    var datatype : Option[L4_Datatype],
    var dims : Integer,
    var expressions : List[L4_TensorEntry]) extends L4_Expression {

  def prettyprint(out : PpStream) = {
    out << "tens1" << "{" << dims.toString << ";" <<< (expressions, ", ") << "}"
  }

  override def progress = ProgressLocation(IR_TensorExpression1(
    L4_ProgressOption(datatype)(_.progress),
    dims,
    progressEntrys(dims, expressions)
  ))

  def progressEntrys(dims: Int, input : List[L4_TensorEntry]) : Array[IR_Expression] = {
    val flattenIn = input.toArray
    if (flattenIn.length > dims) {
      Logger.error("To much tensor entries!")
    }
    for (i <- flattenIn.indices) {
      if (flattenIn(i).index.length != 1) Logger.error("Tensor index [" +
        flattenIn(i).index.toString + "] has wrong dimension")
    }
    val eval : Array[Boolean] = Array.fill(dims) { false }
    val exp = new Array[IR_Expression](dims)
    for (i <- flattenIn.indices) {
      if (flattenIn(i).index.head <= exp.length) {
        if (!eval(flattenIn(i).index.head)) {
          eval(flattenIn(i).index.head)  = true
          exp(flattenIn(i).index.head) = flattenIn(i).coefficient.progress
        } else {
          Logger.error("Tensor index [" + flattenIn(i).index.head.toString + "] was double set")
        }
      } else {
        Logger.error("Tensor index [" + flattenIn(i).index.head.toString + "] is not available "+
          (flattenIn(i).index.head.toString + " > " +  exp.length.toString))
      }
    }
    for (i <- 0 until dims) {
      if (!eval(i)) {
        exp(i) = L4_RealConstant(0.0).progress
      }
    }
    exp
  }

  def order = 1
  def isConstant = expressions.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions.foreach(_.convertConstants(dt))
  }
}

/// L4_TensorExpression2

case class L4_TensorExpression2(
    var datatype : Option[L4_Datatype],
    var dims : Integer,
    var expressions : List[L4_TensorEntry]) extends L4_Expression {

  def prettyprint(out : PpStream) = {
    out << "tens2" << "{" << dims.toString << ";" <<< (expressions, ", ") << "}"
  }

  override def progress = ProgressLocation(
    IR_TensorExpression2(L4_ProgressOption(datatype)(_.progress),
      dims,
      progressEntrys(dims, expressions)
    ))

  def progressEntrys(dims : Int, input : List[L4_TensorEntry]) : Array[IR_Expression] = {
    val flattenIn = input.toArray
    if (flattenIn.length > pow(dims, order).toInt) {// TODO: N-Dimensional
      Logger.error("To much tensor entries!")
    }
    for (i <- flattenIn.indices) {
      if (flattenIn(i).index.length != order) Logger.error("Tensor index [" + flattenIn(i).index(0).toString + "," +
        flattenIn(i).index(1).toString + "] has wrong dimension")
    }
    val eval : Array[Boolean] = Array.fill(pow(dims, order).toInt) { false }
    val exp = new Array[IR_Expression](pow(dims, order).toInt)
    for (i <- flattenIn.indices) {
      if ((flattenIn(i).index(0) + flattenIn(i).index(1) * dims) <= exp.length) {
        if (!eval(flattenIn(i).index(0) + flattenIn(i).index(1) * dims)) {
          eval(flattenIn(i).index(0) + flattenIn(i).index(1) * dims) = true
          exp(flattenIn(i).index(0) + flattenIn(i).index(1) * dims) = flattenIn(i).coefficient.progress
        } else {
          Logger.error("Tensor index [" + flattenIn(i).index(0).toString +"," + flattenIn(i).index(1).toString +
            "] was double set")
        }
      } else {
        Logger.error("Tensor index [" + flattenIn(i).index(0).toString +"," + flattenIn(i).index(1).toString +
          "] is not available "+ (flattenIn(i).index(0) + flattenIn(i).index(1) * 3).toString + " > " +
          exp.length.toString)
      }
    }
    for (i <- 0 until pow(dims, order).toInt) {
      if (!eval(i)) {
        exp(i) = L4_RealConstant(0.0).progress
      }
    }
    exp
  }

  def order = 2
  def isConstant = expressions.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions.foreach(_.convertConstants(dt))
  }
}

/// L4_TensorExpressionN

case class L4_TensorExpressionN(
    var datatype : Option[L4_Datatype],
    var dims : Integer,
    var order : Integer,
    var expressions : List[L4_TensorEntry]) extends L4_Expression {

  def prettyprint(out : PpStream) = {
    out << "tensN" << "{" << dims.toString << ";" << order.toString << ";" <<< (expressions, ", ") << "}"
  }

  override def progress = ProgressLocation(
    IR_TensorExpressionN(L4_ProgressOption(datatype)(_.progress),
      dims,
      order,
      progressEntrys(dims, order, expressions)
    ))

  def progressEntrys(dims: Int, order: Int, input : List[L4_TensorEntry]) : Array[IR_Expression] = {
    val flattenIn = input.toArray
    if (flattenIn.length > pow(dims,order.toDouble).toInt) {
      Logger.error("To much tensor entries!")
    }
    for (i <- flattenIn.indices) {
      if (flattenIn(i).index.length != order) {
        var error : String = "Tensor index ["
        error += flattenIn(i).index.foreach(_.toString + ",")
        error = error.substring(0, error.length - 1)
        error += "] has the wrong dimension"
        Logger.error(error)}
    }
    val eval : Array[Boolean] = Array.fill(pow(dims,order.toDouble).toInt) { false }
    val exp = new Array[IR_Expression](pow(dims,order.toDouble).toInt)
    for (i <- flattenIn.indices) {
      var index : Double = 0
      for (j <- 0 until order) {
        index += flattenIn(i).index(j) * pow(dims,j.toDouble)
      }
      if (index <= exp.length) {
        if (!eval(index.toInt)) {
          eval(index.toInt) = true
          exp(index.toInt) = flattenIn(i).coefficient.progress
        } else {
          var error : String = "Tensor index ["
          error += flattenIn(i).index.foreach(_.toString + ",")
          error = error.substring(0, error.length - 1)
          error += "] was double set"
          Logger.error(error)
        }
      } else {
        var error : String = "Tensor index ["
        error += flattenIn(i).index.foreach(_.toString + ",")
        error = error.substring(0, error.length - 1)
        error += "] is not available "+ index.toInt.toString + " > " +  exp.length.toString
        Logger.error(error)
      }
    }
    for (i <- exp.indices) {
      if (!eval(i)) {
        exp(i) = L4_RealConstant(0.0).progress
      }
    }
    exp
  }

  def isConstant = expressions.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions.foreach(_.convertConstants(dt))
  }
}
