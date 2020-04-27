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
import scala.math.pow

import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir._
import exastencils.core._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.ir._

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackTenComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype : IR_Datatype = mat.datatype
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

/* ###################################################################################################################
################                     abstract IR_TensorExpresion                            ##########################
################################################################################################################### */

/** abstract tensor expression type
 *
 * @param innerDatatype : IR_Datatype, should be IR numeric datatype
 */
abstract class IR_TensorExpression(innerDatatype : Option[IR_Datatype]) extends IR_Expression {
  var expressions : Array[IR_Expression]
  val order : Integer

  override def datatype: IR_Datatype

  def prettyprintInner(out : PpStream) : Unit
  override def prettyprint(out : PpStream) : Unit

  def isConstant : Boolean = expressions.forall(e => e.isInstanceOf[IR_Number])
  def isInteger : Boolean = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  def isReal : Boolean = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def toString : String
}

/* ###################################################################################################################
################                        IR_TensorExpression1                                ##########################
################################################################################################################### */

/** Factory for IR_TensorExpression1 objects */
object IR_TensorExpression1 {
  /** creates a empty first order tensor expression
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @return IR_TensorExpression1 instance
   */
  def apply(innerDatatype : IR_Datatype) : IR_TensorExpression1 = new IR_TensorExpression1(Some(innerDatatype))

  /** creates a first order tensor expression and fill with given array
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param expressions : Array[IR_Expression], input array with length 3
   * @return IR_TensorExpression1 instance
   */
  def apply(innerDatatype : Option[IR_Datatype], expressions : Array[IR_Expression]) : IR_TensorExpression1 = {
    if (expressions.length != 3) {
      Logger.error("expressions has the wrong length")
    }
    val tmp = new IR_TensorExpression1(innerDatatype)
    tmp.expressions = expressions
    tmp
  }

  /** creates a first order tensor expression and fill with list buffer entries
   *
   * @param datatype : IR_Datatype, should be IR numeric datatype
   * @param expressions : ListBuffer[IR_Expression], input list buffer with length 3
   * @return IR_TensorExpression1 instance
   */
  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_TensorExpression1 = {
    if (expressions.toArray.length != 3) {
      Logger.error("expressions has a wrong count of entries")
    } else {
      val tmp = IR_TensorExpression1(datatype.datatype)
      tmp.expressions = expressions.toArray
      tmp
    }
  }

  /** creates a first order tensor expression and fill with single number
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param num : IR_Number, number to fill in tensor
   * @return IR_TensorExpression1 instance
   */
  def fromSingleExpression(innerDatatype : IR_Datatype, num : IR_Number) : IR_TensorExpression1 = {
    val tmp = new IR_TensorExpression1(Some(innerDatatype))
    for (i <- 0 until 3)
      tmp.expressions(i) = Duplicate(num)
    tmp
  }
}

/** Expression of a first order tensor
 *
 * @param innerDatatype : Option[IR_Datatype], Datatype of the saved expression
 */
case class IR_TensorExpression1(var innerDatatype : Option[IR_Datatype]) extends IR_TensorExpression(innerDatatype) {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](3)
  val order : Integer = 1

  override def datatype : IR_TensorDatatype1 = {
    innerDatatype match {
      case None                         =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt : IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _                            =>
    }
    IR_TensorDatatype1(innerDatatype.getOrElse(IR_RealDatatype))
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "__tensor1_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out  << "_t "
    prettyprintInner(out)
  }

  override def isConstant : Boolean = expressions.forall(e => e.isInstanceOf[IR_Number])
  override def isInteger : Boolean = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  override def isReal : Boolean = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def set(x : Integer, num : IR_Expression) : Unit = {
    if (x < 0 || x > 2) {
      Logger.error("get got an index out of the allowed range (0, 2)")
    }
    expressions(x) = num
  }
  def get(x :Integer) : IR_Expression = {
    if (x < 0 || x > 2) {
      Logger.error("set got an index out of the allowed range (0, 2)")
    }
    expressions(x)
  }
  override def toString : String = { "IR_TensorExpression1(" + innerDatatype + "," + 1 + "; Items: "+
    expressions.mkString(", ") + ")" }

}

/* ###################################################################################################################
################                        IR_TensorExpression2                                ##########################
################################################################################################################### */


/** Factory for IR_TensorExpression2 objects */
object IR_TensorExpression2 {
  /** creates a empty second order tensor expression
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @return IR_TensorExpression2 instance
   */
  def apply(innerDatatype : IR_Datatype) : IR_TensorExpression2 = new IR_TensorExpression2(Some(innerDatatype))

  /** creates a second order tensor expression and fill with given array
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param expressions : Array[IR_Expression], input array with length 9
   * @return IR_TensorExpression2 instance
   */
  def apply(innerDatatype : Option[IR_Datatype], expressions : Array[IR_Expression]) : IR_TensorExpression2 = {
    if (expressions.length != 9) {
      Logger.error("expressions has the wrong length")
    }
    val tmp = new IR_TensorExpression2(innerDatatype)
    tmp.expressions = expressions
    tmp
  }

  /** creates a second order tensor expression and fill with given matrix
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param expressions : ListBuffer[ListBuffer[IR_Number]], input 3x3 matrix filled with numbers
   * @return IR_TensorExpression2 instance
   */
  def apply(innerDatatype : Option[IR_Datatype], expressions : ListBuffer[ListBuffer[IR_Number]]) : IR_TensorExpression2 = {
    if ((expressions.toArray.length != 3) || (expressions.head.toArray.length !=3)) {
      Logger.error("matrix has the wrong dimension")
    }
    val tmp = new IR_TensorExpression2(innerDatatype)
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, expressions(x)(y))
      }
    }
    tmp
  }

  /** creates a second order tensor expression and fill with given linearized matrix
   *
   * @param datatype : IR_MatrixDatatype, should be IR numeric datatype
   * @param expressions : Array[IR_Expression], input array with length 9
   * @return IR_TensorExpression2 instance
   */
  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_TensorExpression2 = {
    if (expressions.toArray.length != 9) {
      Logger.error("expressions has a wrong count of entries")
    } else {
      val tmp = IR_TensorExpression2(datatype.datatype)
      tmp.expressions = expressions.toArray
      tmp
    }
  }

  /** creates a 2D tensor expression and fill with single number
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param num : IR_Number, number to fill in tensor
   * @return IR_TensorExpression2 instance
   */
  def fromSingleExpression(innerDatatype : IR_Datatype, num : IR_Number) : IR_TensorExpression2 = {
    val tmp = new IR_TensorExpression2(Some(innerDatatype))
    for (i <- 0 until 9)
      tmp.expressions(i) = Duplicate(num)
    tmp
  }
}

/** Expression of a second order tensor
 *
 * @param innerDatatype : Option[IR_Datatype], Datatype of the saved expression
 */
case class IR_TensorExpression2(var innerDatatype : Option[IR_Datatype]) extends IR_TensorExpression(innerDatatype) {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](9)
  val order : Integer = 2

  override def datatype : IR_TensorDatatype2 = {
    innerDatatype match {
      case None                         =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt : IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _                            =>
    }
    IR_TensorDatatype2(innerDatatype.getOrElse(IR_RealDatatype))
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "__tensor2_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out  << "_t "
    prettyprintInner(out)
  }

  override def isConstant : Boolean = expressions.forall(e => e.isInstanceOf[IR_Number])
  override def isInteger : Boolean = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  override def isReal : Boolean = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(x : Integer, y : Integer) : IR_Expression = {
    if (x < 0 || x > 2 || y < 0 || y > 2) {
      Logger.error("get got an index out of the allowed range (0, 2)")
    }
    expressions(y * 3 + x)
  }
  def set(x : Integer, y: Integer, num : IR_Expression) : Unit = {
    if (x < 0 || x > 2 || y < 0 || y > 2) {
      Logger.error("set got an index out of the allowed range (0, 2)")
    }
    expressions(y * 3 + x) = num
  }
  override def toString : String = { "IR_TensorExpression2(" + innerDatatype + "," + 2 + "; Items: " +
    expressions.mkString(", ") + ")" }
}

/* ###################################################################################################################
################                        IR_TensorExpressionN                                ##########################
################################################################################################################### */

/** Factory for IR_TensorExpressionN objects */
object IR_TensorExpressionN {
  /** creates a empty tensor with n-th order
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param order : Integer, defines the order of the tensor
   * @return IR_TensorExpression2 instance
   */
  def apply(innerDatatype : IR_Datatype, order : Integer) : IR_TensorExpressionN = new IR_TensorExpressionN(Some(innerDatatype), order)

  /** creates a n-th order tensor and fill with given array
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param order : Integer, defines the order of the tensor
   * @param expressions : Array[IR_Expression], input array with length 3^order
   * @return IR_TensorExpression2 instance
   */
  def apply(innerDatatype : Option[IR_Datatype], order : Integer, expressions : Array[IR_Expression]) : IR_TensorExpressionN = {
    if (expressions.length != pow(3, order.toDouble).toInt) {
      Logger.error("expressions has the wrong length")
    }
    val tmp = new IR_TensorExpressionN(innerDatatype, order)
    tmp.expressions = expressions
    tmp
  }

  /** creates a n-th order tensor and fill with given linearized matrix
   *
   * @param datatype : IR_MatrixDatatype, should be IR numeric datatype
   * @param order : Integer, defines the order of the tensor
   * @param expressions : Array[IR_Expression], input array with length 3^order
   * @return IR_TensorExpression2 instance
   */
  def apply(datatype : IR_MatrixDatatype, order : Integer,  expressions : ListBuffer[IR_Expression]) : IR_TensorExpressionN = {
    if (expressions.toArray.length != pow(3, order.toDouble).toInt) {
      Logger.error("expressions has a wrong count of entries")
    } else {
      val tmp = IR_TensorExpressionN(datatype.datatype, order)
      tmp.expressions = expressions.toArray
      tmp
    }
  }

  /** creates a n-th order tensor and fill with single number
   *
   * @param innerDatatype : IR_Datatype, should be IR numeric datatype
   * @param num : IR_Number, number to fill in tensor
   * @return IR_TensorExpression2 instance
   */
  def fromSingleExpression(innerDatatype : IR_Datatype, order: Integer, num : IR_Number) : IR_TensorExpressionN = {
    val tmp = new IR_TensorExpressionN(Some(innerDatatype), order)
    for (i <- tmp.expressions.indices)
      tmp.expressions(i) = Duplicate(num)
    tmp
  }
}

/** Expression of a first order tensor
 *
 * @param innerDatatype : Option[IR_Datatype], Datatype of the saved expression
 * @param ord : Integer, represent the order of the tensor
 */
case class IR_TensorExpressionN(var innerDatatype : Option[IR_Datatype], var ord : Integer) extends IR_TensorExpression(innerDatatype) {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](pow(3, ord.toDouble).toInt)
  val order : Integer = ord

  override def datatype : IR_TensorDatatypeN = {
    innerDatatype match {
      case None                         =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt : IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _                            =>
    }
    IR_TensorDatatypeN(innerDatatype.getOrElse(IR_RealDatatype), ord)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "__tensorN" + order.toString + "_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out  << "_t "
    prettyprintInner(out)
  }

  override def isConstant : Boolean = expressions.forall(e => e.isInstanceOf[IR_Number])
  override def isInteger : Boolean = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  override def isReal : Boolean = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(k : List[Integer]) : IR_Expression = {
    if (k.length != order) {
      Logger.error("get needs a list of integer with length of order")
    }
    var index : Double = 0
    for (i <- 0 until order) {
      if (k(i) < 0 || k(i) > 2) {
        Logger.error("get, got index out of range (0, 2) ")
      }
      index += k(i) * pow(3,i.toDouble)
    }
    expressions(index.toInt)
  }
  def getDirect(k : Integer) : IR_Expression = {
    if (k >= pow(3,order.toDouble).toInt) {
      Logger.error("getDirect, got index out of range <" + pow(3,order.toDouble).toInt.toString)
    }
    expressions(k)
  }
  def set(k : List[Integer], num : IR_Expression) : Unit = {
    if (k.length != order) {
      Logger.error("set needs a list of integer with length of order")
    }
    var index : Double = 0
    for (i <- 0 until order) {
      index += k(i) * pow(3, i.toDouble)
    }
    expressions(index.toInt) = num
  }
  def setDirect(k : Integer, num : IR_Expression) : Unit = {
    if (k >= pow(3,order.toDouble).toInt) {
      Logger.error("setDirect, got index out of range <" + pow(3, order.toDouble).toInt.toString)
    }
    expressions(k) = num
  }
  override def toString : String = { "IR_TensorExpressionN(" + innerDatatype + "," + order + "; Items: " +
    expressions.mkString(", ") + ")" }
}

/*

// TODO: Zeus, eventuell für alle Tensor Varianten aufmachen
object IR_SetupTensor2Expressions extends DefaultStrategy("Convert accesses to matrices and vectors to MatrixExpressions") {
  def duplicateExpressions(access : IR_Expression, dt : IR_TensorDatatype2) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until 3)
      for (col <- 0 until 3)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  this += Transformation("Wrap", {
    case m @ IR_TensorExpression2(_)             => m.get(0, 0)
    case IR_TensorDatatype2(dt)                  => dt
    case m : IR_TensorExpression2                      => m // no need to process further
    case hda : IR_HighDimAccess                       => hda // no need to process further
    case x : IR_FunctionCall if (x.name != "inverse") => x

    case access @ IR_VariableAccess(_, m : IR_TensorDatatype2) => IR_TensorExpression2(Some(m.datatype), duplicateExpressions(access, m))

    case access : IR_MultiDimFieldAccess if access.datatype.isInstanceOf[IR_TensorDatatype2] =>
      val m = access.datatype.asInstanceOf[IR_TensorDatatype2]
      IR_TensorExpression2(Some(m.datatype), duplicateExpressions(access, m))

    // FIXME: add support for stencil fields
  }, false)
}*/
