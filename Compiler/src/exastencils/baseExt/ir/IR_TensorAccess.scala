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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.ir._

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackTenComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

/* ######################################################################################################################################
#########################                      abstract IR_TensorExpresion                            ###################################
###################################################################################################################################### */

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

/* ######################################################################################################################################
#########################                        IR_TensorExpression1                                 ###################################
###################################################################################################################################### */

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
  override def toString : String = { "IR_TensorExpression1(" + innerDatatype + "," + 1 + "; Items: " + expressions.mkString(", ") + ")" }

}

/* ######################################################################################################################################
#########################                        IR_TensorExpression2                                 ###################################
###################################################################################################################################### */


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

  /** creates a second order tensor expression from dyadic product of arrays
   *
   * @param arr1 : Array[IR_Expression], input array with lenght 3, shoud contain IR numeric datatype
   * @param arr2 : Array[IR_Expression], input array with lenght 3, shoud contain IR numeric datatype
   * @return IR_TensorExpression2 instance
   */
  def dyadic(arr1 : Array[IR_Expression], arr2 : Array[IR_Expression]) : IR_TensorExpression2 = {
    if ((arr1.length != 3) || (arr2.length != 3)) {
      Logger.error("both input arrays must have size 3")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(arr1.head.datatype, arr2.head.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(arr1(x),arr2(y)))
        }
      }
      tmp
    }
  }

  /** creates a second tensor expression from dyadic product of arrays
   *
   * @param tens1 : IR_TensorExpression1, input array with lenght 3, shoud contain IR numeric datatype
   * @param tens2 : IR_TensorExpression1, input array with lenght 3, shoud contain IR numeric datatype
   * @return IR_TensorExpression2 instance
   */
  def dyadic(tens1 : IR_TensorExpression1, tens2 : IR_TensorExpression1) : IR_TensorExpression2 = {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.expressions.head.datatype, tens2.expressions.head.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(tens1.get(x),tens2.get(y)))
        }
      }
      tmp
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
  override def toString : String = { "IR_TensorExpression2(" + innerDatatype + "," + 2 + "; Items: " + expressions.mkString(", ") + ")" }
}

/* ######################################################################################################################################
#########################                        IR_TensorExpressionN                                 ###################################
###################################################################################################################################### */

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

  /** creates a n-th order tensor from dyadic product of arrays
   *
   * @param arr : Array[IR_Expression], input array with lenght 3, shoud contain IR numeric datatype
   * @return IR_TensorExpression2 instance
   */
  def dyadic(arr : Array[Array[IR_Expression]]) : IR_TensorExpressionN = {
    for (i <- arr.indices) {
      if (arr(i).length != 3) Logger.error("a array for dyadic product has the wrong dimension")
    }
    var tmp : IR_TensorExpressionN = null
    if (arr.length == 1) {
      tmp = IR_TensorExpressionN(arr(0).head.datatype, order = 1)
      tmp.expressions = arr(0)
    } else if (arr.length == 2) {
      tmp = IR_TensorExpressionN(IR_ResultingDatatype(arr(0).head.datatype, arr(1).head.datatype), order = 2)
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(List(x,y), IR_Multiplication(arr(0)(x),arr(1)(y)))
        }
      }
    } else {
      tmp = IR_TensorExpressionN(IR_ResultingDatatype(arr(0).head.datatype, arr(1).head.datatype), order = arr.length) // TODO: Datentyp nicht ganz korrekt
      // TODO: Hier muss noch das ausrechnen der expresions rein
    }


    tmp
  }

  /** creates a n-th order tensor from dyadic product of arrays
   *
   * @param tens1 : IR_TensorExpression1, input array with lenght 3, shoud contain IR numeric datatype
   * @param tens2 : IR_TensorExpression1, input array with lenght 3, shoud contain IR numeric datatype
   * @return IR_TensorExpression2 instance
   */
  def dyadic(tens1 : IR_TensorExpression1, tens2 : IR_TensorExpression1) : IR_TensorExpression2 = {
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(tens1.expressions.head.datatype, tens2.expressions.head.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(tens1.get(x),tens2.get(y)))
      }
    }
    tmp
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
  override def toString : String = { "IR_TensorExpressionN(" + innerDatatype + "," + order + "; Items: " + expressions.mkString(", ") + ")" }
}


/* ######################################################################################################################################
#########################                  Implementation of tensor functions                         ###################################
###################################################################################################################################### */

/** Declaration of tensor functions and transformation nodes */
object IR_ResolveUserDefinedTensor2Functions extends DefaultStrategy("Resolve user defined functions") {
  var resolveFunctions = ListBuffer[String]()
  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dotp", "deter", "determinant", "getElement", "setElement", "inverse", "dyadic", "trace", "add")
  }

  this += new Transformation("add assignments/decl to function returns to arguments", {

    // Tensor 1 Assignment of function return
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_TensorDatatype1] && src.datatype.isInstanceOf[IR_TensorDatatype1])                   =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_TensorDatatype1] && src.datatype.isInstanceOf[IR_TensorDatatype1]) =>
      val decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_TensorDatatype1] && src.datatype.isInstanceOf[IR_TensorDatatype1])                  =>
      Logger.error("+= tensor1 operator resolution not yet implemented")

    // Tensor 2 Assignment of function return
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_TensorDatatype2] && src.datatype.isInstanceOf[IR_TensorDatatype2])                   =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_TensorDatatype2] && src.datatype.isInstanceOf[IR_TensorDatatype2]) =>
      val decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_TensorDatatype2] && src.datatype.isInstanceOf[IR_TensorDatatype2])                  =>
      Logger.error("+= tensor2 operator resolution not yet implemented")

    // Tensor N Assignment of function return
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_TensorDatatypeN] && src.datatype.isInstanceOf[IR_TensorDatatypeN])                   =>
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_VariableDeclaration(datatype, name, Some(src : IR_FunctionCall), _) if (!resolveFunctions.contains(src.name) && datatype.isInstanceOf[IR_TensorDatatypeN] && src.datatype.isInstanceOf[IR_TensorDatatypeN]) =>
      val decl = IR_VariableDeclaration(datatype, name, None)
      src.arguments += IR_VariableAccess(decl)
      ListBuffer[IR_Statement](
        decl,
        IR_ExpressionStatement(src)
      )
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (!resolveFunctions.contains(src.name) && dest.datatype.isInstanceOf[IR_TensorDatatypeN] && src.datatype.isInstanceOf[IR_TensorDatatypeN])                  =>
      Logger.error("+= tensorN operator resolution not yet implemented")
  })

  this += new Transformation("parameters and return types", {

    // Tensor 1 return types
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_TensorDatatype1])                                    =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) && func.datatype.isInstanceOf[IR_TensorDatatype1]) =>
      val tensor = func.datatype.asInstanceOf[IR_TensorDatatype1]
      func.parameters += IR_FunctionArgument("_tensor1_return", IR_ReferenceDatatype(tensor))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_TensorDatatype1]) => {
          List(
            IR_Assignment(IR_VariableAccess("_tensor1_return", tensor), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func

    // Tensor 2 return types
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_TensorDatatype2])                                    =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) && func.datatype.isInstanceOf[IR_TensorDatatype2]) =>
      val tensor = func.datatype.asInstanceOf[IR_TensorDatatype2]
      func.parameters += IR_FunctionArgument("_tensor2_return", IR_ReferenceDatatype(tensor))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_TensorDatatype2]) => {
          List(
            IR_Assignment(IR_VariableAccess("_tensor2_return", tensor), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func

    // Tensor N return types
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_TensorDatatypeN])                                    =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (!resolveFunctions.contains(func.name) && func.datatype.isInstanceOf[IR_TensorDatatypeN]) =>
      val tensor = func.datatype.asInstanceOf[IR_TensorDatatypeN]
      func.parameters += IR_FunctionArgument("_tensorN_return", IR_ReferenceDatatype(tensor))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_TensorDatatypeN]) => {
          List(
            IR_Assignment(IR_VariableAccess("_tensorN_return", tensor), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func
  })

}

/** Implementation of tensor functions. Separate function for each kind. */
object IR_ResolveTensorFunctions extends DefaultStrategy("Resolve special tensor functions") {

  def getElem(exp : IR_Expression, row : Integer, col : Integer, higherInd: List[Integer]) : IR_Expression = {
    exp match {
      case x : IR_TensorExpression1                                               => x.get(row)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpression1])   => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatype1])     => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row)))
      case x : IR_TensorExpression2                                               => x.get(row, col)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpression2])   => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row + col*3)))
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatype2])     => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row + col*3)))
      case x : IR_TensorExpressionN                                               => x.get(higherInd)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpressionN])   => {
        var index : Double = 0
        for (i <- higherInd.indices) {
          index += higherInd(i) + pow(3,i.toDouble)
        }
        IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(index.toInt)))
      }
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorDatatypeN])     => {
        var index : Double = 0
        for (i <- higherInd.indices) {
          index += higherInd(i) + pow(3,i.toDouble)
        }
        IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(index.toInt)))
      }
      case _                                                                      => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }
  def getSingleElem(exp : IR_Expression) : IR_Expression = {
    exp match {
      case x : IR_TensorExpression2                                           => x.get(0, 0)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_TensorExpression2]) => x
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  def determinant(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2                                           =>
      Duplicate(m.get(0, 0) * m.get(1, 1) * m.get(2, 2) +
        m.get(0, 1) * m.get(1, 2) * m.get(2, 0) +
        m.get(0, 2) * m.get(1, 0) * m.get(2, 1) -
        m.get(2, 0) * m.get(1, 1) * m.get(0, 2) -
        m.get(2, 1) * m.get(1, 2) * m.get(0, 0) -
        m.get(2, 2) * m.get(1, 0) * m.get(0, 1))
      case m : IR_VariableAccess                                              =>
        val acc = IR_HighDimAccess
        val ind = IR_ExpressionIndex
        IR_Addition(
          IR_Multiplication(acc(m, ind(0 + 0* 3)), acc(m, ind(1 + 1*3)), acc(m, ind(2 + 2*3))),
          IR_Multiplication(acc(m, ind(0 + 1*3)), acc(m, ind(1 + 2*3)), acc(m, ind(2 + 0*3))),
          IR_Multiplication(acc(m, ind(0 + 2*3)), acc(m, ind(1 + 0*3)), acc(m, ind(2 + 1*3))),
          IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 0*3)), acc(m, ind(1 + 1*3)), acc(m, ind(0 + 2*3)))),
          IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 1*3)), acc(m, ind(1 + 2*3)), acc(m, ind(0 + 0*3)))),
          IR_Subtraction(0, IR_Multiplication(acc(m, ind(2 + 2*3)), acc(m, ind(1 + 0*3)), acc(m, ind(0 + 1*3))))
        )
      case _                                                                  => Logger.error("Determine got the wrong type")
    }
  }

  def trace(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2 => IR_Addition(m.get(0, 0), m.get(1, 1), m.get(2, 2))
      case m : IR_VariableAccess    => IR_Addition(IR_HighDimAccess(m, IR_ExpressionIndex(0)), IR_HighDimAccess(m, IR_ExpressionIndex(1 + 1 * 3)), IR_HighDimAccess(m, IR_ExpressionIndex(2 + 2 * 3)))
      case _                        => Logger.error("Trace got the wrong type")
    }
  }

  private def addTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(m.datatype, n.datatype))
    for (x <- 0 until 3) {
      tmp.set(x, IR_Addition(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def addTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Addition(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }


  private def addTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    /*if (n.rows != 3 || n.columns != 3) { // TODO: Zeus, hier muss irgendwie das assert rein
      Logger.error("matrix has the wrong dimension")
    } else*/ {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Addition(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
        }
      }
      tmp
    }
  }

  def add(m : IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] && n.datatype.isInstanceOf[IR_TensorDatatype2] => addTwoTensors2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] && n.datatype.isInstanceOf[IR_TensorDatatype1] => addTwoTensors1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorExpression2] && n.datatype.isInstanceOf[IR_MatrixExpression]  => addTensor2Matrix(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_MatrixExpression] && n.datatype.isInstanceOf[IR_TensorExpression2]  => addTensor2Matrix(n, m)
      case (_, _)                                               => Logger.error("Add tensor got the a wrong type")
    }
  }

  private def dotProductTwoTensors1(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression1 = {
    val tmp = IR_TensorExpression1(IR_ResultingDatatype(m.datatype, n.datatype))
    for (x <- 0 until 3) {
      tmp.set(x, IR_Multiplication(getElem(m, x, 0, Nil), getElem(n, x, 0, Nil)))
    }
    tmp
  }

  private def dotProductTwoTensors2(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
      }
    }
    tmp
  }

  private def dotProductTensor2Matrix(m: IR_VariableAccess, n : IR_VariableAccess) : IR_TensorExpression2 = {
    //if (n.rows != 3 || n.columns != 3) {
    //  Logger.error("matrix has the wrong dimension")
    //} else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(getElem(m, x, y, Nil), getElem(n, x, y, Nil)))
        }
      }
      tmp
    //}
  }

  def dot(m : IR_Expression, n : IR_Expression) : IR_TensorExpression = {
    (m, n) match {
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype2] && n.datatype.isInstanceOf[IR_TensorDatatype2] => dotProductTwoTensors2(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorDatatype1] && n.datatype.isInstanceOf[IR_TensorDatatype1] => dotProductTwoTensors1(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_TensorExpression2] && n.datatype.isInstanceOf[IR_MatrixExpression]  => dotProductTensor2Matrix(m, n)
      case (m : IR_VariableAccess, n : IR_VariableAccess) if m.datatype.isInstanceOf[IR_MatrixExpression] && n.datatype.isInstanceOf[IR_TensorExpression2]  => dotProductTensor2Matrix(n, m)
      case (_, _)                                               => Logger.error("Dot product tensor got the a wrong type")
    }
  }

  def scalarProduct(m: IR_TensorExpression2, n : IR_Number) : IR_TensorExpression2 = {
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(m.get(x, y), n))
      }
    }
    tmp
  }

  def scalar(m: IR_Expression, n : IR_Expression) : IR_TensorExpression2 = {
    (m, n) match {
      case (m : IR_TensorExpression2, n : IR_Number)  => scalarProduct(m, n)
      case (m : IR_Number, n : IR_TensorExpression2)  => scalarProduct(n, m)
      case (_,  _)                                    => Logger.error("Scalar product tensor got the a wrong type")
    }
  }

  this += new Transformation("resolution of built-in functions 2/2", {

    case call : IR_FunctionCall if (call.name == "deter")    => // TODO : instanz prüfen
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      determinant(call.arguments.head)  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "trace")    => // TODO : instanz prüfen
      if (call.arguments.length != 1) {
        Logger.error("trace() must have one argument")
      }
      trace(call.arguments.head)  // TODO: Zeus, zu testen
      
    case IR_ElementwiseMultiplication(left, right) =>
      if (!left.isInstanceOf[IR_TensorExpression2]){
        Logger.error("Left input has the wrong type!")
      } else if (!right.isInstanceOf[IR_TensorExpression2]){
        Logger.error("Right input has the wrong type!")
      }
      val me = IR_TensorExpression2(IR_ResultingDatatype(left.datatype, right.datatype))
      for (row <- 0 until 9) {
        for (col <- 0 until 3) {
          me.set(row, col, IR_Multiplication(getElem(left, row, col, Nil), getElem(right, row, col, Nil)))
        }
      }
      me

    case call : IR_FunctionCall if (call.name == "dotp")                                          =>
      if (call.arguments.length != 2) { // TODO: Zeus, warum kollidiert das mit IR_MatrixAccess? "dot"
        Logger.error("mul() must have two arguments")
      }
      dot(call.arguments(0), call.arguments(1))  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "add")                                                                       =>
      if (call.arguments.length != 2) {
        Logger.error("add() must have two arguments")
      }
      add(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "scalar")                                                                   =>
      if (call.arguments.length != 2) {
        Logger.error("scalar() must have two arguments")
      }
      scalar(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen
  })
}


/* ######################################################################################################################################
#########################                  Implementation of tensor assignments                       ###################################
###################################################################################################################################### */

/** Objects transforms tensor assignments */
object IR_ResolveTensorAssignments extends DefaultStrategy("Resolve assignments tensors") {

  this += new Transformation("scalarize 1/2", {
    case stmt : IR_VariableDeclaration => stmt

    // Tensor 1 Assignment
    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype1] && src.isInstanceOf[IR_TensorExpression1] =>
      val tmp = src.asInstanceOf[IR_TensorExpression1]
      var newStmts = ListBuffer[IR_Statement]()
      for (x <- 0 until 3) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(x)), tmp.get(x))
      }
      newStmts

    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype1] && !src.isInstanceOf[IR_TensorExpression1] && src.datatype.isInstanceOf[IR_TensorDatatype1] =>
      var newStmts = ListBuffer[IR_Statement]()
      for (i <- 0 until 3) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i)), IR_HighDimAccess(src, IR_ExpressionIndex(i)))
      }
      newStmts

    // Tensor 2 Assignment
    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype2] && src.isInstanceOf[IR_TensorExpression2] =>
      val tmp = src.asInstanceOf[IR_TensorExpression2]
      var newStmts = ListBuffer[IR_Statement]()
      for (x <- 0 until 3) {
        for (y <- 0 until 3) {
          newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(x + y *3)), tmp.get(x, y))
        }
      }
      newStmts

    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype2] && !src.isInstanceOf[IR_TensorExpression2] && src.datatype.isInstanceOf[IR_TensorDatatype2] =>
      var newStmts = ListBuffer[IR_Statement]()
      for (i <- 0 until 9) {
          newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i)), IR_HighDimAccess(src, IR_ExpressionIndex(i)))
      }
      newStmts

    // Tensor N Assignment
    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatypeN] && src.isInstanceOf[IR_TensorExpressionN] =>
      val tmp = src.asInstanceOf[IR_TensorExpressionN]
      var newStmts = ListBuffer[IR_Statement]()
      for (x <- tmp.expressions.indices) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(x)), tmp.getDirect(x))
      }
      newStmts

    case IR_Assignment(dest, src, "=") if dest.datatype.isInstanceOf[IR_TensorDatatypeN] && !src.isInstanceOf[IR_TensorExpressionN] && src.datatype.isInstanceOf[IR_TensorDatatypeN] =>
      val tmp = src.asInstanceOf[IR_TensorExpressionN]
      var newStmts = ListBuffer[IR_Statement]()
      for (i <- tmp.expressions.indices) {
        newStmts += IR_Assignment(IR_HighDimAccess(dest, IR_ExpressionIndex(i)), IR_HighDimAccess(src, IR_ExpressionIndex(i)))
      }
      newStmts

  })
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
