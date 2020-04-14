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

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.prettyprinting._
import exastencils.util.ir._

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackTenComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_TensorExpression2
object IR_TensorExpression2 {
  // new with empty tensor
  def apply(innerDatatype : IR_Datatype) : IR_TensorExpression2 = new IR_TensorExpression2(Some(innerDatatype))

  // new fill with given array
  def apply(innerDatatype : Option[IR_Datatype], expressions : Array[IR_Expression]) : IR_TensorExpression2 = {
    if (expressions.length != 9) {
      Logger.error("expressions has the wrong length")
    }
    val tmp = new IR_TensorExpression2(innerDatatype)
    tmp.expressions = expressions
    tmp
  }

  // new fill with matrix
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

  // new and fill with other expression list array ...
  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_TensorExpression2 = {
    if (expressions.toArray.length != 9) {
      Logger.error("expressions has a wrong count of entries")
    } else {
      val tmp = IR_TensorExpression2(datatype.datatype)
      tmp.expressions = expressions.toArray
      tmp
    }
  }

  // new from dyadic product
  def apply(innerDatatype : Option[IR_Datatype], arr1 : Array[IR_Expression], arr2 : Array[IR_Expression]) : IR_TensorExpression2 = {
    if ((arr1.length != 3) || (arr2.length != 3)) {
      Logger.error("both input arrays must have size 3")
    } else {
      val tmp = IR_TensorExpression2(innerDatatype)
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(arr1(x),arr2(y)))
        }
      }
      tmp
    }
  }

  // new and fill all elements with expression
  def fromSingleExpression(innerDatatype : IR_Datatype, num : IR_Number) : IR_TensorExpression2 = {
    val tmp = new IR_TensorExpression2(Some(innerDatatype))
    for (i <- 0 until 9)
      tmp.expressions(i) = Duplicate(num)
    tmp
  }
}

case class IR_TensorExpression2(var innerDatatype : Option[IR_Datatype]) extends IR_Expression {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](9)

  override def datatype = {
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

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])
  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(x : Integer, y : Integer) = expressions(y * 3 + x)
  def set(x : Integer, y: Integer, num : IR_Expression) = expressions(y * 3 + x) = num
  override def toString : String = { "IR_TensorExpression2(" + innerDatatype + "," + 2 + "; Items: " + expressions.mkString(", ") + ")" }
}

// TODO: Hier geht der Spaß los
object IR_ResolveTensor2Functions extends DefaultStrategy("Resolve special tensor functions") {

  def getElem(exp : IR_Expression, row : Integer, col : Integer) = {
    exp match {
      case x : IR_TensorExpression2                                           => x.get(row, col)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row, col)))
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }
  def getSingleElem(exp : IR_Expression) = {
    exp match {
      case x : IR_TensorExpression2                                           => x.get(0, 0)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => x
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }

  def determinant(m : IR_TensorExpression2) : IR_Expression = {
    var det : IR_Expression = IR_RealConstant(0)
    var tmpDet : IR_Expression = IR_Multiplication(m.get(1,1), m.get(2,2), m.get(3,3))
    tmpDet = IR_Addition(tmpDet, IR_Multiplication(m.get(1,2), m.get(2,3), m.get(3,1)))
    tmpDet += IR_Addition(tmpDet, IR_Multiplication(m.get(1,3), m.get(2,1), m.get(3,2)))
    tmpDet += IR_Addition(tmpDet, IR_Multiplication(IR_Negation(m.get(3,1)), m.get(2,2), m.get(1,3)))
    tmpDet += IR_Addition(tmpDet, IR_Multiplication(IR_Negation(m.get(2,1)), m.get(1,2), m.get(3,3)))
    tmpDet += IR_Addition(tmpDet, IR_Multiplication(IR_Negation(m.get(1,1)), m.get(3,2), m.get(2,3)))
    det += IR_GeneralSimplifyWrapper.process[IR_Expression](tmpDet)
    IR_GeneralSimplifyWrapper.process(det)
  }

  def trace(m : IR_TensorExpression2) : IR_Expression = {
    var trace : IR_Expression = IR_RealConstant(0)
    val tmpDet = IR_Addition(m.get(1,1), m.get(2,2), m.get(3,3))
    trace += IR_GeneralSimplifyWrapper.process[IR_Expression](tmpDet)
    IR_GeneralSimplifyWrapper.process(trace)
  }

  def addTwoTensors(m: IR_TensorExpression2, n : IR_TensorExpression2) : IR_TensorExpression2 = {
    var tmp : IR_TensorExpression2 = null
    if (m.innerDatatype != n.innerDatatype) { //TODO: warum gehen hier nur Constant und keine Datatype
      if (m.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else if (n.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
        tmp = IR_TensorExpression2(n.innerDatatype)
      } else if (m.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else if (n.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
        tmp = IR_TensorExpression2(n.innerDatatype)
      }  else if (m.innerDatatype.isInstanceOf[Option[IR_FloatConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else {
        tmp = IR_TensorExpression2(n.innerDatatype)
      }
    } else {
      tmp = IR_TensorExpression2(m.innerDatatype)
    }
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Addition(m.get(x, y), n.get(x, y)))
      }
    }
    tmp
  }

  def addTensorsMatrix(m: IR_TensorExpression2, n : IR_MatrixExpression) : IR_TensorExpression2 = {
    if (n.rows != 3 || n.columns != 3) {
      Logger.error("matrix has the wrong dimension")
    } else {
      var tmp : IR_TensorExpression2 = null
      if (m.innerDatatype != n.innerDatatype) {
        if (m.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
          tmp = IR_TensorExpression2(m.innerDatatype)
        } else if (n.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
          tmp = IR_TensorExpression2(n.datatype)
        } else if (m.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
          tmp = IR_TensorExpression2(m.innerDatatype)
        } else if (n.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
          tmp = IR_TensorExpression2(n.datatype)
        } else if (m.innerDatatype.isInstanceOf[Option[IR_FloatConstant]]) {
          tmp = IR_TensorExpression2(m.innerDatatype)
        } else {
          tmp = IR_TensorExpression2(n.datatype)
        }
      } else {
        tmp = IR_TensorExpression2(m.innerDatatype)
      }
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Addition(m.get(x, y), n.get(x, y)))
        }
      }
      tmp
    }
  }

  /*
  def addTensorsMatrix(m: IR_TensorExpression2, n : ListBuffer[ListBuffer[IR_Number]]) : IR_TensorExpression2 = {
    if ((n.toArray.length != 3) || (n.head.toArray.length !=3)) {
      Logger.error("matrix has the wrong dimension")
    } else {
      var tmp : IR_TensorExpression2 = _
      if (n.toArray.head.toArray.head.isInstanceOf[m.innerDatatype] ) {
        if (m.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
          tmp = IR_TensorExpression2(m.innerDatatype)
        } else if (n.toArray.head.toArray.head.isInstanceOf[Option[IR_DoubleConstant]]) {
          tmp = IR_TensorExpression2(n.datatype)
        } else if (m.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
          tmp = IR_TensorExpression2(m.innerDatatype)
        } else if (n.toArray.head.toArray.head.isInstanceOf[Option[IR_RealConstant]]) {
          tmp = IR_TensorExpression2(n.datatype)
        } else if (m.innerDatatype.isInstanceOf[Option[IR_FloatConstant]]) {
          tmp = IR_TensorExpression2(m.innerDatatype)
        } else {
          tmp = IR_TensorExpression2(n.datatype)
        }
      } else {
        tmp = IR_TensorExpression2(m.innerDatatype)
      }
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, m.get(x, y) + n.get(x, y))
        }
      }
      tmp
    }
  } TODO: Hier muss das mit dem Typcheck erldigt werden */

  def dotProductTwoTensors(m: IR_TensorExpression2, n : IR_TensorExpression2) : IR_TensorExpression2 = {
    var tmp : IR_TensorExpression2 = null
    if (m.innerDatatype != n.innerDatatype) {
      if (m.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else if (n.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
        tmp = IR_TensorExpression2(n.innerDatatype)
      } else if (m.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else if (n.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
        tmp = IR_TensorExpression2(n.innerDatatype)
      }  else if (m.innerDatatype.isInstanceOf[Option[IR_FloatConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else {
        tmp = IR_TensorExpression2(n.innerDatatype)
      }
    } else {
      tmp = IR_TensorExpression2(m.innerDatatype)
    }
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(m.get(x, y), n.get(x, y)))
      }
    }
    tmp
  }

  def scalarProduct(m: IR_TensorExpression2, n : IR_Number) : IR_TensorExpression2 = {
    var tmp : IR_TensorExpression2 = null
    if (m.innerDatatype != n.datatype) {
      if (m.innerDatatype.isInstanceOf[Option[IR_DoubleConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else if (n.datatype.isInstanceOf[Option[IR_DoubleConstant]]) {
        tmp = IR_TensorExpression2(n.datatype)
      } else if (m.innerDatatype.isInstanceOf[Option[IR_RealConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else if (n.datatype.isInstanceOf[Option[IR_RealConstant]]) {
        tmp = IR_TensorExpression2(n.datatype)
      }  else if (m.innerDatatype.isInstanceOf[Option[IR_FloatConstant]]) {
        tmp = IR_TensorExpression2(m.innerDatatype)
      } else {
        tmp = IR_TensorExpression2(n.datatype)
      }
    } else {
      tmp = IR_TensorExpression2(m.innerDatatype)
    }
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(m.get(x, y), n))
      }
    }
    tmp
  }
}

/*
object IR_ResolveTensorAssignments extends DefaultStrategy("Resolve assignments to tensors") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  this += new Transformation("scalarize 1/2", {
    case stmt : IR_VariableDeclaration => stmt

    case IR_Assignment(dest, num : IR_Number, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_TensorExpression] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_FunctionCall("std::fill", ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dt.resolveFlattendSize, num)) : IR_Statement

    case IR_Assignment(dest, src : IR_VariableAccess, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_TensorExpression] && src.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_FunctionCall("std::copy", ListBuffer[IR_Expression](Duplicate(src), Duplicate(src) + dt.resolveFlattendSize, dest)) : IR_Statement

    case stmt @ IR_Assignment(dest, _, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach {
            case _ : IR_FunctionArgument                                                                                                            => // do not mark function arguments to be resolved into individual accesses
            case x @ (_ : IR_VariableAccess | _ : IR_TensorExpression | _ : IR_MultiDimFieldAccess) if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => {
              x.annotate(annotationMatrixRow, row)
              x.annotate(annotationMatrixCol, col)
            }
            case exp                                                                                                                                =>
          }
          newStmts += cloned
        }
      }
      newStmts
  })

  this += new Transformation("expressions 2/2", {
    case exp : IR_TensorExpression if (exp.hasAnnotation(annotationMatrixRow)) =>
      exp.get(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))

    case exp : IR_Expression if (exp.hasAnnotation(annotationMatrixRow)) =>
      IR_HighDimAccess(Duplicate(exp), IR_ConstIndex(Array(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))))
  }, false)
}
*/

/*
object IR_SetupTensorExpressions extends DefaultStrategy("Convert accesses to tensors to Tensorexpressions") {
  def duplicateExpressions(access : IR_Expression, dt : IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  this += Transformation("Wrap", {
    case m @ IR_TensorExpression(_, 1, 1)             => m.get(0, 0)
    case IR_MatrixDatatype(dt, 1, 1)                  => dt
    case m : IR_TensorExpression                      => m // no need to process further
    case hda : IR_HighDimAccess                       => hda // no need to process further
    case x : IR_FunctionCall if (x.name != "inverse") => x

    case access @ IR_VariableAccess(_, m : IR_MatrixDatatype) if (m.sizeM > 1 || m.sizeN > 1) => IR_TensorExpression(Some(m.datatype), m.sizeM, m.sizeN, duplicateExpressions(access, m))

    case access : IR_MultiDimFieldAccess if access.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val m = access.datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM > 1 || m.sizeN > 1)
        IR_TensorExpression(Some(m.datatype), m.sizeM, m.sizeN, duplicateExpressions(access, m))
      else
        access

    // FIXME: add support for stencil fields
  }, false)
}

*/