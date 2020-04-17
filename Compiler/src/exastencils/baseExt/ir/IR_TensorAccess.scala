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

// Resolve user defined functions
object IR_ResolveUserDefinedTensor2Functions extends DefaultStrategy("Resolve user defined functions") {
  var resolveFunctions = ListBuffer[String]()
  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "deter", "determinant", "getElement", "setElement", "inverse", "dyadic")
  }

  this += new Transformation("add assignments/decl to function returns to arguments", {
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
      Logger.error("+= tensor operator resolution not yet implemented")
  })

  this += new Transformation("parameters and return types", {
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
  })

}

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

  def determinant(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2                                           =>
      Duplicate(m.get(0, 0) * m.get(1, 1) * m.get(2, 2) +
        m.get(0, 1) * m.get(1, 2) * m.get(2, 0) +
        m.get(0, 2) * m.get(1, 0) * m.get(2, 1) -
        m.get(2, 0) * m.get(1, 1) * m.get(0, 2) -
        m.get(2, 1) * m.get(1, 2) * m.get(0, 0) -
        m.get(2, 2) * m.get(1, 0) * m.get(0, 1))
      case _                                                                  => Logger.error("Determine got the wrong type")
    }
  }

  def trace(m : IR_Expression) : IR_Expression = {
    m match {
      case m : IR_TensorExpression2 => IR_Addition(m.get(1, 1), m.get(2, 2), m.get(3, 3))
      case _                        => Logger.error("Trace got the wrong type")
    }
  }

  def addTwoTensors2(m: IR_TensorExpression2, n : IR_TensorExpression2) : IR_TensorExpression2 = {
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Addition(m.get(x, y), n.get(x, y)))
      }
    }
    tmp
  }

  def addTensor2Matrix(m: IR_TensorExpression2, n : IR_MatrixExpression) : IR_TensorExpression2 = {
    if (n.rows != 3 || n.columns != 3) {
      Logger.error("matrix has the wrong dimension")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Addition(m.get(x, y), n.get(x, y)))
        }
      }
      tmp
    }
  }

  def add(m : IR_Expression, n : IR_Expression) : IR_TensorExpression2 = {
    (m, n) match {
      case (m : IR_TensorExpression2, n : IR_TensorExpression2) => addTwoTensors2(m, n)
      case (m : IR_TensorExpression2, n : IR_MatrixExpression)  => addTensor2Matrix(m, n)
      case (m : IR_MatrixExpression, n : IR_TensorExpression2)  => addTensor2Matrix(n, m)
      case (_, _)                                               => Logger.error("Add tensor got the a wrong type")
    }
  }

  def dotProductTwoTensors2(m: IR_TensorExpression2, n : IR_TensorExpression2) : IR_TensorExpression2 = {
    val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
    for (y <- 0 until 3) {
      for (x <- 0 until 3) {
        tmp.set(x, y, IR_Multiplication(m.get(x, y), n.get(x, y)))
      }
    }
    tmp
  }

  def dotProductTensor2Matrix(m: IR_TensorExpression2, n : IR_MatrixExpression) : IR_TensorExpression2 = {
    if (n.rows != 3 || n.columns != 3) {
      Logger.error("matrix has the wrong dimension")
    } else {
      val tmp = IR_TensorExpression2(IR_ResultingDatatype(m.datatype, n.datatype))
      for (y <- 0 until 3) {
        for (x <- 0 until 3) {
          tmp.set(x, y, IR_Multiplication(m.get(x, y), n.get(x, y)))
        }
      }
      tmp
    }
  }

  def dot(m : IR_Expression, n : IR_Expression) : IR_TensorExpression2 = {
    (m, n) match {
      case (m : IR_TensorExpression2, n : IR_TensorExpression2) => dotProductTwoTensors2(m, n)
      case (m : IR_TensorExpression2, n : IR_MatrixExpression)  => dotProductTensor2Matrix(m, n)
      case (m : IR_MatrixExpression, n : IR_TensorExpression2)  => dotProductTensor2Matrix(n, m)
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
      
    case IR_ElementwiseMultiplication(left, right) =>
      if (!left.isInstanceOf[IR_TensorExpression2]){
        Logger.error("Left input has the wrong type!")
      } else if (!right.isInstanceOf[IR_TensorExpression2]){
        Logger.error("Right input has the wrong type!")
      }
      val me = IR_TensorExpression2(IR_ResultingDatatype(left.datatype, right.datatype))
      for (row <- 0 until 9) {
        for (col <- 0 until 3) {
          me.set(row, col, IR_Multiplication(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case call : IR_FunctionCall if (call.name == "mul") || (call.name == "dot")                                                                     =>
      if (call.arguments.length != 2) {
        Logger.error("mul() must have two arguments")
      }
      dot(call.arguments(0), call.arguments(1))  // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "add")                                                                       =>
      if (call.arguments.length != 2) {
        Logger.error("add() must have two arguments")
      }
      add(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

    case call : IR_FunctionCall if (call.name == "scalar")                                                                      =>
      if (call.arguments.length != 2) {
        Logger.error("scalar() must have two arguments")
      }
      scalar(call.arguments(0), call.arguments(1)) // TODO: Zeus, zu testen

    /*case call : IR_FunctionCall if (call.name == "get")                                                                      =>
      if (call.arguments.length != 3) {
        Logger.error("get() must have two arguments")
      }
      getElem(call.arguments(0), call.arguments(1), call.arguments(2)) */
  })
}


// TODO: Hier gehts weiter
object IR_ResolveTensorAssignments extends DefaultStrategy("Resolve assignments to matrices") {
  //val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  //val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  this += new Transformation("scalarize 1/2", {
    case stmt : IR_VariableDeclaration => stmt

    case IR_Assignment(dest, num : IR_Number, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype2] && !dest.isInstanceOf[IR_TensorExpression2] =>
      val dt = dest.datatype.asInstanceOf[IR_TensorDatatype2]
      IR_FunctionCall("std::fill", ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dt.resolveFlattendSize, num)) : IR_Statement

    case IR_Assignment(dest, src : IR_VariableAccess, "=") if dest.datatype.isInstanceOf[IR_TensorDatatype2] && !dest.isInstanceOf[IR_TensorExpression2] && src.datatype.isInstanceOf[IR_TensorDatatype2] =>
      val dt = dest.datatype.asInstanceOf[IR_TensorDatatype2]
      IR_FunctionCall("std::copy", ListBuffer[IR_Expression](Duplicate(src), Duplicate(src) + dt.resolveFlattendSize, dest)) : IR_Statement
/*
    case stmt @ IR_Assignment(dest, _, _) if (dest.datatype.isInstanceOf[IR_TensorDatatype2]) =>
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until 3) {
        for (col <- 0 until 3) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach {
            case _ : IR_FunctionArgument                                                                                                            => // do not mark function arguments to be resolved into individual accesses
            case x @ (_ : IR_VariableAccess | _ : IR_MatrixExpression | _ : IR_MultiDimFieldAccess) if (x.datatype.isInstanceOf[IR_TensorDatatype2]) => {
              x.annotate(annotationMatrixRow, row)
              x.annotate(annotationMatrixCol, col)
            }
            case exp                                                                                                                                =>
          }
          newStmts += cloned
        }
      }
      newStmts*/
  })
}



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
}
