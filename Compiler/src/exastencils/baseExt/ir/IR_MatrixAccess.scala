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

import exastencils.base.ir.IR_ImplicitConversion._
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

import scala.collection.mutable.ListBuffer

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat: IR_VariableAccess, var i: IR_Expression, var j: IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype

  override def prettyprint(out: PpStream): Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_MatrixExpression
object IR_MatrixExpression {
  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(innerDatatype, rows, columns)
  def apply(innerDatatype: IR_Datatype, rows: Integer, columns: Integer): IR_MatrixExpression = new IR_MatrixExpression(Some(innerDatatype), rows, columns)

  def apply(innerDatatype: Option[IR_Datatype], rows: Integer, columns: Integer, expressions: Array[IR_Expression]): IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }

  def apply(innerDatatype: Option[IR_Datatype], expressions: ListBuffer[ListBuffer[IR_Expression]]): IR_MatrixExpression = {
    val rows = expressions.size
    val columns = expressions(0).size
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        tmp.set(row, col, expressions(row)(col))
      }
    }
    tmp
  }

  def apply(datatype: IR_MatrixDatatype, expressions: ListBuffer[IR_Expression]): IR_MatrixExpression = {
    val tmp = IR_MatrixExpression(datatype.datatype, datatype.sizeM, datatype.sizeN)
    tmp.expressions = expressions.toArray
    tmp
  }

  def fromSingleExpression(innerDatatype: IR_Datatype, rows: Integer, columns: Integer, expression: IR_Expression): IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(Some(innerDatatype), rows, columns)
    for (i <- 0 until rows * columns)
      tmp.expressions(i) = Duplicate(expression)
    tmp
  }
}

case class IR_MatrixExpression(var innerDatatype: Option[IR_Datatype], var rows: Int, var columns: Int) extends IR_Expression {
  var expressions: Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)


  override def datatype = {
    innerDatatype match {
      case None =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt: IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _ =>
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out: PpStream): Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }

  override def prettyprint(out: PpStream): Unit = {
    out << "__matrix_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << '_' << rows << "_" << columns << "_t "
    prettyprintInner(out)
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])

  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])

  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])

  def get(row: Integer, column: Integer) = expressions(row * columns + column)

  def set(row: Integer, column: Integer, exp: IR_Expression) = expressions(row * columns + column) = exp

  def inverse: IR_MatrixExpression = {
    IR_CompiletimeInversion.inverse(this, Knowledge.experimental_resolveInverseFunctionCall, Knowledge.experimental_blocksize)
  }

  override def toString: String = {
    "IR_MatrixExpression(" + innerDatatype + ", " + rows + ", " + columns + "; Items: " + expressions.mkString(", ") + ")"
  }

}

object IR_ExtractMatricesNew extends DefaultStrategy("Extract and split matrix expressions where necessary") {
  this += new Transformation("declarations", {
    //Definition of matrix variable including initialisation -> split into decl and assignment
    case decl@IR_VariableDeclaration(matrix: IR_MatrixDatatype, _, Some(call: IR_FunctionCall), _) =>
      //Logger.error("call " + call.name + " with returntype " + call.datatype + "\n")
      val newStmts = ListBuffer[IR_Statement]()
      // split declaration and definition so each part can be handled by subsequent transformations
      newStmts += IR_VariableDeclaration(matrix, decl.name, None)
      newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), call)
      newStmts
  })
}
object IR_ResolveMatrixFunctionsNew extends DefaultStrategy("Resolve special matrix functions") {
  var debug = true

  this += new Transformation("resolve built-in functions", {
    case stmts@IR_Assignment(dest: IR_VariableAccess, call : IR_FunctionCall, _) if( call.name == "dotProduct") =>
      if (call.arguments.length != 2) {
        Logger.error(s"dotProduct() must have two arguments; has ${call.arguments.length}")
      }

      if(!dest.datatype.isInstanceOf[IR_MatrixDatatype])
        Logger.error("assigning to Variable of wrong datatype: " + dest.datatype)

      var left = call.arguments(0)
      var right = call.arguments(1)

      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Dot product argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      val rsize = right match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Dot product argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${lsize._1}x${lsize._2} * ${rsize._1}x${rsize._2}")
      }

      var stmts = ListBuffer[IR_Statement]()
      for(i <- 0 until lsize._1) {
        for(j <- 0 until rsize._2) {
          var tmp = IR_Addition()
          for(k <- 0 until rsize._1) {
            if(k == 0)
              tmp = IR_Addition(IR_DoubleConstant(0),IR_Multiplication(IR_HighDimAccess(left,IR_ExpressionIndex(i,k)),IR_HighDimAccess(right,IR_ExpressionIndex(k,j))))
            else
              tmp = IR_Addition(tmp, IR_Multiplication(IR_HighDimAccess(left,IR_ExpressionIndex(i,k)),IR_HighDimAccess(right,IR_ExpressionIndex(k,j))))
          }
          stmts += IR_Assignment(IR_HighDimAccess(dest,IR_ExpressionIndex(i,j)),Duplicate(tmp))
        }
      }
      if(debug) {
         stmts ++= IR_GenerateBasicMatrixOperations.printMatrix(dest)
      }
      stmts
  })

  this += new Transformation("resolve inversion", {
    case stmts@IR_Assignment(dest: IR_VariableAccess, call: IR_FunctionCall, _) if (call.name == "inverse") =>
      if(call.arguments.length != 1)
        Logger.error("resolve inversion: inverse call with " + call.arguments.length + " arguments not supported")
      if(!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype] && !call.arguments(0).datatype.isInstanceOf[IR_MatrixExpression])
        Logger.error("resolve inversion: inverse call on object that is of type " + call.arguments(0).datatype + " not supported")

      if(Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
        IR_GenerateRuntimeInversion.inverse(call.arguments(0).asInstanceOf[IR_VariableAccess], dest)
      } else if(Knowledge.experimental_resolveInverseFunctionCall == "Compiletime") {
       IR_Assignment(dest,IR_CompiletimeInversion.inverse(call.arguments(0).asInstanceOf[IR_MatrixExpression],Knowledge.experimental_matrixStructure, Knowledge.experimental_blocksize))
      } else {
        Logger.error("resolve inversion: type of inverse resolve " + Knowledge.experimental_resolveInverseFunctionCall + " not supported")
      }
  })

  }

object IR_ExtractMatrices extends DefaultStrategy("Extract and split matrix expressions where necessary") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  var globalCollection: Option[IR_GlobalCollection] = None

  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "transpose")
    if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime") resolveFunctions += "inverse"
    globalCollection = StateManager.findFirst[IR_GlobalCollection]()
  }

  this += new Transformation("assignment of operation with self", {
    case stmt@IR_Assignment(dest: IR_VariableAccess, src, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // resolve M = M * M into tmp = M * M; M = tmp
      var selfassign = false
      StateManager.findAll[IR_Multiplication](HelperNode(src)).foreach(mult =>
        if (mult.factors.exists(p => p.isInstanceOf[IR_VariableAccess] && p.asInstanceOf[IR_VariableAccess].name == dest.name))
          selfassign = true
      )

      if (selfassign) {
        var newStmts = ListBuffer[IR_Statement]()
        val decl = IR_VariableDeclaration(dest.datatype, "_matrixExp" + matExpCounter, src)
        newStmts += decl
        stmt.dest = IR_VariableAccess(decl)
        newStmts += stmt
        newStmts += IR_Assignment(dest, IR_VariableAccess(decl))
        matExpCounter += 1
        newStmts
      } else {
        stmt
      }
  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {
    this += new Transformation("preparation", {
      case call@IR_FunctionCall(_, args) if (call.name == "inverse") =>
        args(0).datatype match {
          case _: IR_ScalarDatatype => IR_DoubleConstant(1.0) / args(0)

          case m: IR_MatrixDatatype =>
            args(0).datatype.asInstanceOf[IR_MatrixDatatype]
            if (m.sizeM > 1) {
              call.function = IR_PlainInternalFunctionReference("_runtimeInverseMatrix", m)
            }
            call
        }
    })
  }

  this += new Transformation("global declarations", {
    case decl@IR_VariableDeclaration(_: IR_MatrixDatatype, _, Some(exp: IR_Expression), _) =>
      StateManager.findFirst[IR_GlobalCollection]().get.initGlobals.asInstanceOf[IR_Function].body += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      decl.initialValue = None
      decl
  }, applyAtNode = StateManager.findFirst[IR_GlobalCollection]())
  /*
  this += new Transformation("declarations", {
  //Definition of matrix variable including initialisation -> split into decl and assignment
  case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) =>
  val newStmts = ListBuffer[IR_Statement]()
  // split declaration and definition so each part can be handled by subsequent transformations
  newStmts += IR_VariableDeclaration(matrix, decl.name, None)
  newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
  newStmts
  })
  */
  this += new Transformation("extract function calls 1/3", {
    case stmt@IR_Assignment(_, src, _) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).filter(f => !resolveFunctions.contains(f.function.name)).foreach(exp => {
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      // FIXME: only do the following if necessary
      //      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
    case stmt@IR_ExpressionStatement(src) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // Extract all function calls into separate variables since any function could have unwanted side effects if called more than once
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => { // resolveFunction check not needed: all internally resolved function return a value
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      //      StateManager.findAll[IR_MatrixExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
  })

  this += new Transformation("extract function calls 3/3", {
    case exp: IR_FunctionCall if (exp.hasAnnotation(annotationFctCallCounter)) =>
      IR_VariableAccess("_fct" + exp.popAnnotationAs[Int](annotationFctCallCounter) + "_" + exp.function.name.replace('<', '_').replace('>', '_'), exp.function.returnType)

    case exp: IR_MatrixExpression if (exp.hasAnnotation(annotationMatExpCounter)) =>
      IR_VariableAccess("_matrixExp" + exp.popAnnotationAs[Int](annotationMatExpCounter), exp.datatype)
  })

  this += new Transformation("parameters and return types", {
    case arg: IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func: IR_Function if (func.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      val matrix = func.datatype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          List(
            IR_Assignment(IR_VariableAccess("_matrix_return", matrix), exp),
            IR_Return())
        }
        case _ => List(stmt)
      })
      func
  })

  this += new Transformation("function call returns", {
    case IR_Assignment(dest, src: IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_Assignment(dest, src: IR_FunctionCall, "+=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      Logger.error("+= matrix operator resolution not yet implemented")
  })

  this += new Transformation("simplify function call arguments", {
    case stmt@IR_ExpressionStatement(exp: IR_FunctionCall) =>
      var newStmts = ListBuffer[IR_Statement]()
      exp.arguments.transform {
        case argexp: IR_MultiDimFieldAccess => argexp
        case argexp: IR_VariableAccess => argexp
        case argexp: IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, argexp)
          newStmts += decl
          IR_VariableAccess(decl)
        }
        case arg => arg
      }
      newStmts += stmt
      newStmts
    case stmt@IR_Assignment(_, exp: IR_FunctionCall, _) if !resolveFunctions.contains(exp.function.name) =>
      var newStmts = ListBuffer[IR_Statement]()

      exp.arguments.transform {
        case argexp: IR_MultiDimFieldAccess => argexp
        case argexp: IR_VariableAccess => argexp
        case argexp: IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, argexp)
          newStmts += decl
          IR_VariableAccess(decl)
        }
        case arg => arg
      }
      newStmts += stmt
      newStmts
  })
}

object IR_ResolveMatrixFunctions extends DefaultStrategy("Resolve special matrix and vector functions") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  def calculateDeterminant(m: IR_MatrixExpression): IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("determinant for non-quadratic matrices not implemented")
      // FIXME Nullzeilen/-spalten ergaenzen
    }
    if (m.rows <= 0) {
      Logger.error("MatrixExpression of size <= 0")
    } else if (m.rows == 1) {
      return Duplicate(m.get(0, 0))
    } else if (m.rows == 2) {
      return Duplicate(m.get(0, 0) * m.get(1, 1) - m.get(0, 1) * m.get(1, 0))
    } else if (m.rows == 3) {
      return Duplicate(m.get(0, 0) * m.get(1, 1) * m.get(2, 2) +
        m.get(0, 1) * m.get(1, 2) * m.get(2, 0) +
        m.get(0, 2) * m.get(1, 0) * m.get(2, 1) -
        m.get(2, 0) * m.get(1, 1) * m.get(0, 2) -
        m.get(2, 1) * m.get(1, 2) * m.get(0, 0) -
        m.get(2, 2) * m.get(1, 0) * m.get(0, 1))
    } else {
      var det: IR_Expression = IR_IntegerConstant(0)
      val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
      // laplace expansion
      for (i <- 0 until m.rows) {
        var tmpRow = 0
        for (row <- 0 until m.rows) {
          if (row != i) {
            for (col <- 1 until m.columns) {
              tmp.set(tmpRow, col - 1, Duplicate(m.get(row, col)))
            }
            tmpRow += 1
          }
        }
        val tmpDet = m.get(i, 0) * calculateDeterminant(tmp) * IR_DoubleConstant(math.pow(-1, i))
        det += IR_GeneralSimplifyWrapper.process[IR_Expression](tmpDet)
      }
      IR_GeneralSimplifyWrapper.process(det)
    }
  }

  def calculateMatrixOfMinorsElement(m: IR_MatrixExpression, forRow: Integer, forColumn: Integer): IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("matrix of minors for non-quadratic matrices not implemented ")
    }
    val tmp = IR_MatrixExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
    var tmpRow = 0
    for (row <- 0 until m.rows) {
      if (row != forRow) {
        var tmpCol = 0
        for (col <- 0 until m.columns) {
          if (col != forColumn) {
            tmp.set(tmpRow, tmpCol, m.get(row, col))
            tmpCol += 1
          }
        }
        tmpRow += 1
      }
    }
    return calculateDeterminant(tmp)
  }

  def findPivot(matrix: IR_MatrixExpression, startRow: Int): Int = {
    var myMax = (0, 0.0)
    for (row <- startRow until matrix.rows) {
      val myElem = matrix.get(row, startRow)
      myElem match {
        case x: IR_IntegerConstant => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x: IR_RealConstant => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x: IR_FloatConstant => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case _ =>
      }
    }
    if (myMax._2 == 0.0) {
      // it's not constant 0 (or all the other stuff was 0 as well), so let's hope we gonna be fine...
      return startRow
    } else {
      return myMax._1
    }
  }

  def getElem(exp: IR_Expression, row: Integer, col: Integer) = {
    exp match {
      case x: IR_MatrixExpression => x.get(row, col)
      case x: IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row, col)))
      case _ => Logger.error(s"Argument is of unexpected type ${exp.getClass.getTypeName}: $exp")
    }
  }

  def getSingleElem(exp: IR_Expression) = {
    exp match {
      case x: IR_MatrixExpression => x.get(0, 0)
      case x: IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => x
      case _ => Logger.error(s"Argument is of unexpected type ${exp.getClass.getTypeName}: $exp")
    }
  }

  this += new Transformation("resolution of built-in functions 2/2", {

    case call: IR_FunctionCall if (call.name == "dotProduct" || call.name == "dot") =>
      if (call.arguments.length != 2) {
        Logger.error(s"dotProduct() must have two arguments; has ${call.arguments.length}")
      }

      val left = call.arguments(0)
      val right = call.arguments(1)

      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Dot product argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      val rsize = right match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Dot product argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${lsize._1}x${lsize._2} * ${rsize._1}x${rsize._2}")
      }

      var additions = ListBuffer[IR_Expression]()
      if (lsize._1 > 1 || lsize._2 > 1) {
        for (row <- 0 until lsize._1) {
          for (col <- 0 until lsize._2) {
            additions += IR_Multiplication(getElem(left, row, col), getElem(right, row, col))
          }
        }
      } else {
        additions += (getSingleElem(left), getSingleElem(right))
      }
      IR_Addition(additions)

    case call: IR_FunctionCall if call.name == "crossProduct" || call.name == "cross" =>
      if (call.arguments.length != 2) {
        Logger.error(s"Cross product must have two arguments; has ${call.arguments.length}")
      }

      val left = call.arguments(0)
      val right = call.arguments(1)

      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Cross product argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      val rsize = right match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Cross product argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for dotProduct() - attempting ${lsize._1}x${lsize._2} * ${rsize._1}x${rsize._2}")
      }
      lsize._1 match {
        case 2 => getElem(left, 0, 0) * getElem(right, 1, 0) - getElem(left, 1, 0) * getElem(right, 0, 0)
        case 3 => ???
        case other => Logger.error(s"Cross product is not defined for dimensionality $other")
      }


    case call: IR_FunctionCall if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime" && call.name == "inverse") =>
      if (call.arguments.length != 1) {
        Logger.error("inverse() must have one argument, but has " + call.arguments.length + " arguments.")
      }
      call.arguments(0) match {
        case s: IR_Expression if (s.datatype.isInstanceOf[IR_ScalarDatatype]) => 1 / s
        case s: IR_Expression if (s.datatype.isInstanceOf[IR_ComplexDatatype]) => 1 / s
        case m: IR_MatrixExpression => m.inverse
        case _ => Logger.warn("Unable to handle inverse() argument: " + call.arguments(0)); call
      }

    case call: IR_FunctionCall if (call.name == "det") =>
      if (call.arguments.length != 1) {
        Logger.error("det() must have one argument")
      }
      val m = call.arguments(0).asInstanceOf[IR_MatrixExpression]
      calculateDeterminant(m)

    // FIXME shorten this code (less code duplication)
    case IR_ElementwiseMultiplication(left, right) =>
      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      val rsize = right match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for .* - attempting ${lsize._1}x${lsize._2} * ${rsize._1}x${rsize._2}")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Multiplication(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case IR_ElementwiseDivision(left, right) =>
      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      val rsize = right match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for ./ - attempting ${lsize._1}x${lsize._2} * ${rsize._1}x${rsize._2}")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Division(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case IR_ElementwiseModulo(left, right) =>
      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      val rsize = right match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }

      if (lsize != rsize) {
        Logger.warn(left)
        Logger.warn(right)
        Logger.error(s"Matrix sizes must match for .% - attempting ${lsize._1}x${lsize._2} * ${rsize._1}x${rsize._2}")
      }
      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Modulo(getElem(left, row, col), getElem(right, row, col)))
        }
      }
      me

    case IR_ElementwisePower(left, right) =>
      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case other => Logger.error(s"Element-wise operation argument is of unexpected type ${other.getClass.getTypeName}: $other")
      }
      right match {
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => Logger.error(s"Element-wise operation argument is of unexpected type ${va.getClass.getTypeName}: $va")
        case other =>
      }

      val me = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
      for (row <- 0 until lsize._1) {
        for (col <- 0 until lsize._2) {
          me.set(row, col, IR_Power(getElem(left, row, col), right))
        }
      }
      me

    case call: IR_FunctionCall if call.name == "transpose" =>
      if (call.arguments.length != 1) {
        Logger.error(s"Transpose operation must have one arguments; has ${call.arguments.length}")
      }

      val left = call.arguments(0)
      var transform = true

      val lsize = left match {
        case me: IR_MatrixExpression => (me.rows, me.columns)
        case va if (va.datatype.isInstanceOf[IR_MatrixDatatype]) => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
        case _ => transform = false; (0, 0)
      }
      if (transform) {
        val me = IR_MatrixExpression(left.datatype, lsize._2, lsize._1)
        for (row <- 0 until lsize._1) {
          for (col <- 0 until lsize._2) {
            me.set(col, row, getElem(left, row, col))
          }
        }
        me
      } else {
        call
      }

    case call: IR_FunctionCall if call.name == "getRow" =>
      if (call.arguments.length != 2) {
        Logger.error(s"getRow() must have 2 arguments for matrices; has ${call.arguments.length}")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("getRow() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("getRow() may not be used for vectors")
      }

      var expressions = ListBuffer[IR_Expression]()
      for (r <- 0 until m.sizeN) {
        expressions += getElem(call.arguments(0), call.arguments(1).asInstanceOf[IR_IntegerConstant].v.intValue(), r)
      }
      IR_MatrixExpression(Some(m.resolveBaseDatatype), 1, m.sizeN, expressions.toArray)

    case call: IR_FunctionCall if call.name == "getColumn" =>
      if (call.arguments.length != 2) {
        Logger.error(s"getColumn() must have 2 arguments for matrices; has ${call.arguments.length}")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("getColumn() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("getColumn() may not be used for vectors")
      }

      var expressions = ListBuffer[IR_Expression]()
      for (r <- 0 until m.sizeM) {
        expressions += getElem(call.arguments(0), r, call.arguments(1).asInstanceOf[IR_IntegerConstant].v.intValue())
      }
      IR_MatrixExpression(Some(m.resolveBaseDatatype), m.sizeM, 1, expressions.toArray)

    case call: IR_FunctionCall if call.name == "getElement" =>
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("getElement() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      val itm = Array(IR_IntegerConstant(0), IR_IntegerConstant(0))

      if (m.sizeM > 1 && m.sizeN > 1) {
        if (call.arguments.length != 3) {
          Logger.error(s"getElement() must have 3 arguments for matrices; has ${call.arguments.length}")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        itm(1) = call.arguments(2).asInstanceOf[IR_IntegerConstant]
      } else if (m.sizeM == 1 && m.sizeN > 1) {
        if (call.arguments.length != 2) {
          Logger.error(s"getElement() must have 2 arguments for vectors; has ${call.arguments.length}")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
      } else if (m.sizeM > 1 && m.sizeN == 1) {
        if (call.arguments.length != 2) {
          Logger.error(s"getElement() must have 2 arguments for vectors; has ${call.arguments.length}")
        }
        itm(1) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
      }

      var ret: IR_Expression = call

      call.arguments(0) match {
        case x: IR_MatrixExpression => ret = getElem(x, itm(0).asInstanceOf[IR_IntegerConstant].v.intValue(), itm(1).asInstanceOf[IR_IntegerConstant].v.intValue())
        case x@(_: IR_FieldAccess | _: IR_VariableAccess) if x.datatype.isInstanceOf[IR_MatrixDatatype] => ret = IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(itm(0), itm(1)))
        case _ => Logger.error(s"Unhandled argument ${call.arguments(0)} for getElement()")
      }
      ret

    case IR_ExpressionStatement(call: IR_FunctionCall) if call.name == "setRow" =>
      if (call.arguments.length != 3) {
        Logger.error(s"setRow() must have 3 arguments for matrices; has ${call.arguments.length}")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("setRow() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("setRow() may not be used for vectors")
      }
      val v = call.arguments(2)
      if (!v.datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("Argument 3 of setRow() must be vector")
      }

      var stmts = ListBuffer[IR_Statement]()
      for (r <- 0 until m.sizeN) {
        stmts += IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(call.arguments(1), r)), IR_HighDimAccess(v, IR_ExpressionIndex(r)))
      }
      stmts

    case IR_ExpressionStatement(call: IR_FunctionCall) if call.name == "setColumn" =>
      if (call.arguments.length != 3) {
        Logger.error(s"setColumn() must have 3 arguments for matrices; has ${call.arguments.length}")
      }
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("setColumn() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM == 1 || m.sizeN == 1) {
        Logger.error("setColumn() may not be used for vectors")
      }
      val v = call.arguments(2)
      if (!v.datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("Argument 3 of setColumn() must be vector")
      }

      var stmts = ListBuffer[IR_Statement]()
      for (r <- 0 until m.sizeN) {
        stmts += IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(r, call.arguments(1))), IR_HighDimAccess(v, IR_ExpressionIndex(r)))
      }
      stmts

    case IR_ExpressionStatement(call: IR_FunctionCall) if call.name == "setElement" =>
      if (!call.arguments(0).datatype.isInstanceOf[IR_MatrixDatatype]) {
        Logger.error("setElement() may only be used for matrix datatypes")
      }
      val m = call.arguments(0).datatype.asInstanceOf[IR_MatrixDatatype]
      val itm = Array(IR_IntegerConstant(0), IR_IntegerConstant(0))
      var obj: IR_Expression = null
      if (m.sizeM > 1 && m.sizeN > 1) {
        if (call.arguments.length != 4) {
          Logger.error(s"setElement() must have 4 arguments for matrices; has ${call.arguments.length}")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        itm(1) = call.arguments(2).asInstanceOf[IR_IntegerConstant]
        obj = call.arguments(3)
      } else if (m.sizeM == 1 && m.sizeN > 1) {
        if (call.arguments.length != 3) {
          Logger.error(s"setElement() must have 3 arguments for vectors; has ${call.arguments.length}")
        }
        itm(0) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        obj = call.arguments(2)
      } else if (m.sizeM > 1 && m.sizeN == 1) {
        if (call.arguments.length != 3) {
          Logger.error(s"setElement() must have 3 arguments for vectors; has ${call.arguments.length}")
        }
        itm(1) = call.arguments(1).asInstanceOf[IR_IntegerConstant]
        obj = call.arguments(2)
      }
      IR_Assignment(IR_HighDimAccess(call.arguments(0), IR_ExpressionIndex(itm(0), itm(1))), obj)

  })

  if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {

    this += new Transformation("resolve runtime inversion", {
      case IR_ExpressionStatement(call@IR_FunctionCall(_, ListBuffer(in: IR_VariableAccess, out: IR_VariableAccess))) if (call.name == "_runtimeInverseMatrix") =>
        IR_GenerateRuntimeInversion.inverse(in,out)
    })
  }
}

object IR_ResolveMatrixAssignments extends DefaultStrategy("Resolve assignments to matrices") {
  val annotationMatrixRow = "IR_ResolveMatrices.matrixRow"
  val annotationMatrixCol = "IR_ResolveMatrices.matrixCol"

  this += new Transformation("scalarize 1/2", {
    case stmt: IR_VariableDeclaration => stmt

    case IR_Assignment(dest, num: IR_Number, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_MatrixExpression] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_FunctionCall("std::fill", ListBuffer[IR_Expression](Duplicate(dest), Duplicate(dest) + dt.resolveFlattendSize, num)): IR_Statement

    case IR_Assignment(dest, src: IR_VariableAccess, "=") if dest.datatype.isInstanceOf[IR_MatrixDatatype] && !dest.isInstanceOf[IR_MatrixExpression] && src.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val dt = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      IR_FunctionCall("std::copy", ListBuffer[IR_Expression](Duplicate(src), Duplicate(src) + dt.resolveFlattendSize, dest)): IR_Statement

    case stmt@IR_Assignment(dest, _, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      val matrix = dest.datatype.asInstanceOf[IR_MatrixDatatype]
      var newStmts = ListBuffer[IR_Statement]()
      for (row <- 0 until matrix.sizeM) {
        for (col <- 0 until matrix.sizeN) {
          var cloned = Duplicate(stmt)
          StateManager.findAll[IR_Expression](cloned).foreach {
            case _: IR_FunctionArgument => // do not mark function arguments to be resolved into individual accesses
            case x@(_: IR_VariableAccess | _: IR_MatrixExpression | _: IR_MultiDimFieldAccess) if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => {
              x.annotate(annotationMatrixRow, row)
              x.annotate(annotationMatrixCol, col)
            }
            case exp =>
          }
          newStmts += cloned
        }
      }
      newStmts
  })

  this += new Transformation("expressions 2/2", {
    case exp: IR_MatrixExpression if (exp.hasAnnotation(annotationMatrixRow)) =>
      exp.get(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))

    case exp: IR_Expression if (exp.hasAnnotation(annotationMatrixRow)) =>
      IR_HighDimAccess(Duplicate(exp), IR_ConstIndex(Array(exp.popAnnotationAs[Int](annotationMatrixRow), exp.popAnnotationAs[Int](annotationMatrixCol))))
  }, false)
}

object IR_SetupMatrixExpressions extends DefaultStrategy("Convert accesses to matrices and vectors to MatrixExpressions") {
  def duplicateExpressions(access: IR_Expression, dt: IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  this += Transformation("Wrap", {
    case m@IR_MatrixExpression(_, 1, 1) => m.get(0, 0)
    case IR_MatrixDatatype(dt, 1, 1) => dt
    case m: IR_MatrixExpression => m // no need to process further
    case hda: IR_HighDimAccess => hda // no need to process further
    case x: IR_FunctionCall if (x.name != "inverse") => x

    case access@IR_VariableAccess(_, m: IR_MatrixDatatype) if (m.sizeM > 1 || m.sizeN > 1) => IR_MatrixExpression(Some(m.datatype), m.sizeM, m.sizeN, duplicateExpressions(access, m))

    case access: IR_MultiDimFieldAccess if access.datatype.isInstanceOf[IR_MatrixDatatype] =>
      val m = access.datatype.asInstanceOf[IR_MatrixDatatype]
      if (m.sizeM > 1 || m.sizeN > 1)
        IR_MatrixExpression(Some(m.datatype), m.sizeM, m.sizeN, duplicateExpressions(access, m))
      else
        access

    // FIXME: add support for stencil fields
  }, false)
}

object IR_LinearizeMatrices extends DefaultStrategy("Linearize matrices") {
  this += Transformation("Linearize", {
    case IR_HighDimAccess(base, _) if (!base.datatype.isInstanceOf[IR_MatrixDatatype]) => base

    case IR_HighDimAccess(base: IR_MultiDimFieldAccess, idx: IR_Index) =>
      val hoIdx = idx.toExpressionIndex
      val fieldLayout = base.field.layout
      for (dim <- fieldLayout.numDimsGrid until fieldLayout.numDimsData) {
        if (base.index.indices.length <= dim)
          base.index.indices :+= hoIdx(dim - fieldLayout.numDimsGrid)
        else
          base.index.indices(dim) += hoIdx(dim - fieldLayout.numDimsGrid)
      }
      base

    case IR_HighDimAccess(base, idx: IR_ConstIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM > 1 || matrix.sizeN > 1 || idx(0) > 0 || idx(1) > 0)
        IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
      else
        base

    case IR_HighDimAccess(base, idx: IR_ExpressionIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM > 1 || matrix.sizeN > 1)
        IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
      else
        base
  }, false)
}


object IR_BasicMatrixOperations {
  def copySubMatrix(from: IR_MatrixExpression, offset_rows: Int, offset_cols: Int, n_rows: Int, n_cols: Int): IR_MatrixExpression = {
    if (offset_cols < 0 || offset_rows < 0) {
      Logger.error("IR_ResolveMatrixFunctions::copySubMatrix negative offset")
    }
    var submatrix = IR_MatrixExpression(Some(from.datatype.datatype), n_rows, n_cols)
    val bound_cols = offset_cols + n_cols
    val bound_rows = offset_rows + n_rows
    for (i <- offset_rows until bound_rows) {
      for (j <- offset_cols until bound_cols) {
        var n = Duplicate(from.get(i, j))
        submatrix.set(i - offset_rows, j - offset_cols, n)
      }
    }
    submatrix
  }

  def pasteSubMatrix(source: IR_MatrixExpression, target: IR_MatrixExpression, offset_rows: Int, offset_cols: Int): Unit = {
    if (offset_rows + source.rows > target.rows || offset_cols + source.columns > target.columns) {
      //Logger.error("IR_ResolveMatrixFunctions::pasteSubMatrix content does not fit into target")
    }
    if (offset_rows < 0 || offset_cols < 0) {
      Logger.error("IR_ResolveMatrixFunctions::paseSubMatrix negative offset")
    }
    val bound_cols = offset_cols + source.columns
    val bound_rows = offset_rows + source.rows
    for (i <- offset_rows until bound_rows) {
      for (j <- offset_cols until bound_cols) {
        //TODO move node objects instead of copying?
        var n = Duplicate(source.get(i - offset_rows, j - offset_cols))
        target.set(i, j, n)
      }
    }
  }

  def mult(left: IR_MatrixExpression, right: IR_MatrixExpression): IR_MatrixExpression = {
    if (left.columns != right.rows) {
      Logger.error(" Dimensions do not match!")
    }
    var out = IR_MatrixExpression(Some(left.datatype.datatype), left.rows, right.columns)
    for (i <- 0 until left.rows) {
      for (j <- 0 until right.columns) {
        out.set(i, j, IR_RealConstant(0))
        for (k <- 0 until left.columns) {
          var factor_l = Duplicate(left.get(i, k))
          var factor_r = Duplicate(right.get(k, j))
          //out.set(i, j, IR_Addition(Duplicate(out.get(i, j)), IR_Multiplication(factor_l, factor_r)))
          out.set(i, j, IR_Addition(out.get(i, j), IR_Multiplication(factor_l, factor_r)))
        }
      }
    }
    out
  }

  def sub(left: IR_MatrixExpression, right: IR_MatrixExpression): IR_MatrixExpression = {
    if (left.rows != right.rows || left.columns != right.columns) {
      Logger.error(" Dimensions do not match!")
    }
    var out = IR_MatrixExpression(left.innerDatatype, left.rows, left.columns)
    for (i <- 0 until left.rows) {
      for (j <- 0 until left.columns) {
        out.set(i, j, IR_Subtraction(Duplicate(left.get(i, j)), Duplicate(right.get(i, j))))
      }
    }
    out
  }

  def add(left: IR_MatrixExpression, right: IR_MatrixExpression): IR_MatrixExpression = {
    if (left.rows != right.rows || left.columns != right.columns) {
      Logger.error(" Dimensions do not match!")
    }
    var out = IR_MatrixExpression(left.innerDatatype, left.rows, left.columns)
    for (i <- 0 until left.rows) {
      for (j <- 0 until left.columns) {
        out.set(i, j, IR_Addition(Duplicate(left.get(i, j)), Duplicate(right.get(i, j))))
      }
    }
    out
  }

  def negative(that: IR_MatrixExpression): IR_MatrixExpression = {
    var out = IR_MatrixExpression(that.innerDatatype, that.rows, that.columns)
    for (i <- 0 until that.rows) {
      for (j <- 0 until that.columns) {
        //        out.set(i,j,IR_Subtraction(IR_RealConstant(0),Duplicate(that.get(i,j))))
        out.set(i, j, IR_Negative(Duplicate(that.get(i, j))))
      }
    }
    out
  }

  def getElem(exp: IR_Expression, row: Integer, col: Integer) = {
    exp match {
      case x: IR_MatrixExpression => x.get(row, col)
      case x: IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row, col)))
      case _ => Logger.error(s"Argument is of unexpected type ${exp.getClass.getTypeName}: $exp")
    }
  }

  def getSingleElem(exp: IR_Expression) = {
    exp match {
      case x: IR_MatrixExpression => x.get(0, 0)
      case x: IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => x
      case _ => Logger.error(s"Argument is of unexpected type ${exp.getClass.getTypeName}: $exp")
    }
  }

}

object IR_CompiletimeInversion {
  // head function that branches to specific inversions
  def inverse(that: IR_MatrixExpression, matrixStructure: String, blocksize: Int): IR_MatrixExpression = {
    if(that.rows != that.columns)
      Logger.error("inversion of non quadratic matrices not supported.")
    if(blocksize < 1)
      Logger.error("blocksize < 1")
    that.rows match {
      case 1 =>
        IR_MatrixExpression(that.innerDatatype, 1, 1, Array(IR_Division(IR_RealConstant(1.0), that.get(0, 0))))

      case 2 =>
        val a = that.get(0, 0)
        val b = that.get(0, 1)
        val c = that.get(1, 0)
        val d = that.get(1, 1)
        val det: IR_Expression = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
        IR_MatrixExpression(that.innerDatatype, 2, 2, Array(Duplicate(det) * Duplicate(d), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(a)))

      case 3 =>
        val a = that.get(0, 0)
        val b = that.get(0, 1)
        val c = that.get(0, 2)
        val d = that.get(1, 0)
        val e = that.get(1, 1)
        val f = that.get(1, 2)
        val g = that.get(2, 0)
        val h = that.get(2, 1)
        val i = that.get(2, 2)
        val A = Duplicate(e) * Duplicate(i) - Duplicate(f) * Duplicate(h)
        val B = IR_IntegerConstant(-1) * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
        val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
        val D = IR_IntegerConstant(-1) * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
        val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
        val F = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
        val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
        val H = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
        val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
        val det = Duplicate(a) * A + Duplicate(b) * B + Duplicate(c) * C
        IR_MatrixExpression(that.innerDatatype, 3, 3, Array(Duplicate(A) / Duplicate(det), Duplicate(D) / Duplicate(det), Duplicate(G) / Duplicate(det), Duplicate(B) / Duplicate(det), Duplicate(E) / Duplicate(det), Duplicate(H) / Duplicate(det), Duplicate(C) / Duplicate(det), Duplicate(F) / Duplicate(det), Duplicate(I) / Duplicate(det)))

      case _ =>
        // TODO gather and exploit knowledge about matrix structure
        matrixStructure match {
          case "Diagonal"
          => {
            val tmp = Duplicate(that)
            for (row <- 0 until that.rows) {
              //Logger.error("IR_MatrixAccess::inverse: Diagonal element is 0!")
              tmp.set(row, row, IR_Division(IR_RealConstant(1.0), that.get(row, row)))
            }
            tmp
          }
          case "Blockdiagonal"
          => {
            var out = Duplicate(that)
            if (blocksize < 1) {
              Logger.error("Blocksize must be at least 1")
            }
            else {
              val n_blocks = that.rows / blocksize
              if (that.rows % blocksize != 0) {
                Logger.error("Rows are not a multiple of blocksize")
              }
              else {
                for (block <- 0 until n_blocks) {
                  val offset = block * blocksize

                  // extract matrix block
                  var subMatrix = IR_BasicMatrixOperations.copySubMatrix(that, offset, offset, blocksize, blocksize)

                  // invert with GaussJordan method
                  var subMatrix_inv = gaussJordanInverse(subMatrix)

                  // copy to out matrix
                  IR_BasicMatrixOperations.pasteSubMatrix(subMatrix_inv, out, offset, offset)
                }
              }
            }
            out
          }
          case "Schur"
          => {
            //var out = IR_MatrixExpression(that.datatype.datatype, that.rows, that.columns)
            var out = Duplicate(that)
            /* use an invert algorithm using the schur complement

              -1             -1
             M  =  (  A  B  )      =    ( A_inv + A_inv*B*S_inv*C*A_inv -A_inv*B*S_inv  )
                   (  C  D  )           (           -S_inv*C*A_inv           S_inv      )

              with M of size (n + m) x (n + m) and S = D - C * A_inv * B
            */
            val n = blocksize
            val m = that.rows - n

            if (n < 1) {
              Logger.error("IR_MatrixAccess::inverse n < 1!")
            }
            else {
              if ((that.rows - 1) % n != 0) {
                Logger.error("IR_MatrixAccess::inverse Rows of A are not a multiple of n! rows = " + that.rows + ", n = " + n)
              }
              else {
                var blocksize_A = Knowledge.experimental_blocksize_A

                // extract and invert A: Blockdiagonalmatrix assumed
                var A = IR_BasicMatrixOperations.copySubMatrix(that, 0, 0, n, n)
                var A_inv = inverse(A, "Blockdiagonal", blocksize_A)
                IR_GeneralSimplify.doUntilDoneStandalone(A_inv)


                // calculate S
                val B = IR_BasicMatrixOperations.copySubMatrix(that, 0, n, n, m)
                val C = IR_BasicMatrixOperations.copySubMatrix(that, n, 0, m, n)
                val D = IR_BasicMatrixOperations.copySubMatrix(that, n, n, m, m)
                val CA_inv = IR_BasicMatrixOperations.mult(C, A_inv)
                val CA_invB = IR_BasicMatrixOperations.mult(CA_inv, B)
                val S = IR_BasicMatrixOperations.sub(D, CA_invB)

                // invert S
                // for schur complement inversion multiple structure information is necessary(n and m, blocksize of A, S is probably always filled) in case  m is larger than 1 (default should be "Filled")
                val S_inv = inverse(S, "Filled", blocksize_A)

                // copy result blocks to 'out' matrix
                val lowerLeft = IR_BasicMatrixOperations.negative(IR_BasicMatrixOperations.mult(S_inv, CA_inv))
                val lowerRight = S_inv
                val A_invB = IR_BasicMatrixOperations.mult(A_inv, B)
                val A_invBS_inv = IR_BasicMatrixOperations.mult(A_invB,S_inv)
                val upperRight = IR_BasicMatrixOperations.negative(A_invBS_inv)
                val upperLeft = IR_BasicMatrixOperations.add(A_inv, IR_BasicMatrixOperations.mult(A_invBS_inv, CA_inv))
                IR_BasicMatrixOperations.pasteSubMatrix(upperLeft, out, 0, 0)
                IR_BasicMatrixOperations.pasteSubMatrix(upperRight, out, 0, n)
                IR_BasicMatrixOperations.pasteSubMatrix(lowerLeft, out, n, 0)
                IR_BasicMatrixOperations.pasteSubMatrix(lowerRight, out, n, n)
              }
            }
            out
          }
          case "Cofactors"
          => {
            val inv_det = IR_IntegerConstant(1) / IR_ResolveMatrixFunctions.calculateDeterminant(that)
            val tmp = IR_MatrixExpression(Some(that.innerDatatype.getOrElse(IR_RealDatatype)), that.rows, that.columns)
            for (row <- 0 until that.rows) {
              for (col <- 0 until that.columns) {
                tmp.set(col, row, IR_ResolveMatrixFunctions.calculateMatrixOfMinorsElement(that, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
              }
            }
            tmp
          }
          case "GaussJordan"
          => {
            var tmp = gaussJordanInverse(that)
            tmp
          }
          case "Filled"
          => {
            var tmp = gaussJordanInverse(that)
            tmp
          }
          case "Runtime"
          => Logger.error("'Runtime' matrix inversion chosen but in code path for compile time")
          case _ => Logger.error(s"""Unknown matrix inversion resolution strategy "${matrixStructure}"""")
        }

    }
  }

  def gaussJordanInverse(that: IR_MatrixExpression): IR_MatrixExpression = {
    val matrix = Duplicate(that)
    val other = IR_MatrixExpression(matrix.datatype, matrix.rows, matrix.columns)
    for (i <- 0 until other.rows) {
      for (j <- 0 until other.columns) {
        if (i == j) other.set(i, j, 1.0); else other.set(i, j, 0.0)
      }
    }

    for (i <- matrix.rows - 1 to 0) {
      var swap = false
      val topValue = matrix.get(i - 1, i)
      val currentValue = matrix.get(i, i)
      (topValue, currentValue) match {
        case (top: IR_Number, current: IR_Number) => swap = Math.abs(top.value.asInstanceOf[Number].doubleValue) > Math.abs(current.value.asInstanceOf[Number].doubleValue)
        case _ =>
      }

      if (swap) {
        for (j <- 0 until matrix.columns) {
          var d = matrix.get(i, j)
          matrix.set(i, j, matrix.get(i - 1, j))
          matrix.set(i - 1, j, d)
          d = other.get(i, j)
          other.set(i, j, other.get(i - 1, j))
          other.set(i - 1, j, d)
        }
      }
    }

    for (i <- 0 until matrix.rows) {
      for (j <- 0 until matrix.rows) {
        if (j != i) {
          val d = matrix.get(j, i) / matrix.get(i, i)
          for (k <- 0 until matrix.rows) {
            var newExp = matrix.get(j, k) - Duplicate(matrix.get(i, k)) * Duplicate(d)
            matrix.set(j, k, newExp)

            newExp = other.get(j, k) - Duplicate(other.get(i, k)) * Duplicate(d)
            other.set(j, k, newExp)
          }
        }
      }
    }

    IR_GeneralSimplify.doUntilDoneStandalone(matrix)

    for (i <- 0 until matrix.rows) {
      val d = matrix.get(i, i)
      for (j <- 0 until matrix.rows) {
        val newExp = other.get(i, j) / Duplicate(d)
        other.set(i, j, newExp)
      }
    }
    other
  }
}

object IR_GenerateBasicMatrixOperations {

  def printMatrix(matrix: IR_VariableAccess) = {
    val stmts = ListBuffer[IR_Statement]()
    matrix.datatype match {
      case dt: IR_MatrixDatatype =>
        for (i <- 0 until dt.sizeM) {
          for (j <- 0 until dt.sizeN) {
            stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, ListBuffer[IR_Expression](IR_StringConstant("%f "), IR_HighDimAccess(matrix, IR_ConstIndex(i, j)))))
          }
          stmts += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("\\n")))
        }
    }
    stmts
  }

  def mkConstant(dt: IR_Datatype, v: Double) = dt match {
    case IR_RealDatatype => IR_RealConstant(v)
    case IR_IntegerDatatype => IR_IntegerConstant(v.toInt)
    case _ => exastencils.logger.Logger.error("mkConstant not implemented for " + dt.toString)
  }

  // multiply 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def multAtSubmatrix(left: IR_VariableAccess, right: IR_VariableAccess, out: IR_VariableAccess, offset_r: Int, offset_c: Int): IR_Scope = {
    var func = IR_Scope(Nil)
    val leftDt = left.datatype.asInstanceOf[IR_MatrixDatatype]
    val rightDt = right.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    if(leftDt.sizeN != rightDt.sizeM)
      Logger.error("dimensions do not match!")
    val M = leftDt.sizeM
    val K = leftDt.sizeN
    var N = rightDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    var _k = IR_VariableAccess("_k", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_VariableDeclaration(_k)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), 0),
        IR_ForLoop(IR_Assignment(_k, 0), IR_Lower(_k, K), IR_PreIncrement(_k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Multiplication(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _k)), IR_HighDimAccess(right, IR_ExpressionIndex(_k, _j)))))
        ))
      ))
    ))
    func
  }

  // add 'left' and 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def addAtSubmatrix(left: IR_VariableAccess, right: IR_VariableAccess, out: IR_VariableAccess, offset_r: Int, offset_c: Int): IR_Scope = {
    var func = IR_Scope(Nil)
    val leftDt = left.datatype.asInstanceOf[IR_MatrixDatatype]
    val rightDt = right.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    if(leftDt.sizeM != rightDt.sizeM || leftDt.sizeN != rightDt.sizeN)
      Logger.error("dimensions do not match!")
    val M = leftDt.sizeM
    var N = leftDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Addition(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
      ))
    ))
    func
  }

  // subtract 'left' from 'right' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def subAtSubmatrix(left: IR_VariableAccess, right: IR_VariableAccess, out: IR_VariableAccess, offset_r: Int, offset_c: Int): IR_Scope = {
    var func = IR_Scope(Nil)
    val leftDt = left.datatype.asInstanceOf[IR_MatrixDatatype]
    val rightDt = right.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    if(leftDt.sizeM != rightDt.sizeM || leftDt.sizeN != rightDt.sizeN)
      Logger.error("dimensions do not match!")
    val M = leftDt.sizeM
    var N = leftDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Subtraction(IR_HighDimAccess(left, IR_ExpressionIndex(_i, _j)), IR_HighDimAccess(right, IR_ExpressionIndex(_i, _j))))
      ))
    ))
    func
  }

  // produce negative of 'that' and write result to specified place at 'offset_r', 'offset_c' in 'out'
  def negAtSubmatrix(that: IR_VariableAccess, out: IR_VariableAccess, offset_r: Int, offset_c: Int): IR_Scope = {
    var func = IR_Scope(Nil)
    val thatDt = that.datatype.asInstanceOf[IR_MatrixDatatype]
    val outDt = out.datatype.asInstanceOf[IR_MatrixDatatype]
    val M = thatDt.sizeM
    var N = thatDt.sizeN
    var _i = IR_VariableAccess("_i", IR_IntegerDatatype)
    var _j = IR_VariableAccess("_j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(_i)
    func.body += IR_VariableDeclaration(_j)
    func.body += IR_ForLoop(IR_Assignment(_i, 0), IR_Lower(_i, M), IR_PreIncrement(_i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(_j, 0), IR_Lower(_j, N), IR_PreIncrement(_j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(_i + offset_r, _j + offset_c)), IR_Negative(IR_HighDimAccess(that, IR_ExpressionIndex(_i, _j))))
      ))
    ))
    func
  }

  // copy a submatrix of n_rows x n_cols to 'copy' from position offset_r, offset_c in 'source'
  def copySubmatrix(source: IR_VariableAccess, copy: IR_VariableAccess, offset_r: IR_VariableAccess, offset_c: IR_VariableAccess, n_rows: IR_VariableAccess, n_cols: IR_VariableAccess): IR_Scope = {
    var stmts = IR_Scope(Nil)
    val sourceDt = source.datatype.asInstanceOf[IR_MatrixDatatype]
    val copyDt = copy.datatype.asInstanceOf[IR_MatrixDatatype]
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_VariableDeclaration(i)
    stmts.body += IR_VariableDeclaration(j)
    stmts.body += IR_ForLoop(IR_Assignment(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(copy, IR_ExpressionIndex(i - offset_r, j - offset_c)), IR_HighDimAccess(source, IR_ExpressionIndex(i, j)))
      ))
    ))
    stmts
  }

  // write a submatrix 'source' of n_rows x n_cols to 'destination' at position offset_r, offset_c
  def pasteSubmatrix(source: IR_VariableAccess, destination: IR_VariableAccess, offset_r: IR_VariableAccess, offset_c: IR_VariableAccess): IR_Scope = {
    var stmts = IR_Scope(Nil)
    val sourceDt = source.datatype.asInstanceOf[IR_MatrixDatatype]
    val n_rows = sourceDt.sizeM
    val n_cols = sourceDt.sizeN
    val destDt = destination.datatype.asInstanceOf[IR_MatrixDatatype]
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    stmts.body += IR_VariableDeclaration(i)
    stmts.body += IR_VariableDeclaration(j)
    stmts.body += IR_ForLoop(IR_Assignment(i, offset_r), IR_Lower(i, n_rows + offset_r), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, offset_c), IR_Lower(j, offset_c + n_cols), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(destination, IR_ExpressionIndex(i, j)), IR_HighDimAccess(source, IR_ExpressionIndex(i - offset_r, j - offset_c)))
      ))
    ))
    stmts
  }

  // test basic matrix operation generation necessary for schur complement: ignores input matrix of invert call
  def testAll(in: IR_VariableAccess, out: IR_VariableAccess): IR_Scope = {
    var func = IR_Scope(Nil)

    var left = IR_VariableAccess("left", IR_MatrixDatatype(IR_IntegerDatatype, 4, 4))
    var right = IR_VariableAccess("right", IR_MatrixDatatype(IR_IntegerDatatype, 4, 4))
    var out = IR_VariableAccess("out", IR_MatrixDatatype(IR_IntegerDatatype, 4, 4))
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(left)
    func.body += IR_VariableDeclaration(right)
    func.body += IR_VariableDeclaration(i)
    func.body += IR_VariableDeclaration(j)
    func.body += IR_VariableDeclaration(out)
    func.body += IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, 4), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(j, 0), IR_Lower(j, 4), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(left, IR_ExpressionIndex(i, j)), i * 4 + j),
        IR_Assignment(IR_HighDimAccess(right, IR_ExpressionIndex(i, j)), i * 4 + j)
      ))
    ))
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] Input matrix 'left'\\n")))
    func.body ++= printMatrix(left)
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] Input matrix 'right'\\n")))
    func.body ++= printMatrix(right)

    // mult
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] calculate product:\\n")))
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(left, right, out, 0, 0)
    func.body ++= printMatrix(out)

    // add
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] calculate sum(add):\\n")))
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(left, right, out, 0, 0)
    func.body ++= printMatrix(out)

    // sub
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] calculate sum(sub):\\n")))
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(left, right, out, 0, 0)
    func.body ++= printMatrix(out)

    // neg
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] calculate negative:\\n")))
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(left, out, 0, 0)
    func.body ++= printMatrix(out)

    // copy submatrix
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] copy submatrix from 'left' at offset_rows = 1, offset_cols = 2, n_rows = 1, n_cols = 2:\\n")))
    var offset_r = IR_VariableAccess("offset_r", IR_IntegerDatatype)
    var offset_c = IR_VariableAccess("offset_c", IR_IntegerDatatype)
    var n_rows = IR_VariableAccess("n_rows", IR_IntegerDatatype)
    var n_cols = IR_VariableAccess("n_cols", IR_IntegerDatatype)
    func.body += IR_VariableDeclaration(offset_r)
    func.body += IR_VariableDeclaration(offset_c)
    func.body += IR_VariableDeclaration(n_rows)
    func.body += IR_VariableDeclaration(n_cols)
    func.body += IR_Assignment(offset_r, 1)
    func.body += IR_Assignment(offset_c, 2)
    func.body += IR_Assignment(n_rows, 1)
    func.body += IR_Assignment(n_cols, 2)
    var copy = IR_VariableAccess("submatrix", IR_MatrixDatatype(IR_IntegerDatatype, 1, 2))
    func.body += IR_VariableDeclaration(copy)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(left, copy, offset_r, offset_c, n_rows, n_cols)
    func.body ++= printMatrix(copy)
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] copy submatrix from 'left' at offset_rows = 1, offset_cols = 1, n_rows = 3, n_cols = 2:\\n")))
    func.body += IR_Assignment(offset_r, 1)
    func.body += IR_Assignment(offset_c, 1)
    func.body += IR_Assignment(n_rows, 3)
    func.body += IR_Assignment(n_cols, 2)
    var copy2 = IR_VariableAccess("submatrix2", IR_MatrixDatatype(IR_IntegerDatatype, 3, 2))
    func.body += IR_VariableDeclaration(copy2)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(left, copy2, offset_r, offset_c, n_rows, n_cols)
    func.body ++= printMatrix(copy2)

    // paste submatrix
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] write matrix at offset_rows = 1, offset_cols = 2, n_rows = 3, n_cols = 1 to right:\\n")))
    var mat = IR_VariableAccess("mat", IR_MatrixDatatype(IR_IntegerDatatype, 3, 1))
    func.body += IR_VariableDeclaration(mat)
    func.body ++= printMatrix(mat)
    func.body += IR_Assignment(offset_c, 2)
    func.body += IR_GenerateBasicMatrixOperations.pasteSubmatrix(mat, right, offset_r, offset_c)
    func.body ++= printMatrix(right)
    // paste submatrix
    func.body += (IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[Test] write matrix at offset_rows = 1, offset_cols = 1, n_rows = 3, n_cols = 3 to right:\\n")))
    var mat2 = IR_VariableAccess("mat2", IR_MatrixDatatype(IR_IntegerDatatype, 3, 3))
    func.body += IR_VariableDeclaration(mat2)
    func.body ++= printMatrix(mat2)
    func.body += IR_Assignment(offset_c, 1)
    func.body += IR_GenerateBasicMatrixOperations.pasteSubmatrix(mat2, right, offset_r, offset_c)
    func.body ++= printMatrix(right)
    func
  }
}

object IR_GenerateRuntimeInversion {
  /*
  def runtimeInverseLU(in: IR_VariableAccess, out: IR_VariableAccess) = {

    val debug = false

    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]

    val N = inDt.sizeM
    val myType = inDt.resolveBaseDatatype
    val func = IR_Scope(Nil)
    val myL = IR_VariableAccess("_L", IR_MatrixDatatype(myType, N, N))
    val myU = IR_VariableAccess("_U", IR_MatrixDatatype(myType, N, N))
    val myQ = IR_VariableAccess("_q", IR_ArrayDatatype(IR_IntegerDatatype, N))
    val myI = IR_VariableAccess("_i", IR_IntegerDatatype)
    val myJ = IR_VariableAccess("_j", IR_IntegerDatatype)
    val myK = IR_VariableAccess("_k", IR_IntegerDatatype)

    func.body += IR_VariableDeclaration(myL)
    func.body += IR_VariableDeclaration(myU)
    func.body += IR_Assignment(myL, IR_GenerateBasicMatrixOperations.mkConstant(myType, 0))
    func.body += IR_Assignment(myU, in)
    func.body += IR_VariableDeclaration(myQ)
    func.body += IR_Assignment(out, IR_GenerateBasicMatrixOperations.mkConstant(myType, 0))
    for (i <- 0 until N) {
      func.body += IR_Assignment(IR_HighDimAccess(myL, IR_ConstIndex(i, i)), IR_GenerateBasicMatrixOperations.mkConstant(myType, 1))
    }
    for (i <- 0 until N) {
      func.body += IR_Assignment(IR_ArrayAccess(myQ, IR_IntegerConstant(i)), IR_IntegerConstant(i))
    }
    val myColmax = IR_VariableAccess("_colmax", myType)
    val myMaxCol = IR_VariableAccess("_maxCol", IR_IntegerDatatype)
    func.body += IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(0)), IR_Lower(myK, IR_IntegerConstant(N - 1)), IR_ExpressionStatement(IR_PreIncrement(myK)), ListBuffer[IR_Statement](
      IR_VariableDeclaration(myColmax, IR_RealConstant(0)),
      IR_VariableDeclaration(myMaxCol, myK),
      IR_ForLoop(IR_VariableDeclaration(myJ, myK), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
        IR_IfCondition(IR_Greater(IR_FunctionCall(IR_MathFunctionReference.fabs, IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ))), myColmax), ListBuffer[IR_Statement](
          IR_Assignment(myColmax, IR_FunctionCall(IR_MathFunctionReference.fabs, IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ)))),
          IR_Assignment(myMaxCol, myJ)
        ))
      )),
      if (debug) IR_IfCondition(IR_Lower(myColmax, IR_GenerateBasicMatrixOperations.mkConstant(myType, 1e-15)),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[WARNING] Inverting potentially singular matrix\\n"))) +:
          IR_GenerateBasicMatrixOperations.printMatrix(myU)
      ) else IR_NullStatement,
      IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_ArrayAccess(myQ, myK), IR_ArrayAccess(myQ, myMaxCol)))
    ))
    for (i <- 0 until N) {
      func.body.last.asInstanceOf[IR_ForLoop].body += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_HighDimAccess(myL, IR_ExpressionIndex(IR_IntegerConstant(i), myK)), IR_HighDimAccess(myL, IR_ExpressionIndex(IR_IntegerConstant(i), myMaxCol))))
    }
    for (i <- 0 until N) {
      func.body.last.asInstanceOf[IR_ForLoop].body += IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::swap"), IR_HighDimAccess(myU, IR_ExpressionIndex(IR_IntegerConstant(i), myK)), IR_HighDimAccess(myU, IR_ExpressionIndex(IR_IntegerConstant(i), myMaxCol))))
    }
    func.body.last.asInstanceOf[IR_ForLoop].body += IR_ForLoop(IR_VariableDeclaration(myI, myK + IR_IntegerConstant(1)), IR_Lower(myI, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myI)), ListBuffer[IR_Statement](
      IR_Assignment(IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)), IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myK)) / IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myK))),
      IR_ForLoop(IR_VariableDeclaration(myJ, myK), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myJ)), IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myJ)) - IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(myU, IR_ExpressionIndex(myK, myJ)))
      ))
    ))
    val myY = IR_VariableAccess("_y", IR_MatrixDatatype(myType, N, N))
    val mySum = IR_VariableAccess("_sum", myType)
    func.body += IR_VariableDeclaration(myY)
    func.body += IR_ForLoop(IR_VariableDeclaration(myJ, IR_IntegerConstant(0)), IR_Lower(myJ, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myJ)), ListBuffer[IR_Statement](
      IR_ForLoop(IR_VariableDeclaration(myI, IR_IntegerConstant(0)), IR_Lower(myI, IR_IntegerConstant(N)), IR_ExpressionStatement(IR_PreIncrement(myI)), ListBuffer[IR_Statement](
        IR_VariableDeclaration(mySum, IR_RealConstant(0)),
        IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(0)), IR_Lower(myK, myI), IR_ExpressionStatement(IR_PreIncrement(myK)), ListBuffer[IR_Statement](
          IR_Assignment(mySum, mySum + IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(myY, IR_ExpressionIndex(myK, myJ)))
        )),
        IR_IfCondition(IR_EqEq(myI, myJ),
          IR_Assignment(IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)), (IR_GenerateBasicMatrixOperations.mkConstant(myType, 1) - mySum) / IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myI))),
          IR_Assignment(IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)), (IR_GenerateBasicMatrixOperations.mkConstant(myType, 0) - mySum) / IR_HighDimAccess(myL, IR_ExpressionIndex(myI, myI)))
        )
      )),
      IR_ForLoop(IR_VariableDeclaration(myI, IR_IntegerConstant(N) - IR_IntegerConstant(1)), IR_GreaterEqual(myI, IR_IntegerConstant(0)), IR_ExpressionStatement(IR_PreDecrement(myI)), ListBuffer[IR_Statement](
        IR_VariableDeclaration(mySum, IR_RealConstant(0)),
        IR_ForLoop(IR_VariableDeclaration(myK, IR_IntegerConstant(N) - IR_IntegerConstant(1)), IR_Greater(myK, myI), IR_ExpressionStatement(IR_PreDecrement(myK)), ListBuffer[IR_Statement](
          IR_Assignment(mySum, mySum + IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myK)) * IR_HighDimAccess(out, IR_ExpressionIndex(myK, myJ)))
        )),
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(myI, myJ)), (IR_HighDimAccess(myY, IR_ExpressionIndex(myI, myJ)) - mySum) / IR_HighDimAccess(myU, IR_ExpressionIndex(myI, myI)))
      ))
    ))
    func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)
    func
  }
  */

  // direct inversion for small matrices
  def smallMatrixInversion(in: IR_VariableAccess, out: IR_VariableAccess): IR_Scope = {
    var debug = false

    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    //var stmts = IR_Scope(Nil)
    var stmts = ListBuffer[IR_Statement]()
    val M = inDt.sizeM
    M match {
      case 1 =>
        stmts += IR_Assignment(out, IR_Division(IR_RealConstant(1), IR_BasicMatrixOperations.getElem(in, 0, 0)))

      case 2 =>
        val a = IR_BasicMatrixOperations.getElem(in, 0, 0)
        val b = IR_BasicMatrixOperations.getElem(in, 0, 1)
        val c = IR_BasicMatrixOperations.getElem(in, 1, 0)
        val d = IR_BasicMatrixOperations.getElem(in, 1, 1)
        val det = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 0)), Duplicate(det) * Duplicate(d))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 1)), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 0)), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 1)), Duplicate(det) * Duplicate(a))

      case 3 =>
        val a = IR_BasicMatrixOperations.getElem(in, 0, 0)
        val b = IR_BasicMatrixOperations.getElem(in, 0, 1)
        val c = IR_BasicMatrixOperations.getElem(in, 0, 2)
        val d = IR_BasicMatrixOperations.getElem(in, 1, 0)
        val e = IR_BasicMatrixOperations.getElem(in, 1, 1)
        val f = IR_BasicMatrixOperations.getElem(in, 1, 2)
        val g = IR_BasicMatrixOperations.getElem(in, 2, 0)
        val h = IR_BasicMatrixOperations.getElem(in, 2, 1)
        val i = IR_BasicMatrixOperations.getElem(in, 2, 2)
        val A = Duplicate(e) * Duplicate(i) - Duplicate(f) * Duplicate(h)
        val B = IR_IntegerConstant(-1) * (Duplicate(d) * Duplicate(i) - Duplicate(f) * Duplicate(g))
        val C = Duplicate(d) * Duplicate(h) - Duplicate(e) * Duplicate(g)
        val D = IR_IntegerConstant(-1) * (Duplicate(b) * Duplicate(i) - Duplicate(c) * Duplicate(h))
        val E = Duplicate(a) * Duplicate(i) - Duplicate(c) * Duplicate(g)
        val F = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(h) - Duplicate(b) * Duplicate(g))
        val G = Duplicate(b) * Duplicate(f) - Duplicate(c) * Duplicate(e)
        val H = IR_IntegerConstant(-1) * (Duplicate(a) * Duplicate(f) - Duplicate(c) * Duplicate(d))
        val I = Duplicate(a) * Duplicate(e) - Duplicate(b) * Duplicate(d)
        val det = Duplicate(a) * Duplicate(A) + Duplicate(b) * Duplicate(B) + Duplicate(c) * Duplicate(C)
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 0)), Duplicate(A) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 1)), Duplicate(D) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(0, 2)), Duplicate(G) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 0)), Duplicate(B) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 1)), Duplicate(E) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(1, 2)), Duplicate(H) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(2, 0)), Duplicate(C) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(2, 1)), Duplicate(F) / Duplicate(det))
        stmts += IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(2, 2)), Duplicate(I) / Duplicate(det))
    }
    stmts ++= IR_GenerateBasicMatrixOperations.printMatrix(out)
    IR_Scope(stmts)
  }

  // give a invert algorithm for diagonal matrices
  def diagonal(in: IR_VariableAccess, out: IR_VariableAccess): IR_Scope = {
    var debug = true
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    val inner = inDt.resolveBaseDatatype
    var func = IR_Scope(Nil)
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var tmp = IR_VariableAccess("tmp", inner)

    func.body += IR_VariableDeclaration(tmp)
    func.body += IR_VariableDeclaration(i)
    func.body += IR_Assignment(i, IR_IntegerConstant(0))
    func.body += IR_WhileLoop(IR_Lower(i, IR_IntegerConstant(N)), ListBuffer[IR_Statement](
      IR_Assignment(tmp, IR_HighDimAccess(in, IR_ExpressionIndex(i, i))),
      IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(IR_Addition(IR_Multiplication(i, N), i))), IR_Division(IR_DoubleConstant(1), tmp)),
      IR_Assignment(i, IR_Addition(i, IR_IntegerConstant(1)))
    ))

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  // generate a LU decomposition for a submatrix at 'offset_r','offset_c' of 'in' inplace
  def localLUDecomp(in: IR_VariableAccess, P: IR_VariableAccess, blocksize_asInt: Int, offset_r: IR_VariableAccess, offset_c: IR_VariableAccess): ListBuffer[IR_Statement] = {
    val debug = false

    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val Tol = IR_RealConstant(0.0001)
    val baseType = inDt.resolveBaseDatatype
    val func = ListBuffer[IR_Statement]()
    val i = IR_VariableAccess("i", IR_IntegerDatatype)
    val j = IR_VariableAccess("j", IR_IntegerDatatype)
    val k = IR_VariableAccess("k", IR_IntegerDatatype)
    val imax = IR_VariableAccess("imax", IR_IntegerDatatype)
    val maxA = IR_VariableAccess("maxA", baseType)
    val absA = IR_VariableAccess("absA", baseType)
    val tmp_row = IR_VariableAccess("tmp_row", IR_ArrayDatatype(baseType, blocksize_asInt))

    func += IR_VariableDeclaration(i)
    func += IR_VariableDeclaration(j)
    func += IR_VariableDeclaration(k)
    func += IR_VariableDeclaration(tmp_row)
    func += IR_VariableDeclaration(imax)
    func += IR_VariableDeclaration(maxA)
    func += IR_VariableDeclaration(absA)
    func += IR_ForLoop(IR_Assignment(i, IR_IntegerConstant(0)), IR_Lower(i, blocksize_asInt + 1), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(IR_ArrayAccess(P, i), i)
    ))
    func += IR_ForLoop(IR_Assignment(i, IR_IntegerConstant(0)), IR_Lower(i, blocksize_asInt), IR_ExpressionStatement(IR_PreIncrement(i)), ListBuffer[IR_Statement](
      IR_Assignment(maxA, IR_RealConstant(0)),
      IR_Assignment(imax, i),
      IR_ForLoop(IR_Assignment(k, i), IR_Lower(k, blocksize_asInt), IR_ExpressionStatement(IR_PreIncrement(k)), ListBuffer[IR_Statement](
        IR_Assignment(absA, IR_FunctionCall(IR_ExternalFunctionReference.fabs, ListBuffer[IR_Expression](IR_HighDimAccess(in, IR_ExpressionIndex(k + offset_r, i + offset_c))))),
        IR_IfCondition(IR_Greater(absA, maxA), ListBuffer[IR_Statement](IR_Assignment(maxA, absA), IR_Assignment(imax, k)), ListBuffer[IR_Statement]())
      )),
      IR_IfCondition(IR_Lower(maxA, Tol), ListBuffer[IR_Statement](IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference.printf, IR_StringConstant("[WARNING] Inverting potentially singular matrix\\n"))), IR_Return(IR_IntegerConstant(-1)))),
      IR_IfCondition(IR_Neq(imax, i), ListBuffer[IR_Statement](
        IR_Assignment(j, IR_ArrayAccess(P, i)),
        IR_Assignment(IR_ArrayAccess(P, i), IR_ArrayAccess(P, imax)),
        IR_Assignment(IR_ArrayAccess(P, imax), j),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, 0 + offset_c))), IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference("std::memcpy"), ListBuffer[IR_Expression](IR_AddressOf(IR_HighDimAccess(in, IR_ExpressionIndex(imax + offset_r, 0 + offset_c))), IR_AddressOf(IR_ArrayAccess(tmp_row, 0)), blocksize_asInt * IR_SizeOf(baseType)))),
        IR_PostIncrement(IR_ArrayAccess(P, blocksize_asInt + 1))
      )),
      IR_ForLoop(IR_Assignment(j, i + 1), IR_Lower(j, blocksize_asInt), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_Division(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, i + offset_c)))),
        IR_ForLoop(IR_Assignment(k, i + 1), IR_Lower(k, blocksize_asInt), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, k + offset_c)), IR_Subtraction(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, k + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(j + offset_r, i + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)))))
        ))
      ))
    ))
    func
  }

  // generate an inverted matrix for a submatrix at 'offset_r','offset_c' if submatrix('in') is LU decomposed
  def localLUDecompedInversion(in: IR_VariableAccess, P: IR_VariableAccess, blocksize: Int, offset_r: IR_VariableAccess, offset_c: IR_VariableAccess, out: IR_VariableAccess): ListBuffer[IR_Statement] = {
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var k = IR_VariableAccess("k", IR_IntegerDatatype)
    var func = ListBuffer[IR_Statement]()
    func += IR_ForLoop(IR_Assignment(j, 0), IR_Lower(j, blocksize), IR_PreIncrement(j), ListBuffer[IR_Statement](
      IR_ForLoop(IR_Assignment(i, 0), IR_Lower(i, blocksize), IR_PreIncrement(i), ListBuffer[IR_Statement](
        IR_IfCondition(IR_EqEq(IR_ArrayAccess(P, i), j), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), 1.0)
        ), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), 0.0)
        )),
        IR_ForLoop(IR_Assignment(k, 0), IR_Lower(k, i), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Subtraction(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)), IR_HighDimAccess(out, IR_ExpressionIndex(k + offset_r, j + offset_c)))))
        ))
      )),
      IR_ForLoop(IR_Assignment(i, blocksize - 1), IR_GreaterEqual(i, 0), IR_PostDecrement(i), ListBuffer[IR_Statement](
        IR_ForLoop(IR_Assignment(k, i + 1), IR_Lower(k, blocksize), IR_PreIncrement(k), ListBuffer[IR_Statement](
          IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Subtraction(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Multiplication(IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, k + offset_c)), IR_HighDimAccess(out, IR_ExpressionIndex(k + offset_r, j + offset_c)))))
        )),
        IR_Assignment(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_Division(IR_HighDimAccess(out, IR_ExpressionIndex(i + offset_r, j + offset_c)), IR_HighDimAccess(in, IR_ExpressionIndex(i + offset_r, i + offset_c))))
      ))
    ))
    func
  }

  // combines LU decomposition and inversion of submatrix of 'in' at 'offset_r', 'offset_c' of size 'blocksize'
  def localLUInversion(in: IR_VariableAccess, blocksize_asInt: Int, offset_r: IR_VariableAccess, offset_c: IR_VariableAccess, out: IR_VariableAccess): IR_Scope = {
    var func = IR_Scope(Nil)
    var P = IR_VariableAccess("P", IR_ArrayDatatype(IR_IntegerDatatype, blocksize_asInt + 1))
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize_asInt != 0) Logger.error("IR_ResolveMatrixFunctions::localLUInversion: Matrices with size not mutliple of blocksize not implemented yet")
    func.body += IR_VariableDeclaration(P)
    func.body ++= localLUDecomp(in, P, blocksize_asInt, offset_r, offset_c)
    func.body ++= localLUDecompedInversion(in, P, blocksize_asInt, offset_r, offset_c, out)
    func
  }

  // give an invert algorithm for blockdiagonal matrices
  def blockdiagonal(in: IR_VariableAccess, blocksize: Int, out: IR_VariableAccess): IR_Scope = {
    var debug = true
    var func = IR_Scope(Nil)
    var block = IR_VariableAccess("block", IR_IntegerDatatype)
    val inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    val N = inDt.sizeM
    if (N % blocksize != 0) Logger.error("IR_ResolveMatrixFunctions::runtimeInverseBlockdiagonal: Matrices with size not mutliple of blocksize not implemented yet")
    func.body += IR_VariableDeclaration(block)
    func.body += IR_ForLoop(IR_Assignment(block, 0), IR_Lower(block, N), IR_Assignment(block, IR_Addition(block, blocksize)), ListBuffer[IR_Statement](
    ) += localLUInversion(in, blocksize, block, block, out))

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  /* give an invert algorithm using the schur complement

    -1             -1
   M  =  (  A  B  )      =    ( A_inv + A_inv*B*S_inv*C*A_inv -A_inv*B*S_inv  )
         (  C  D  )           (           -S_inv*C*A_inv           S_inv      )

             with M of size (n + m) x (n + m) and S = D - C * A_inv * B
 */
  def schur(in: IR_VariableAccess, blocksize: Int, out: IR_VariableAccess): IR_Scope = {
    var debug = true

    var func = IR_Scope(Nil)
    var inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    var baseType = inDt.resolveBaseDatatype
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var offset_r = IR_VariableAccess("offset_r", IR_IntegerDatatype)
    var offset_c = IR_VariableAccess("offset_c", IR_IntegerDatatype)
    var n = IR_VariableAccess("n", IR_IntegerDatatype)
    var m = IR_VariableAccess("m", IR_IntegerDatatype)
    var n_asInt = blocksize
    var m_asInt = inDt.sizeM - blocksize
    var A = IR_VariableAccess("A", IR_MatrixDatatype(baseType, n_asInt, n_asInt))
    var A_inv = IR_VariableAccess("A_inv", IR_MatrixDatatype(baseType, n_asInt, n_asInt))
    var B = IR_VariableAccess("B", IR_MatrixDatatype(baseType, n_asInt, m_asInt))
    var C = IR_VariableAccess("C", IR_MatrixDatatype(baseType, m_asInt, n_asInt))
    var D = IR_VariableAccess("D", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var S = IR_VariableAccess("S", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var S_inv = IR_VariableAccess("S_inv", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var CA_inv = IR_VariableAccess("CA_inv", IR_MatrixDatatype(baseType, m_asInt, n_asInt))
    var CA_invB = IR_VariableAccess("CA_invB", IR_MatrixDatatype(baseType, m_asInt, m_asInt))
    var A_invB = IR_VariableAccess("A_invB", IR_MatrixDatatype(baseType, n_asInt, m_asInt))
    var A_invBS_inv = IR_VariableAccess("A_invBS_inv", IR_MatrixDatatype(baseType, n_asInt, m_asInt))
    var S_invCA_inv = IR_VariableAccess("S_invCA_inv", IR_MatrixDatatype(baseType, m_asInt, n_asInt))
    var A_invBS_invCA_inv = IR_VariableAccess("A_invBS_invCA_inv", IR_MatrixDatatype(baseType, n_asInt, n_asInt))

    func.body += IR_VariableDeclaration(offset_r)
    func.body += IR_VariableDeclaration(offset_c)
    func.body += IR_Assignment(offset_r, 0)
    func.body += IR_Assignment(offset_c, 0)
    func.body += IR_VariableDeclaration(n)
    func.body += IR_Assignment(n, blocksize)
    func.body += IR_VariableDeclaration(m)
    func.body += IR_Assignment(m, inDt.sizeM - blocksize)

    // copy A and invert
    //TODO use algorithm that exploits structure -> receive matrix structure information from classifier -> e.g. blockdiagonal
    // blocksize of the diagonal blocks of A if A is a blockdiagonal matrix -> later this information comes from the classifyer?
    val blocksize_A = Knowledge.experimental_blocksize_A
    func.body += IR_VariableDeclaration(A)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, A, offset_r, offset_c, n, n)
    func.body += IR_VariableDeclaration(A_inv)
    //func.body += GenerateRuntimeInversion.blockdiagonal(A, blocksize_A, A_inv)
    //func.body += GenerateRuntimeInversion.runtimeInverseLU(A,A_inv)
    func.body += IR_GenerateRuntimeInversion.localLUInversion(A, n_asInt, offset_r, offset_c, A_inv)

    // copy B
    func.body += IR_VariableDeclaration(B)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, B, offset_r, n, n, m)

    // copy C
    func.body += IR_VariableDeclaration(C)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, C, n, offset_c, m, n)

    // copy D
    func.body += IR_VariableDeclaration(D)
    func.body += IR_GenerateBasicMatrixOperations.copySubmatrix(in, D, n, n, m, m)

    // calculate S
    func.body += IR_VariableDeclaration(S)
    func.body += IR_VariableDeclaration(CA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(C, A_inv, CA_inv, 0, 0)
    func.body += IR_VariableDeclaration(CA_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(CA_inv, B, CA_invB, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.subAtSubmatrix(D, CA_invB, S, 0, 0)

    // calculate S_inv
    func.body += IR_VariableDeclaration(S_inv)
    func.body += IR_GenerateRuntimeInversion.localLUInversion(S, m_asInt, offset_r, offset_c, S_inv)

    // calculate upper right result block
    func.body += IR_VariableDeclaration(A_invB)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_inv, B, A_invB, 0, 0)
    func.body += IR_VariableDeclaration(A_invBS_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_inv, A_invBS_inv, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(A_invBS_inv, out, 0, n_asInt)

    // insert lower right result block
    func.body += IR_GenerateBasicMatrixOperations.pasteSubmatrix(S_inv, out, n, n)

    // calculate lower left result block
    func.body += IR_VariableDeclaration(S_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(S_inv, CA_inv, S_invCA_inv, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.negAtSubmatrix(S_invCA_inv, out, n_asInt, 0)

    // calculate upper left result block
    func.body += IR_VariableDeclaration(A_invBS_invCA_inv)
    func.body += IR_GenerateBasicMatrixOperations.multAtSubmatrix(A_invB, S_invCA_inv, A_invBS_invCA_inv, 0, 0)
    func.body += IR_GenerateBasicMatrixOperations.addAtSubmatrix(A_inv, A_invBS_invCA_inv, out, 0, 0)

    if (debug)
      func.body ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

    func
  }

  // head function that branches to specific inversions
  def inverse(in:IR_VariableAccess, out:IR_VariableAccess): IR_Scope = {

    var inDt = in.datatype.asInstanceOf[IR_MatrixDatatype]
    if(inDt.sizeM != inDt.sizeN)
        Logger.error("IR_GenerateMatrixInversion::inverse: inversion of matrices of size " + inDt.sizeM + "," + inDt.sizeN + " not supported")

    if(inDt.sizeN < 4) {
      smallMatrixInversion(in,out)
    } else if(Knowledge.experimental_matrixStructure == "Filled") {
      var debug = true
      //TODO maybe overload GenerateRuntimeInversion methods or 0-access-constant
      var stmts = ListBuffer[IR_Statement]()
      var offsetIsZero = IR_VariableAccess("zero", IR_IntegerDatatype)
      stmts += IR_VariableDeclaration(offsetIsZero, 0)

      // use localLUInversion for the full matrix
      stmts += localLUInversion(in, inDt.sizeN, offsetIsZero, offsetIsZero, out)

      if (debug)
        stmts ++= IR_GenerateBasicMatrixOperations.printMatrix(out)

      IR_Scope(stmts)

    } else if (Knowledge.experimental_matrixStructure == "Diagonal") {
      diagonal(in, out)
    } else if (Knowledge.experimental_matrixStructure == "Blockdiagonal") {
      blockdiagonal(in, Knowledge.experimental_blocksize, out)
    } else if (Knowledge.experimental_matrixStructure == "Schur") {
      schur(in, Knowledge.experimental_blocksize, out)
    } else if (Knowledge.experimental_matrixStructure == "Test") {
      IR_GenerateBasicMatrixOperations.testAll(in, out)
    } else
      Logger.error("IR_ResolveMatrixFunctions::resolve runtime inversion: unknown runtimeInverse resolve")
  }
}

