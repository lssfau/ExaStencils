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

/// IR_TensorExpression
object IR_TensorExpression {
  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_TensorExpression = new IR_TensorExpression(innerDatatype, rows, columns)
  def apply(innerDatatype : IR_Datatype, rows : Integer, columns : Integer) : IR_TensorExpression = new IR_TensorExpression(Some(innerDatatype), rows, columns)
  def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer, expressions : Array[IR_Expression]) : IR_TensorExpression = {
    val tmp = new IR_TensorExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }
  def apply(innerDatatype : Option[IR_Datatype], expressions : ListBuffer[ListBuffer[IR_Expression]]) : IR_TensorExpression = {
    val rows = expressions.size
    val columns = expressions(0).size
    val tmp = new IR_TensorExpression(innerDatatype, rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        tmp.set(row, col, expressions(row)(col))
      }
    }
    tmp
  }
  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_TensorExpression = {
    val tmp = IR_TensorExpression(datatype.datatype, datatype.sizeM, datatype.sizeN)
    tmp.expressions = expressions.toArray
    tmp
  }

  def fromSingleExpression(innerDatatype : IR_Datatype, rows : Integer, columns : Integer, expression : IR_Expression) : IR_TensorExpression = {
    val tmp = new IR_TensorExpression(Some(innerDatatype), rows, columns)
    for (i <- 0 until rows * columns)
      tmp.expressions(i) = Duplicate(expression)
    tmp
  }
}

case class IR_TensorExpression(var innerDatatype : Option[IR_Datatype], var rows : Int, var columns : Int) extends IR_Expression {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    innerDatatype match {
      case None                         =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt : IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _                            =>
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "__matrix_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << '_' << rows << "_" << columns << "_t "
    prettyprintInner(out)
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])
  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])
  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])
  def get(row : Integer, column : Integer) = expressions(row * columns + column)
  def set(row : Integer, column : Integer, exp : IR_Expression) = expressions(row * columns + column) = exp
  override def toString : String = { "IR_TensorExpression(" + innerDatatype + ", " + rows + ", " + columns + "; Items: " + expressions.mkString(", ") + ")" }
}
  /*
  def inverse : IR_TensorExpression = {
    rows match {
      case 1 =>
        IR_TensorExpression(innerDatatype, 1, 1, Array(IR_Division(IR_RealConstant(1.0), get(0, 0))))

      case 2 =>
        val a = get(0, 0)
        val b = get(0, 1)
        val c = get(1, 0)
        val d = get(1, 1)
        val det : IR_Expression = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
        IR_TensorExpression(innerDatatype, 2, 2, Array(Duplicate(det) * Duplicate(d), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(a)))

      case 3 =>
        val a = get(0, 0)
        val b = get(0, 1)
        val c = get(0, 2)
        val d = get(1, 0)
        val e = get(1, 1)
        val f = get(1, 2)
        val g = get(2, 0)
        val h = get(2, 1)
        val i = get(2, 2)
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
        IR_TensorExpression(innerDatatype, 3, 3, Array(Duplicate(A) / Duplicate(det), Duplicate(D) / Duplicate(det), Duplicate(G) / Duplicate(det), Duplicate(B) / Duplicate(det), Duplicate(E) / Duplicate(det), Duplicate(H) / Duplicate(det), Duplicate(C) / Duplicate(det), Duplicate(F) / Duplicate(det), Duplicate(I) / Duplicate(det)))

      case _ =>
        // TODO gather and exploit knowledge about matrix structure
        Knowledge.experimental_resolveInverseFunctionCall match {
          case "Cofactors"   => {
            val inv_det = IR_IntegerConstant(1) / IR_ResolveMatrixFunctions.calculateDeterminant(this)
            val tmp = IR_TensorExpression(Some(innerDatatype.getOrElse(IR_RealDatatype)), rows, columns)
            for (row <- 0 until rows) {
              for (col <- 0 until columns) {
                tmp.set(col, row, IR_ResolveMatrixFunctions.calculateMatrixOfMinorsElement(this, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
              }
            }
            tmp
          }
          case "GaussJordan" => {
            val matrix = Duplicate(this)
            val other = IR_TensorExpression(matrix.datatype, matrix.rows, matrix.columns)
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
                case (top : IR_Number, current : IR_Number) => swap = Math.abs(top.value.asInstanceOf[Number].doubleValue) > Math.abs(current.value.asInstanceOf[Number].doubleValue)
                case _                                      =>
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
          case "Runtime"     => Logger.error("'Runtime' matrix inversion chosen but in code path for compile time")
          case _             => Logger.error(s"""Unknown matrix inversion resolution strategy "${ Knowledge.experimental_resolveInverseFunctionCall }"""")
        }
    }
  }
}

object IR_ExtractMatrices extends DefaultStrategy("Extract and split matrix expressions where necessary") {
  val annotationFctCallCounter = "IR_ResolveMatrices.fctCallCounter"
  var fctCallCounter = 0
  // temporary variable used to replace function calls in expressions
  val annotationMatExpCounter = "IR_ResolveMatrices.matrixExpressionCounter"
  var matExpCounter = 0
  var resolveFunctions = ListBuffer[String]()
  var globalCollection : Option[IR_GlobalCollection] = None

  this.onBefore = () => {
    resolveFunctions.clear()
    resolveFunctions ++= ListBuffer("dotProduct", "dot", "crossProduct", "cross", "det", "transpose")
    if (Knowledge.experimental_resolveInverseFunctionCall != "Runtime") resolveFunctions += "inverse"
    globalCollection = StateManager.findFirst[IR_GlobalCollection]()
  }

  this += new Transformation("assignment of operation with self", {
    case stmt @ IR_Assignment(dest : IR_VariableAccess, src, _) if (dest.datatype.isInstanceOf[IR_MatrixDatatype]) =>
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
      case call @ IR_FunctionCall(_, args) if (call.name == "inverse") =>
        args(0).datatype match {
          case _ : IR_ScalarDatatype => IR_DoubleConstant(1.0) / args(0)

          case m : IR_MatrixDatatype =>
            args(0).datatype.asInstanceOf[IR_MatrixDatatype]
            if (m.sizeM > 1) {
              call.function = IR_PlainInternalFunctionReference("_runtimeInverseMatrix", m)
            }
            call
        }
    })
  }

  this += new Transformation("global declarations", {
    case decl @ IR_VariableDeclaration(_ : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) =>
      StateManager.findFirst[IR_GlobalCollection]().get.initGlobals.asInstanceOf[IR_Function].body += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
      decl.initialValue = None
      decl
  }, applyAtNode = StateManager.findFirst[IR_GlobalCollection]())

//  this += new Transformation("declarations", {
  // Definition of matrix variable including initialisation -> split into decl and assignment
//    case decl @ IR_VariableDeclaration(matrix : IR_MatrixDatatype, _, Some(exp : IR_Expression), _) =>
//      val newStmts = ListBuffer[IR_Statement]()
//      // split declaration and definition so each part can be handled by subsequent transformations
//      newStmts += IR_VariableDeclaration(matrix, decl.name, None)
//      newStmts += IR_Assignment(IR_VariableAccess(Duplicate(decl)), exp)
//      newStmts
//  })

  this += new Transformation("extract function calls 1/2", {
    case stmt @ IR_Assignment(_, src, _) if (src.datatype.isInstanceOf[IR_MatrixDatatype])    =>
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
      //      StateManager.findAll[IR_TensorExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
    case stmt @ IR_ExpressionStatement(src) if (src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      var newStmts = ListBuffer[IR_Statement]()
      StateManager.findAll[IR_FunctionCall](src).foreach(exp => { // resolveFunction check not needed: all internally resolved function return a value
        val decl = IR_VariableDeclaration(exp.datatype, "_fct" + fctCallCounter + "_" + exp.name.replace('<', '_').replace('>', '_'), None)
        newStmts += decl
        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
        exp.annotate(annotationFctCallCounter, fctCallCounter)
        fctCallCounter += 1
      })
      //      StateManager.findAll[IR_TensorExpression](src).foreach(exp => {
      //        val decl = IR_VariableDeclaration(exp.datatype, "_matrixExp" + matExpCounter, None)
      //        newStmts += decl
      //        newStmts += IR_Assignment(IR_VariableAccess(decl), Duplicate(exp))
      //        exp.annotate(annotationMatExpCounter, matExpCounter)
      //        matExpCounter += 1
      //      })
      newStmts += stmt
      newStmts
  })

  this += new Transformation("extract function calls 2/2", {
    case exp : IR_FunctionCall if (exp.hasAnnotation(annotationFctCallCounter)) =>
      IR_VariableAccess("_fct" + exp.popAnnotationAs[Int](annotationFctCallCounter) + "_" + exp.function.name.replace('<', '_').replace('>', '_'), exp.function.returnType)

    case exp : IR_TensorExpression if (exp.hasAnnotation(annotationMatExpCounter)) =>
      IR_VariableAccess("_matrixExp" + exp.popAnnotationAs[Int](annotationMatExpCounter), exp.datatype)
  })

  this += new Transformation("parameters and return types", {
    case arg : IR_FunctionArgument if (arg.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      arg.datatype = IR_ReferenceDatatype(arg.datatype)
      arg
    case func : IR_Function if (func.datatype.isInstanceOf[IR_MatrixDatatype])       =>
      val matrix = func.datatype.asInstanceOf[IR_MatrixDatatype]
      func.parameters += IR_FunctionArgument("_matrix_return", IR_ReferenceDatatype(matrix))
      func.datatype = IR_UnitDatatype

      func.body = func.body.flatMap(stmt => stmt match {
        case IR_Return(Some(exp)) if (exp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          List(
            IR_Assignment(IR_VariableAccess("_matrix_return", matrix), exp),
            IR_Return())
        }
        case _                                                                      => List(stmt)
      })
      func
  })

  this += new Transformation("function call returns", {
    case IR_Assignment(dest, src : IR_FunctionCall, "=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype])  =>
      // FIXME resolve IR_Assignments with operator += before this
      src.arguments += dest
      IR_ExpressionStatement(src)
    case IR_Assignment(dest, src : IR_FunctionCall, "+=") if (dest.datatype.isInstanceOf[IR_MatrixDatatype] && src.datatype.isInstanceOf[IR_MatrixDatatype]) =>
      Logger.error("+= matrix operator resolution not yet implemented")
  })

  this += new Transformation("simplify function call arguments", {
    case stmt @ IR_ExpressionStatement(exp : IR_FunctionCall)                                               =>
      var newStmts = ListBuffer[IR_Statement]()

      exp.arguments.transform {
        case argexp : IR_MultiDimFieldAccess                                             => argexp
        case argexp : IR_VariableAccess                                                  => argexp
        case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, argexp)
          newStmts += decl
          IR_VariableAccess(decl)
        }
        case arg                                                                         => arg
      }
      newStmts += stmt
      newStmts
    case stmt @ IR_Assignment(_, exp : IR_FunctionCall, _) if !resolveFunctions.contains(exp.function.name) =>
      var newStmts = ListBuffer[IR_Statement]()

      exp.arguments.transform {
        case argexp : IR_MultiDimFieldAccess                                             => argexp
        case argexp : IR_VariableAccess                                                  => argexp
        case argexp : IR_Expression if (argexp.datatype.isInstanceOf[IR_MatrixDatatype]) => {
          var decl = IR_VariableDeclaration(argexp.datatype, "_matrixExp" + matExpCounter, argexp)
          newStmts += decl
          IR_VariableAccess(decl)
        }
        case arg                                                                         => arg
      }
      newStmts += stmt
      newStmts
  })
}
*/

// TODO: Hier geht der Spaß los
object IR_ResolveTensorFunctions extends DefaultStrategy("Resolve special matrix and vector functions") {
  val annotationMatrixRow = "IR_ResolveTensor.matrixRow"
  val annotationMatrixCol = "IR_ResolveTensor.matrixCol"

  def calculateDeterminant(m : IR_TensorExpression) : IR_Expression = {
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
      var det : IR_Expression = IR_IntegerConstant(0)
      val tmp = IR_TensorExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
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

  def calculateMatrixOfMinorsElement(m : IR_TensorExpression, forRow : Integer, forColumn : Integer) : IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("matrix of minors for non-quadratic matrices not implemented ")
    }
    val tmp = IR_TensorExpression(Some(m.innerDatatype.getOrElse(IR_RealDatatype)), m.rows - 1, m.columns - 1)
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

  def findPivot(matrix : IR_TensorExpression, startRow : Int) : Int = {
    var myMax = (0, 0.0)
    for (row <- startRow until matrix.rows) {
      val myElem = matrix.get(row, startRow)
      myElem match {
        case x : IR_IntegerConstant => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x : IR_RealConstant    => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case x : IR_FloatConstant   => if (Math.abs(x.value) > myMax._2) myMax = (row, Math.abs(x.v))
        case _                      =>
      }
    }
    if (myMax._2 == 0.0) {
      // it's not constant 0 (or all the other stuff was 0 as well), so let's hope we gonna be fine...
      return startRow
    } else {
      return myMax._1
    }
  }

  def getElem(exp : IR_Expression, row : Integer, col : Integer) = {
    exp match {
      case x : IR_TensorExpression                                           => x.get(row, col)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_HighDimAccess(Duplicate(x), new IR_ConstIndex(Array(row, col)))
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }
  def getSingleElem(exp : IR_Expression) = {
    exp match {
      case x : IR_TensorExpression                                           => x.get(0, 0)
      case x : IR_Expression if (x.datatype.isInstanceOf[IR_MatrixDatatype]) => x
      case _                                                                 => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
    }
  }
}

object IR_ResolveTensorAssignments extends DefaultStrategy("Resolve assignments to matrices") {
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

object IR_SetupTensorExpressions extends DefaultStrategy("Convert accesses to matrices and vectors to MatrixExpressions") {
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

object IR_LinearizeTensors extends DefaultStrategy("Linearize matrices") {
  this += Transformation("Linearize", {
    case IR_HighDimAccess(base, _) if (!base.datatype.isInstanceOf[IR_MatrixDatatype]) => base

    case IR_HighDimAccess(base : IR_MultiDimFieldAccess, idx : IR_Index) =>
      val hoIdx = idx.toExpressionIndex
      val fieldLayout = base.field.layout
      for (dim <- fieldLayout.numDimsGrid until fieldLayout.numDimsData) {
        if (base.index.indices.length <= dim)
          base.index.indices :+= hoIdx(dim - fieldLayout.numDimsGrid)
        else
          base.index.indices(dim) += hoIdx(dim - fieldLayout.numDimsGrid)
      }
      base

    case IR_HighDimAccess(base, idx : IR_ConstIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM > 1 || matrix.sizeN > 1 || idx(0) > 0 || idx(1) > 0)
        IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
      else
        base

    case IR_HighDimAccess(base, idx : IR_ExpressionIndex) if idx.indices.length == 2 =>
      val matrix = base.datatype.asInstanceOf[IR_MatrixDatatype]
      if (matrix.sizeM > 1 || matrix.sizeN > 1)
        IR_ArrayAccess(base, matrix.sizeN * idx.indices(0) + idx.indices(1))
      else
        base
  }, false)
}
