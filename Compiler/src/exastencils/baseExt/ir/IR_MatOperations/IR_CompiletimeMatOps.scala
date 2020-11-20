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

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatNodeUtils._
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.solver.ir.IR_MatrixSolveOps
import exastencils.util.ir._

// simple operations with matrices like addition or multiplication
object IR_CompiletimeMatOps {
  var tmpCounter = 0
  var eliminatedPivotsOfMatrix = 0

  /** Method: get an element of a however typed matrix
    *
    * @param exp : IR_Expression, matrix to get the element from
    * @param pos : Int*, optional position indices
    * @return element of position pos
    **/
  def getElem(exp : IR_Expression, pos : Int*) = {
    try {
      exp match {
        case x : IR_MatrixExpression                                                 =>
          if (pos.length != 2)
            Logger.error("position arguments of wrong form: " + pos)
          x.get(pos(0), pos(1))
        case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype]) =>
          if (pos.length != 2)
            Logger.error("position arguments of wrong form: " + pos)
          IR_HighDimAccess(va, IR_ExpressionIndex(pos(0), pos(1)))
        case fa : IR_FieldAccess if (fa.datatype.isInstanceOf[IR_MatrixDatatype])    =>
          IR_HighDimAccess(fa, IR_ExpressionIndex(pos(0), pos(1)))
        case sc if (isScalar(sc))                                                    => sc
        case _                                                                       => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
      }
    } catch {
      case e : ArrayIndexOutOfBoundsException => throw new ArrayIndexOutOfBoundsException
    }
  }

  def evalNumExprWrapper(expr : IR_Expression) : Option[Double] = {
    val dvalue : Option[Double] =
      try {
        Some(evalNumExpr(expr))
      } catch {
        case e : EvaluationException => None
      }
    dvalue
  }

  def evalNumExpr(expr : IR_Expression) : Double = expr match {
    case IR_RealConstant(v)                                   => v
    case IR_DoubleConstant(v)                                 => v
    case IR_FloatConstant(v)                                  => v.doubleValue()
    case IR_IntegerConstant(v)                                => v.doubleValue()
    case IR_Addition(sums : ListBuffer[IR_Expression])        => sums.view.map(s => evalNumExpr(s)).sum
    case IR_Subtraction(l : IR_Expression, r : IR_Expression) => evalNumExpr(l) - evalNumExpr(r)
    case IR_Multiplication(facs : ListBuffer[IR_Expression])  => facs.view.map(s => evalNumExpr(s)).product
    case IR_Division(l : IR_Expression, r : IR_Expression)    => evalNumExpr(l) / evalNumExpr(r)
    case IR_Power(base, exp)                                  => scala.math.pow(evalNumExpr(base), evalNumExpr(exp))
    case IR_Negative(v)                                       => evalNumExpr(v) * (-1.0)
    case _                                                    =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  def isConst(expr : IR_Expression) : Boolean = expr match {
    case IR_RealConstant(_)                                   => true
    case IR_DoubleConstant(_)                                 => true
    case IR_FloatConstant(_)                                  => true
    case IR_IntegerConstant(_)                                => true
    case IR_Addition(sums : ListBuffer[IR_Expression])        => sums.forall(e => isConst(e))
    case IR_Subtraction(l : IR_Expression, r : IR_Expression) => isConst(l) && isConst(r)
    case IR_Multiplication(facs : ListBuffer[IR_Expression])  => facs.forall(e => isConst(e))
    case IR_Division(l : IR_Expression, r : IR_Expression)    => isConst(l) && isConst(r)
    case _                                                    => false
  }

  def isConstMatrix(me : IR_MatrixExpression) : Boolean = me.expressions.forall(e => isConst(e))

  def isRConst(e : IR_Expression) : Boolean = e.isInstanceOf[IR_RealConstant]

  def simplifyNumExpr(expr : IR_Expression) : IR_Expression = expr match {
    case rc : IR_RealConstant                                => rc
    case dc : IR_DoubleConstant                              => dc
    case va : IR_VariableAccess                              => va
    case IR_Division(l, r)                                   =>
      val leval = simplifyNumExpr(l)
      val reval = simplifyNumExpr(r)
      (isRConst(leval), isRConst(reval)) match {
        case (true, false)  => IR_Division(leval, reval)
        case (false, true)  => IR_Division(leval, reval)
        case (false, false) => IR_Division(leval, reval)
        case (true, true)   => leval.asInstanceOf[IR_RealConstant].value / reval.asInstanceOf[IR_RealConstant].value
      }
    case IR_Subtraction(l, r)                                =>
      val leval = simplifyNumExpr(l)
      val reval = simplifyNumExpr(r)
      (isRConst(leval), isRConst(reval)) match {
        case (true, false)  => IR_Subtraction(leval, reval)
        case (false, true)  => IR_Subtraction(leval, reval)
        case (false, false) => IR_Subtraction(leval, reval)
        case (true, true)   => leval.asInstanceOf[IR_RealConstant].value - reval.asInstanceOf[IR_RealConstant].value
      }
    case IR_Multiplication(facs : ListBuffer[IR_Expression]) =>
      val facseval = facs.map(f => simplifyNumExpr(f))
      var rval = 1.0
      var accs = ListBuffer[IR_Expression]()
      facseval.map(f => if (isRConst(f)) rval = rval * f.asInstanceOf[IR_RealConstant].v else accs += f)
      if (accs.nonEmpty) {
        accs += IR_RealConstant(rval)
        IR_Multiplication(accs)
      } else IR_RealConstant(rval)
    case IR_Addition(sums : ListBuffer[IR_Expression])       =>
      val sumseval = sums.map(f => simplifyNumExpr(f))
      var rval = 0.0
      var accs = ListBuffer[IR_Expression]()
      sumseval.map(f => if (isRConst(f)) rval = rval + f.asInstanceOf[IR_RealConstant].v else accs += f)
      if (accs.nonEmpty) {
        accs += IR_RealConstant(rval)
        IR_Addition(accs)
      } else IR_RealConstant(rval)

    case _ =>
      throw EvaluationException("unknown expression type for evaluation: " + expr.getClass)
  }

  /** Method: return the size of a however typed matrix
    *
    * @param in : IR_Expression, matrix to get the size of
    * @return tuple of int: size
    **/
  def getSize(in : IR_Expression) = {
    in match {
      case me : IR_MatrixExpression                                                          => (me.rows, me.columns)
      case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype])           => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
      case _ : IR_ScalarDatatype                                                             => (1, 1)
      case sva : IR_VariableAccess if (sva.datatype.isInstanceOf[IR_ScalarDatatype])         => (1, 1)
      case mdt : IR_MatrixDatatype                                                           => (mdt.sizeM, mdt.sizeN)
      case fa : IR_FieldAccess if (fa.field.layout.datatype.isInstanceOf[IR_MatrixDatatype])
        => (fa.field.layout.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, fa.field.layout.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
      case _                                                                                 => Logger.error("argument is of unexpected type: " + in)
    }
  }

  /** Method: copy a submatrix of a matrix
    *
    * @param from        : IR_Expression, matrix to get slice from
    * @param offset_rows : Int, offset of the slice in y direction
    * @param offset_cols : Int, offset of the slice in x direction
    * @param n_rows      : Int, width in y direction
    * @param n_cols      : Int, width in x direction
    * @return slice of from as expression
    **/
  def copySubMatrix(from : IR_Expression, offset_rows : Int, offset_cols : Int, n_rows : Int, n_cols : Int) : IR_MatrixExpression = {
    if (offset_cols < 0 || offset_rows < 0) {
      Logger.error("negative offset")
    }
    //Logger.error(from.datatype.resolveBaseDatatype)
    var submatrix = IR_MatrixExpression(Some(from.datatype.resolveBaseDatatype), n_rows, n_cols)
    val bound_cols = offset_cols + n_cols
    val bound_rows = offset_rows + n_rows
    for (i <- offset_rows until bound_rows) {
      for (j <- offset_cols until bound_cols) {
               var n = Duplicate(getElem(from, i, j))
        //var n = getElem(from, i, j)
        submatrix.set(i - offset_rows, j - offset_cols, n)
      }
    }
    for (a <- from.annotations)
      submatrix.annotate(a._1, a._2)
    submatrix
  }

  /** Method: set a submatrix of a matrix with another matrix
    *
    * @param source      : IR_Expression, matrix to get slice from
    * @param target      : IR_Expression, return parameter, matrix to set the slice in
    * @param offset_rows : Int, offset of the slice in y direction
    * @param offset_cols : Int, offset of the slice in x direction
    * @return unit, slice set in target
    **/
  def pasteSubMatrix(source : IR_Expression, target : IR_MatrixExpression, offset_rows : Int, offset_cols : Int) : Unit = {
    if (offset_rows < 0 || offset_cols < 0) {
      Logger.error("negative offset")
    }
    val ssize = getSize(source)
    val bound_cols = offset_cols + ssize._2
    val bound_rows = offset_rows + ssize._1
    for (i <- offset_rows until bound_rows) {
      for (j <- offset_cols until bound_cols) {
        //TODO move node objects instead of copying?
        //var n = Duplicate(getElem(source, i - offset_rows, j - offset_cols))
        var n = getElem(source, i - offset_rows, j - offset_cols)
        target.set(i, j, n)
      }
    }
    if (source.hasAnnotation("CTPivotElimination")) {
      if (target.hasAnnotation("CTPivotElimination")) {
        target.annotate("CTPivotElimination", target.popAnnotationAs[ListBuffer[IR_VariableDeclaration]]("CTPivotElimination")
          ++= source.popAnnotationAs[ListBuffer[IR_VariableDeclaration]]("CTPivotElimination"))
      } else {
        target.annotate("CTPivotElimination", source.popAnnotationAs[ListBuffer[IR_VariableDeclaration]]("CTPivotElimination"))
      }
    }
  }

  /** Method: calculate the determinant of a matrix per laplace expansion
    *
    * @param m : IR_MatrixExpression, matrix to calculate the determinant of
    * @return determinant
    **/
  def smallMatrixDeterminant(m : IR_MatrixExpression) : IR_Expression = {
    if (m.rows != m.columns) {
      Logger.error("determinant for non-quadratic matrices not implemented")
      // FIXME Nullzeilen/-spalten ergaenzen
    }
    if (m.rows <= 0) {
      Logger.error("MatrixExpression of size <= 0")
    } else if (m.rows == 1) {
      return Duplicate(m.get(0, 0))
    } else if (m.rows == 2) {
      return Duplicate(m.get(0, 0)) * Duplicate(m.get(1, 1)) - Duplicate(m.get(0, 1)) * Duplicate(m.get(1, 0))
    } else if (m.rows == 3) {
      return Duplicate(m.get(0, 0)) * Duplicate(m.get(1, 1)) * Duplicate(m.get(2, 2)) +
        Duplicate(m.get(0, 1)) * Duplicate(m.get(1, 2)) * Duplicate(m.get(2, 0)) +
        Duplicate(m.get(0, 2)) * Duplicate(m.get(1, 0)) * Duplicate(m.get(2, 1)) -
        Duplicate(m.get(2, 0)) * Duplicate(m.get(1, 1)) * Duplicate(m.get(0, 2)) -
        Duplicate(m.get(2, 1)) * Duplicate(m.get(1, 2)) * Duplicate(m.get(0, 0)) -
        Duplicate(m.get(2, 2)) * Duplicate(m.get(1, 0)) * Duplicate(m.get(0, 1))
    } else {
      var det : IR_Expression = IR_IntegerConstant(0)
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
        val tmpDet = m.get(i, 0) * smallMatrixDeterminant(tmp) * IR_DoubleConstant(math.pow(-1, i))
        det += IR_GeneralSimplifyWrapper.process[IR_Expression](tmpDet)
      }
      IR_GeneralSimplifyWrapper.process(det)
    }
  }

  def calculateMatrixOfMinorsElement(m : IR_MatrixExpression, forRow : Integer, forColumn : Integer) : IR_Expression = {
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
    smallMatrixDeterminant(tmp)
  }

  // transpose a matrix passed by a variable
  /** Method: calculate the transpose of a matrix
    *
    * @param source : IR_VariableDeclaration, matrix to calculate the transposed
    * @return transposed matrix
    **/
  def transpose(source : IR_MatrixExpression) : IR_MatrixExpression = {
    var out = IR_MatrixExpression(source.datatype.resolveBaseDatatype, source.datatype.sizeN, source.datatype.sizeM)
    for (i <- 0 until source.datatype.sizeM) {
      for (j <- 0 until source.datatype.sizeN) {
        out.set(j, i, Duplicate(source.get(i, j)))
      }
    }
    out
  }

  def dotProduct(l : IR_MatrixExpression, r : IR_MatrixExpression) : IR_MatrixExpression = {
    var lsize = (l.datatype.sizeM, l.datatype.sizeN)
    var rsize = (r.datatype.sizeM, r.datatype.sizeN)
    (lsize, rsize) match {
      case ((lrows, lcols), (rrows, rcols)) if (lcols == rcols && lrows == rrows) =>
        var out = IR_MatrixExpression(IR_ResultingDatatype(l.datatype, r.datatype), 1, 1)
        out.set(0, 0, IR_IntegerConstant(0))
        for (i <- 0 until rrows) {
          for (j <- 0 until rcols) {
            out.set(0, 0, IR_Addition(Duplicate(out.get(0, 0)), IR_Multiplication(l.get(i, j), r.get(i, j))))
          }
        }
        out
      case ((1, lcols), (rrows, 1)) if (lcols == rrows)                           =>
        var out = IR_MatrixExpression(IR_ResultingDatatype(l.datatype, r.datatype), 1, 1)
        out.set(0, 0, IR_IntegerConstant(0))
        for (i <- 0 until rrows) {
          out.set(0, 0, IR_Addition(Duplicate(out.get(0, 0)), IR_Multiplication(l.get(0, i), r.get(i, 0))))
        }
        out
      case _                                                                      => Logger.error("unsupported argument form: " + lsize + ", " + rsize + ", expected arguments of the same size")
    }

  }

  /** Method: calculate the ross product of two vectors as matrices with 1 in one dimension
    *
    * @param left  : IR_Expression, left matrix operand
    * @param right : IR_Expression, right matrix  operand
    * @return cross product as matrix
    **/
  def crossProduct(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    var lsize = getSize(left)
    var rsize = getSize(right)
    if (lsize._1 != 3 || lsize._2 != 1 || rsize._1 != 3 || rsize._2 != 1)
      Logger.error("cross product only supported for two column vectors of size 3, arguments are of form: " + lsize + "," + rsize)
    var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), 3, 1)
    out.set(0, 0, IR_Subtraction(IR_Multiplication(getElem(left, 1, 0), getElem(right, 2, 0)), IR_Multiplication(getElem(left, 2, 0), getElem(right, 1, 0))))
    out.set(1, 0, IR_Subtraction(IR_Multiplication(getElem(left, 2, 0), getElem(right, 0, 0)), IR_Multiplication(getElem(left, 0, 0), getElem(right, 2, 0))))
    out.set(2, 0, IR_Subtraction(IR_Multiplication(getElem(left, 0, 0), getElem(right, 1, 0)), IR_Multiplication(getElem(left, 1, 0), getElem(right, 0, 0))))
    out
  }

  /** Method: matrix matrix multiplication
    *
    * @param left  : IR_Expression, left matrix operand
    * @param right : IR_Expression, right matrix  operand
    * @return result of multiplication
    ***/
  def mult(left : IR_MatrixExpression, right : IR_MatrixExpression) = {
    var lsize = getSize(left)
    var rsize = getSize(right)
    if (lsize._2 != rsize._1)
      Logger.error("sizes do not match: " + lsize + " vs " + rsize)
    var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype.resolveBaseDatatype, right.datatype.resolveBaseDatatype), lsize._1, rsize._2)
    for (i <- 0 until lsize._1) {
      for (j <- 0 until rsize._2) {
        var tmp = IR_Addition(IR_IntegerConstant(0))
        for (k <- 0 until rsize._1) {
          tmp = IR_Addition(tmp, IR_Multiplication(Duplicate(left.get(i, k)), Duplicate(right.get(k, j))))
        }
        out.set(i, j, Duplicate(tmp))
      }
    }
    out
  }
  def isMatrixCpy(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))                                                 => true
      case IR_MatrixExpression(_, _, _, _)                                                                  => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_MatrixDatatype]) => true
      //FIXME this stmt enables SWE test
      //case fa : IR_MultiDimFieldAccess if (fa.datatype.isInstanceOf[IR_MatrixDatatype])      => true
      //case fa : IR_FieldAccess if (fa.field.layout.datatype.isInstanceOf[IR_MatrixDatatype]) => true
      case _                                                                                 => false
    }
  }
  /** Method: matrix matrix multiplication
    *
    * @param mult : IR_Multiplication, operands as multiplication
    * @return result of multiplication
    ***/
  def mult(mult : IR_Multiplication) : IR_MatrixExpression = {
    var result = IR_MatrixExpression(IR_IntegerDatatype, 1, 1)
    var firstMatrix = mult.factors.find(fac => isMatrix(fac)).getOrElse(Logger.error("no matrix in factors!"))
    var firstMatrixIdx = mult.factors.indexOf(firstMatrix)
    mult.factors.remove(firstMatrixIdx)
    mult.factors.prepend(firstMatrix)
    var tmp = mult.factors(0) match {
      case va : IR_Access if (va.datatype.isInstanceOf[IR_MatrixDatatype]) =>
        IR_MatNodeUtils.accessToMatExpr(va)
      case x : IR_MatrixExpression                                         =>
        Duplicate(x)
      case _                                                               =>
        Logger.error("unexpected type: " + mult.factors(0))
    }
    for (f <- 1 until mult.factors.length) {
      result = mult.factors(f) match {
        case va : IR_Access if (va.datatype.isInstanceOf[IR_MatrixDatatype]) =>
          IR_CompiletimeMatOps.mult(tmp, IR_MatNodeUtils.accessToMatExpr(va))
        case x : IR_MatrixExpression                                         =>
          IR_CompiletimeMatOps.mult(tmp, x)
        case s if (isScalar(s))                                              =>
          IR_CompiletimeMatOps.elementwiseMultiplication(tmp, s)
        case _                                                               =>
          Logger.error("unexpected type: " + mult.factors(f))
      }
      tmp = Duplicate(result)
    }
    result
  }

  /** Method: matrix matrix addition
    *
    * @param left  : IR_Expression, left matrix operand
    * @param right : IR_Expression, right matrix  operand
    * @return result of addition
    ***/
  def add(left : IR_MatrixExpression, right : IR_MatrixExpression) = {
    var lsize = getSize(left)
    var rsize = getSize(right)
    if (lsize != rsize)
      Logger.error("sizes do not match: " + lsize + " vs " + rsize)
    var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
    for (i <- 0 until lsize._1) {
      for (j <- 0 until rsize._2) {
        out.set(i, j, Duplicate(IR_Addition(left.get(i, j), right.get(i, j))))
      }
    }
    out
  }

  /** Method: matrix matrix addition
    *
    * @param addition : IR_Expression,  matrix operands can be IR_ElementwiseAddition or IR_Addition
    * @return result of addition
    ***/
  def add(addition : IR_Expression) : IR_MatrixExpression = {
    addition match {
      case a : IR_Addition                  =>
        var size = (0, 0)
        a.summands.foreach(x => if (x.datatype.isInstanceOf[IR_MatrixDatatype]) size = getSize(x))
        if (size == (0, 0))
          Logger.error("no matrix in summands")
        var datatype = a.summands(0).datatype
        a.summands.foreach(x => datatype = IR_ResultingDatatype.apply(datatype, x.datatype))
        var out = IR_MatrixExpression(datatype.resolveBaseDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_IntegerConstant(0))
          }
        }
        for (k <- 0 until a.summands.length) {
          for (i <- 0 until size._1) {
            for (j <- 0 until size._2) {
              out.set(i, j, IR_Addition(Duplicate(out.get(i, j)), getElem(a.summands(k), i, j)))
            }
          }
        }
        out
      case _ @ IR_ElementwiseAddition(_, _) =>
        Logger.error("elementwise addition not yet supported")
      case _                                => Logger.error("unexpected type: " + addition + ", expected IR_Addition or IR_ElementwiseAddition")
    }

  }

  /** Method: matrix matrix subtraction
    *
    * @param left  : IR_Expression, left matrix operand
    * @param right : IR_Expression, right matrix  operand
    * @return result of subtraction
    ***/
  def sub(left : IR_MatrixExpression, right : IR_MatrixExpression) : IR_MatrixExpression = {
    var lsize = getSize(left)
    var rsize = getSize(right)
    if (lsize != rsize)
      Logger.error("sizes do not match: " + lsize + " vs " + rsize)
    var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), lsize._1, lsize._2)
    for (i <- 0 until lsize._1) {
      for (j <- 0 until rsize._2) {
        out.set(i, j, Duplicate(IR_Subtraction(left.get(i, j), right.get(i, j))))
      }
    }
    out
  }

  /** Method: matrix matrix subtraction
    *
    * @param subtraction : IR_Expression,  matrix operands can be IR_ElementwiseAddition or IR_Addition
    * @return result of subtraction
    ***/
  def sub(subtraction : IR_Expression) : IR_MatrixExpression = {
    subtraction match {
      case sub : IR_Subtraction             =>
        sub.left match {
          case matrix if (isMatrix(matrix)) =>
            var size = getSize(matrix)
            var out = IR_MatrixExpression(matrix.datatype.resolveBaseDatatype, size._1, size._2)
            for (i <- 0 until size._1) {
              for (j <- 0 until size._2) {
                out.set(i, j, IR_Subtraction(getElem(sub.left, i, j), getElem(sub.right, i, j)))
              }
            }
            out
          case scalar if (isScalar(scalar)) =>
            sub.right match {
              case x : IR_MatrixExpression                                         =>
                IR_CompiletimeMatOps.sub(IR_Subtraction(negative(x), IR_Negative(sub.left)))
              case va : IR_Access if (va.datatype.isInstanceOf[IR_MatrixDatatype]) =>
                IR_CompiletimeMatOps.sub(IR_Subtraction(negative(IR_MatNodeUtils.accessToMatExpr(va)), IR_Negative(sub.left)))
              case _                                                               =>
                Logger.error(s"unexpected argument of type: ${ sub.right.datatype } and basetype: ${ sub.right.datatype.resolveBaseDatatype }, left side is of type: ${ sub.left.datatype } and basetype: ${ sub.left.datatype.resolveBaseDatatype }")
            }
          case _                            => Logger.error("unexpected argument: " + sub.left + ", expected matrix or scalar")
        }
      case esub : IR_ElementwiseSubtraction =>
        Logger.error("IR_ElementwiseSubtraction not yet supported")
      case _                                =>
        Logger.error("unexpected argument: " + subtraction + ", expected IR_Subtraction or IR_ElementwiseSubtraction")
    }
  }

  /** Method: elementwise multiplication
    *
    * @param left  : IR_Expression, left operand can be scalar or matrix
    * @param right : IR_Expression, right operand can be scalar or matrix
    * @return result of multiplication
    ***/
  def elementwiseMultiplication(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar, matrix x matrix
      case (scalar, matrix) if (isScalar((scalar)) && isMatrix(matrix))                   =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_RealDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case (matrix, scalar) if (isScalar((scalar)) && isMatrix(matrix))                   =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_RealDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(scalar, i, j), getElem(matrix, i, j)))
          }
        }
        out
      case (matrixLeft, matrixRight) if (isMatrix((matrixLeft)) && isMatrix(matrixRight)) =>
        var size = getSize(matrixLeft)
        var sizeR = getSize(matrixRight)
        if (size != sizeR)
          Logger.error("sizes do not match: " + size + " vs " + sizeR)
        var out = IR_MatrixExpression(IR_RealDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(matrixLeft, i, j), getElem(matrixRight, i, j)))
          }
        }
        out
      case _                                                                              => Logger.error("unexpected argument combination: " + (left, right))
    }
  }

  /** Method: elementwise division
    *
    * @param left  : IR_Expression, left operand can be scalar or matrix
    * @param right : IR_Expression, right operand can be scalar or matrix
    * @return result of division
    ***/
  def elementwiseDivision(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar, matrix x matrix
      case (scalar, matrix) if (isScalar((scalar)) && isMatrix(matrix))                   =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_RealDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(scalar, i, j), getElem(matrix, i, j)))
          }
        }
        out
      case (matrix, scalar) if (isScalar((scalar)) && isMatrix(matrix))                   =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_RealDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case (matrixLeft, matrixRight) if (isMatrix((matrixLeft)) && isMatrix(matrixRight)) =>
        var size = getSize(matrixLeft)
        var sizeR = getSize(matrixRight)
        if (size != sizeR)
          Logger.error("sizes do not match: " + size + " vs " + sizeR)
        var out = IR_MatrixExpression(IR_RealDatatype, size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(matrixLeft, i, j), getElem(matrixRight, i, j)))
          }
        }
        out
      case _                                                                              => Logger.error("unexpected argument combination")
    }
  }

  //TODO elementwise power does not parse
  /*
   def elementwisePower(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
     (left, right) match {
       // scalar x matrix, matrix x scalar
       case (scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
         var size = getSize(matrix)
         var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
         for (i <- 0 until size._1) {
           for (j <- 0 until size._2) {
             out.set(i, j, IR_Power(getElem(scalar, i, j), getElem(matrix, i, j)))
           }
         }
         out
       case (matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), scalar @ (IR_VariableAccess(_, IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype))) =>
         var size = getSize(matrix)
         var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
         for (i <- 0 until size._1) {
           for (j <- 0 until size._2) {
             out.set(i, j, IR_Power(getElem(matrix, i, j), getElem(scalar, i, j)))
           }
         }
         out
       case _                                                                                                                                                         => Logger.error("unexpected argument combination")
     }
   }
 */

  // return a matrix with negative elements of input
  def negative(that : IR_Expression) : IR_MatrixExpression = {
    val out = that match {
      case mat : IR_MatrixExpression                                       =>
        IR_MatrixExpression(mat.innerDatatype, mat.rows, mat.columns)
      case va : IR_Access if (va.datatype.isInstanceOf[IR_MatrixDatatype]) =>
        val dt = va.datatype.asInstanceOf[IR_MatrixDatatype]
        IR_MatrixExpression(dt.resolveBaseDatatype, dt.sizeM, dt.sizeN)
    }
    for (i <- 0 until out.rows) {
      for (j <- 0 until out.columns) {
        //        out.set(i,j,IR_Subtraction(IR_RealConstant(0),Duplicate(that.get(i,j))))
        out.set(i, j, IR_Negative(Duplicate(IR_CompiletimeMatOps.getElem(that, i, j))))
      }
    }
    out
  }

  // return the sum of the diagonal elements of a matrix
  def trace(matrix : IR_MatrixExpression) : IR_Addition = {
    var sum = IR_Addition(IR_IntegerConstant(0))

    if (matrix.datatype.sizeM != matrix.datatype.sizeN)
      Logger.error("trace only for quadratic matrices supported")
    for (i <- 0 until matrix.datatype.sizeM) {
      sum = IR_Addition(sum, matrix.get(i, i))
    }
    sum
  }

  // convert tensor to matrix expression
  def tenExprToMatExpr(ten : IR_Expression) : IR_MatrixExpression = {
    ten match {
      case ten1 : IR_TensorExpression1 =>
        val out = IR_MatrixExpression(ten1.innerDatatype.get, ten1.dims, 1)
        out.expressions = Duplicate(ten1.expressions)
        out
      case ten2 : IR_TensorExpression2 =>
        val out = IR_MatrixExpression(ten2.innerDatatype.get, ten2.dims, ten2.dims)
        out.expressions = Duplicate(ten2.expressions)
        out
      case tenN : IR_TensorExpressionN => Logger.error("conversion from tensor of order N to matrix not yet implemented!")
      case _                           => Logger.error(s"uenxpected argument ${ ten }, expected tensor")
    }
  }

  def convertTensorToMat(ten : IR_Expression) : IR_MatrixExpression = {
    ten match {
      case x : IR_TensorExpression                                           => tenExprToMatExpr(x)
      case va @ IR_VariableAccess(_, dt @ IR_TensorDatatype1(innerDt, dims)) =>
        val out = IR_TensorExpression1(innerDt, dims)
        for (i <- 0 until dims) {
          out.set(i, IR_HighDimAccess(va, IR_ExpressionIndex(i)))
        }
        tenExprToMatExpr(out)
      case va @ IR_VariableAccess(_, dt @ IR_TensorDatatype2(innerDt, dims)) =>
        val out = IR_TensorExpression2(innerDt, dims)
        for (i <- 0 until dims) {
          for (j <- 0 until dims) {
            out.set(i, j, IR_HighDimAccess(va, IR_ExpressionIndex(i, j)))
          }
        }
        tenExprToMatExpr(out)
      case _                                                                 => Logger.error(s"unexpected type ${ ten }, expected tensor as access or expression")
    }
  }

  // head function that branches to specific inversions
  def inverse(that : IR_MatrixExpression, msi : IR_MatShape) : IR_MatrixExpression = {
    var matrixShape = msi.shape
    if (that.rows != that.columns)
      Logger.error("inversion of non quadratic matrices not supported.")

    if (Knowledge.experimental_matrixDebugConfig)
      Logger.warn(s"inverting at compiletime with shape=$matrixShape, ${ that.rows }, ${ that.columns }")

    matrixShape match {
      case "diagonal"
                => {

        if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using diagonal for inversion")
        val tmp = Duplicate(that)
        for (row <- 0 until that.rows) {
          tmp.set(row, row, IR_Division(IR_RealConstant(1.0), that.get(row, row)))
        }
        tmp
      }
      case "blockdiagonal"
                => {
        if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using blockdiagonal for inversion")
        if (that.rows < 4)
          Logger.error("Blockdiagonal inversion not applicable for matrices < 4, use diagonal")
        var out = Duplicate(that)

        // print nodes
        if (Knowledge.experimental_matrixDebugConfig)
          exastencils.core.NodeCounter()

        val sizeA : Int = msi.size("block")
        if (sizeA < 1) {
          Logger.error("Blocksize must be at least 1")
        }
        else {
          val n_blocks = that.rows / sizeA
          if (that.rows % sizeA != 0) {
            Logger.error("Rows are not a multiple of sizeA")
          }
          else {
            for (block <- 0 until n_blocks) {
              val offset = block * sizeA

              // extract matrix block
              var subMatrix = IR_CompiletimeMatOps.copySubMatrix(that, offset, offset, sizeA, sizeA)

              //var subMatrix_inv = gaussJordanInverse(subMatrix)
              var subMatrix_inv = inverse(subMatrix, IR_MatShape("filled"))


              // copy to out matrix
              IR_CompiletimeMatOps.pasteSubMatrix(subMatrix_inv, out, offset, offset)

            }
          }
        }

        if (Knowledge.experimental_matrixDebugConfig) {
          exastencils.core.NodeCounter()
          exastencils.core.NodeCounter.countSubTree(out, "bldiag inv mat", None, None)
        }

        out
      }
      case "schur"
                => {
        if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using schur for inversion")
        if (that.rows < 3)
          Logger.error("Schur inversion not applicable for matrices < 3")
        //var out = IR_MatrixExpression(that.datatype.datatype, that.rows, that.columns)
        var out = Duplicate(that)
        /* use an invert algorithm using the schur complement

          -1             -1
         M  =  (  A  B  )      =    ( A_inv + A_inv*B*S_inv*C*A_inv -A_inv*B*S_inv  )
               (  C  D  )           (           -S_inv*C*A_inv           S_inv      )

          with M of size (n + m) x (n + m) and S = D - C * A_inv * B
        */
        val n : Int = msi.size("block")
        val m = that.rows - n

        if (n < 1) {
          Logger.error("n < 1!")
        }
        else {
          val dt = that.datatype.resolveBaseDatatype
          val tmp = if (Knowledge.experimental_schurWithHelper) {
            schurWithHelpers(that, dt, m, n, msi, out)
          } else {
            schur(that, dt, m, n, msi, out)
          }

          if (Knowledge.experimental_matrixDebugConfig)
            exastencils.core.NodeCounter.countSubTree(tmp, "schurTmp", None, None)

          tmp
        }
      }
      case "cofactors"
                => {
        if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using CF for inversion")
        cofactorInverse(that)
      }
      case "gaussJordan"
                => {
        if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using GS for inversion")
        val out = gaussJordanInverse(that)

        if (Knowledge.experimental_matrixDebugConfig)
          exastencils.core.NodeCounter.countSubTree(out, "gjTmp", None, None)

        out
      }
      case "LU" =>
        if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using LU for inversion")
        val LUP = LUDecomp(that)

        if (Knowledge.experimental_matrixDebugConfig)
          exastencils.core.NodeCounter.countSubTree(LUP._1, "LUtmp_1", None, None)

        val out = LUDecompedInverse(LUP._1, LUP._2)

        if (Knowledge.experimental_matrixDebugConfig)
          exastencils.core.NodeCounter.countSubTree(out, "LUtmp_2", None, None)

        //IR_GeneralSimplify.doUntilDoneStandalone(LUP._1)

        if (Knowledge.experimental_matrixDebugConfig)
          exastencils.core.NodeCounter.countSubTree(out, "LUtmp_3", None, None)

        out
      case "filled"
                => {
        val out : IR_MatrixExpression = that.rows match {
          case 1 =>
            IR_MatrixExpression(that.innerDatatype, 1, 1, Array(IR_Division(IR_RealConstant(1.0), Duplicate(that.get(0, 0)))), None)

          case 2 =>
            val a = that.get(0, 0)
            val b = that.get(0, 1)
            val c = that.get(1, 0)
            val d = that.get(1, 1)
            val det : IR_Expression = IR_Division(IR_RealConstant(1.0), (Duplicate(a) * Duplicate(d)) - (Duplicate(b) * Duplicate(c)))
            IR_MatrixExpression(that.innerDatatype, 2, 2, Array(Duplicate(det) * Duplicate(d), Duplicate(det) * Duplicate(b) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(c) * IR_IntegerConstant(-1), Duplicate(det) * Duplicate(a)), None)
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
            IR_MatrixExpression(that.innerDatatype, 3, 3, Array(Duplicate(A) / Duplicate(det), Duplicate(D) / Duplicate(det), Duplicate(G) / Duplicate(det), Duplicate(B) / Duplicate(det), Duplicate(E) / Duplicate(det), Duplicate(H) / Duplicate(det), Duplicate(C) / Duplicate(det), Duplicate(F) / Duplicate(det), Duplicate(I) / Duplicate(det)), None)
          case _ =>
            if (!Knowledge.experimental_CTPivotElimination && !isConstMatrix(that)) {
              if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using cofactors for inversion")
              cofactorInverse(that)
            } else {
              if (Knowledge.experimental_matrixDebugConfig) Logger.warn("using LU for inversion")
              val LUP = LUDecomp(that)

              if (Knowledge.experimental_matrixDebugConfig)
                exastencils.core.NodeCounter.countSubTree(LUP._1, "LUtmp_1", None, None)

              val out = LUDecompedInverse(LUP._1, LUP._2)

              if (Knowledge.experimental_matrixDebugConfig)
                exastencils.core.NodeCounter.countSubTree(out, "LUtmp_2", None, None)

              // for(a <- LUP._1.annotations)
              //   out.annotate(a._1, a._2)

              out
            }
        }
        simplifyMatrix(out)
      }

      case "Runtime"
             => Logger.error("'Runtime' matrix inversion chosen but in code path for compile time")
      case _ => Logger.error(s"""Unknown matrix inversion resolution strategy "${ matrixShape }"""")
    }
  }

  def simplifyMatrix(m : IR_MatrixExpression) : IR_MatrixExpression = {
    for (i <- 0 until m.rows) {
      for (j <- 0 until m.columns) {
        val simplified = evalNumExprWrapper(m.get(i, j))
        if (simplified.isDefined)
          m.set(i, j, simplified.get)
      }
    }
    m
  }

  def cofactorInverse(matrix : IR_MatrixExpression) : IR_MatrixExpression = {
    val inv_det = IR_IntegerConstant(1) / IR_CompiletimeMatOps.smallMatrixDeterminant(matrix)
    val tmp = IR_MatrixExpression(Some(matrix.innerDatatype.getOrElse(IR_RealDatatype)), matrix.rows, matrix.columns)
    for (row <- 0 until matrix.rows) {
      for (col <- 0 until matrix.columns) {
        tmp.set(col, row, IR_CompiletimeMatOps.calculateMatrixOfMinorsElement(matrix, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
      }
    }

    if (Knowledge.experimental_matrixDebugConfig)
      exastencils.core.NodeCounter.countSubTree(tmp, "cofactorsTmp", None, None)

    tmp
  }

  /** Method: calculate the inverse of a matrix per gaussJordan inverse
    *
    * @param matrix : IR_MatrixExpression, matrix to invert
    * @return inverse
    **/
  def gaussJordanInverse(matrix : IR_MatrixExpression) : IR_MatrixExpression = {
    //var matrix = Duplicate(that)
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

  def LUDecomp(matrix : IR_MatrixExpression) : (IR_MatrixExpression, Array[Int]) = {
    //var LU = IR_MatrixExpression(that.innerDatatype, that.rows, that.columns)
    var LU = Duplicate(matrix)
    val N : Int = matrix.columns
    if (N != matrix.rows) Logger.error("can only decompose quadratic matrices with LU")
    var P = new Array[Int](N)
    for (i <- 0 until N) P(i) = i
    var absA : IR_Expression = 0.0

    // for experimental_checkCTPivots check
    var ctPivots = IR_MatrixExpression(IR_RealDatatype, 1, N)
    var ctPivotsIdx = 0

    // for experimental_CTPivotElimination optimization
    var ctEliminatedPivots = ListBuffer[IR_VariableDeclaration]()
    var ctEliminatedPivotsIdx = 0

    for (i <- 0 until N) {
      if (Knowledge.experimental_matrixDebugConfig)
        exastencils.core.NodeCounter.countSubTree(LU, "LUTmp", None, None)

      if (Knowledge.experimental_CTPivoting) {
        // pivot
        var maxA : IR_Expression = IR_RealConstant(0.0)
        var imax : Int = i
        for (k <- i until N) {

          var value_at_ki : Double = 0.0
          var evaluatable = true
          try {
            value_at_ki = IR_CompiletimeMatOps.evalNumExpr(LU.get(k, i))
            //Logger.warn(s"${ value_at_ki }")
          } catch {
            case _ : EvaluationException => evaluatable = false
          }
          if (evaluatable) {
            // if evaluatable compare and switch if larger
            LU.set(k, i, IR_RealConstant(value_at_ki))
            absA = IR_RealConstant(value_at_ki.abs)
            if (absA.asInstanceOf[IR_RealConstant].value > maxA.asInstanceOf[IR_RealConstant].value) {
              maxA = absA
              imax = k
            }
          }
        }

        // no pivot element larger than 0 found or evaluatable -> take an Access and hope
        if (maxA.isInstanceOf[IR_RealConstant] && maxA.asInstanceOf[IR_RealConstant].value < 0.000000000001) {
          var k : Int = i
          var found : Boolean = false
          while (k < N && !found) {
            try {
              IR_CompiletimeMatOps.evalNumExpr(LU.get(k, i))
            } catch {
              case _ : EvaluationException =>
                found = true
                if (Knowledge.experimental_checkCTPivots) {
                  ctPivots.set(0, ctPivotsIdx, Duplicate(LU.get(k, i)))
                  ctPivotsIdx += 1
                }
                imax = k
            }
            k += 1
          }
        }

        if (imax != i) {
          if (Knowledge.experimental_matrixDebugConfig)
            Logger.warn(s"pivoting imax=${ imax } and i=${ i }")
          var tmp = P(i)
          P(i) = P(imax)
          P(imax) = tmp
          for (j <- 0 until N) {
            var tmp2 = LU.get(i, j)
            LU.set(i, j, LU.get(imax, j))
            LU.set(imax, j, tmp2)
          }
        }
      }

      // decompose
      /*
      for (j <- i + 1 until N) {
        val tmp1 = Duplicate(LU.get(j, i)) / Duplicate(LU.get(i, i))
        LU.set(j, i, tmp1)
        for (k <- i + 1 until N) {
          val tmp2 = Duplicate(LU.get(j, k)) - Duplicate(LU.get(j, i)) * Duplicate(LU.get(i, k))
          LU.set(j, k, tmp2)

          val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(LU.get(j, k))
          if (simplified.isDefined) LU.set(j, k, simplified.get)
        }
      }

       */
      for (j <- i + 1 until N) {
        val tmp1 = LU.get(j, i) / LU.get(i, i)
        LU.set(j, i, tmp1)

        // simplify if possible
        val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(LU.get(j, i))
        if (simplified.isDefined) LU.set(j, i, simplified.get)

        //LU.set(j,i,simplifyNumExpr(LU.get(j,i)))
        if (Knowledge.experimental_CTPivotElimination) {
          // retrieve pivot
          val pivAcc = IR_VariableAccess("mat_" + eliminatedPivotsOfMatrix + "_piv_" + ctEliminatedPivotsIdx, IR_RealDatatype)
          ctEliminatedPivots += IR_VariableDeclaration(pivAcc, Duplicate(LU.get(j, i)))
          ctEliminatedPivotsIdx += 1
          // replace with access
          LU.set(j, i, pivAcc)
        }

        for (k <- i + 1 until N) {
          val tmp2 = LU.get(j, k) - LU.get(j, i) * LU.get(i, k)
          LU.set(j, k, tmp2)

          // simplify if possible
          val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(LU.get(j, k))
          if (simplified.isDefined) LU.set(j, k, simplified.get)

          //LU.set(j,k,simplifyNumExpr(LU.get(j,k)))

        }
      }

    }

    if (Knowledge.experimental_checkCTPivots) {
      LU.annotate("checkCTPivots", ctPivots)
    }

    if (Knowledge.experimental_CTPivotElimination) {
      LU.annotate("CTPivotElimination", ctEliminatedPivots)
      eliminatedPivotsOfMatrix += 1
    }

    (LU, P)
  }

  def LUDecompedInverse(LU : IR_MatrixExpression, P : Array[Int]) : IR_MatrixExpression = {
    val N = LU.columns
    val LU_inv = IR_MatrixExpression(LU.innerDatatype, LU.rows, LU.rows)

    for (j <- 0 until N) {
      for (i <- 0 until N) {
        if (P(i) == j)
          LU_inv.set(i, j, IR_DoubleConstant(1.0));
        else
          LU_inv.set(i, j, IR_DoubleConstant(0.0));
        /*
                for (k <- 0 until i)
                  LU_inv.set(i, j, Duplicate(LU_inv.get(i, j)) - Duplicate(LU.get(i, k)) * Duplicate(LU_inv.get(k, j)))
        */
        for (k <- 0 until i) {
          LU_inv.set(i, j, LU_inv.get(i, j) - LU.get(i, k) * LU_inv.get(k, j))

          val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(LU_inv.get(i, j))
          if (simplified.isDefined) LU_inv.set(i, j, simplified.get)

          //        LU_inv.set(i,j,simplifyNumExpr(LU_inv.get(i,j)))

        }
      }
      /*
            for (i <- N - 1 to 0 by -1) {
              for (k <- i + 1 until N) {
                LU_inv.set(i, j, Duplicate(LU_inv.get(i, j)) - Duplicate(LU.get(i, k)) * Duplicate(LU_inv.get(k, j)))

              }

       */
      for (i <- N - 1 to 0 by -1) {
        for (k <- i + 1 until N) {
          LU_inv.set(i, j, LU_inv.get(i, j) - LU.get(i, k) * LU_inv.get(k, j))

          val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(LU_inv.get(i, j))
          if (simplified.isDefined) LU_inv.set(i, j, simplified.get)

          //LU_inv.set(i,j,simplifyNumExpr(LU_inv.get(i,j)))

        }
        LU_inv.set(i, j, LU_inv.get(i, j) / LU.get(i, i))
        val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(LU_inv.get(i, j))
        if (simplified.isDefined) LU_inv.set(i, j, simplified.get)
        //LU_inv.set(i,j,simplifyNumExpr(LU_inv.get(i,j)))

      }

    }
    if (LU.hasAnnotation("CTPivotElimination"))
      LU_inv.annotate("CTPivotElimination", LU.popAnnotationAs("CTPivotElimination"))

    if (Knowledge.experimental_checkCTPivots) {
      LU_inv.annotate("checkCTInversionPivots", LU.popAnnotationAs[IR_MatrixExpression]("checkCTInversionPivots"))
    }
    LU_inv
  }



  /** Method: calculates the inverse per schur complement and with saving the helper matrices in seperate variables
    *
    * @param that : IR_MatrixExpression, matrix to invert
    * @param dt   : IR_Datatype, inner datatype of the matrix
    * @param m    : Int, size of A matrix in schur block representation of that
    * @param n    : Int, size of D matrix in schur block representation of that
    * @param msi  : IR_MatrixShape, shape object, contains information about the shape of that
    * @param out  : IR_MatrixExpression, result of inversion
    * @return result of inversion
    *
    **/
  def schurWithHelpers(that : IR_MatrixExpression, dt : IR_Datatype, m : Int, n : Int, msi : IR_MatShape, out : IR_MatrixExpression) : IR_MatrixExpression = {

    // helper matrix declarations, to be added to statement later
    val hms = ListBuffer[IR_VariableDeclaration]()
    var S_inv = IR_VariableAccess("S_inv_" + tmpCounter, IR_MatrixDatatype(dt, m, m))
    var A_inv = IR_VariableAccess("A_inv_" + tmpCounter, IR_MatrixDatatype(dt, n, n))
    var CA_inv = IR_VariableAccess("CA_inv_" + tmpCounter, IR_MatrixDatatype(dt, m, n))
    var CA_invB = IR_VariableAccess("CA_invB_" + tmpCounter, IR_MatrixDatatype(dt, m, m))
    var A_invB = IR_VariableAccess("A_invB_" + tmpCounter, IR_MatrixDatatype(dt, n, m))
    var A_invBS_inv = IR_VariableAccess("A_invBS_inv_" + tmpCounter, IR_MatrixDatatype(dt, n, m))
    var S_invCA_inv = IR_VariableAccess("S_invCA_inv_" + tmpCounter, IR_MatrixDatatype(dt, m, n))
    var A_invBS_invCA_inv = IR_VariableAccess("A_invBS_invCA_inv_" + tmpCounter, IR_MatrixDatatype(dt, n, n))
    tmpCounter += 1

    // build new matrix structure for submatrix A:
    var A = copySubMatrix(that, 0, 0, n, n)
    val shapeA = IR_MatShape(msi.shape("A"))
    if (shapeA.shape == "blockdiagonal")
      shapeA.addInfo("block", msi.size("Ablock"))
    hms += IR_VariableDeclaration(A_inv, inverse(A, shapeA))
    //IR_GeneralSimplify.doUntilDoneStandalone(A_inv)

    // other block submatrices
    val B = copySubMatrix(that, 0, n, n, m)
    val C = copySubMatrix(that, n, 0, m, n)
    val D = copySubMatrix(that, n, n, m, m)

    // calculate S
    hms += (IR_VariableDeclaration(CA_inv, mult(IR_Multiplication(C, A_inv))))
    hms += IR_VariableDeclaration(CA_invB, mult(IR_Multiplication(CA_inv, B)))
    val S = sub(IR_Subtraction(D, CA_invB))

    // invert S
    // for schur complement inversion multiple structure information is necessary(n and m, blocksize of A, S is probably always filled) in case  m is larger than 1 (default should be "filled")
    hms += IR_VariableDeclaration(S_inv, inverse(S, IR_MatShape("filled")))

    // copy result blocks to 'out' matrix
    hms += IR_VariableDeclaration(A_invB, mult(IR_Multiplication(A_inv, B)))

    // upper right
    hms += (IR_VariableDeclaration(A_invBS_inv, mult(IR_Multiplication(A_invB, S_inv))))
    pasteSubMatrix(negative(A_invBS_inv), out, 0, n)

    // lower left
    hms += (IR_VariableDeclaration(S_invCA_inv, negative(mult(IR_Multiplication(S_inv, CA_inv)))))
    pasteSubMatrix(S_invCA_inv, out, n, 0)

    // lower right
    pasteSubMatrix(S_inv, out, n, n)

    // upper left
    hms += (IR_VariableDeclaration(A_invBS_invCA_inv, mult(IR_Multiplication(A_invBS_inv, CA_inv))))
    pasteSubMatrix(add(IR_Addition(A_inv, A_invBS_invCA_inv)), out, 0, 0)

    // helper matrices to inverted expression
    out.annotate("helperMatrices", hms)

    if (Knowledge.experimental_matrixDebugConfig) {
      for (h <- hms) {
        exastencils.core.NodeCounter.countSubTree(h.initialValue.getOrElse(Logger.error("helper without init")), "helper " + h.name, None, None)
      }
    }

    out
  }

  def schur(that : IR_MatrixExpression, dt : IR_Datatype, m : Int, n : Int, msi : IR_MatShape, out : IR_MatrixExpression) : IR_MatrixExpression = {
    var A = copySubMatrix(that, 0, 0, n, n)

    // build new matrix structure for submatrix A:
    val shapeA = IR_MatShape(msi.shape("A"))
    if (shapeA.shape == "blockdiagonal")
      shapeA.addInfo("block", msi.size("Ablock"))
    var A_inv = inverse(A, shapeA)
    //IR_GeneralSimplify.doUntilDoneStandalone(A_inv)

    // calculate S
    val B = copySubMatrix(that, 0, n, n, m)
    val C = copySubMatrix(that, n, 0, m, n)
    val D = copySubMatrix(that, n, n, m, m)
    val CA_inv = mult(C, A_inv)
    val CA_invB = mult(CA_inv, B)
    val S = sub(D, CA_invB)

    // variable accesses for A size matrices

    // invert S
    // for schur complement inversion multiple structure information is necessary(n and m, blocksize of A, S is probably always filled) in case  m is larger than 1 (default should be "filled")
    val S_inv = inverse(S, IR_MatShape("filled"))

    // copy result blocks to 'out' matrix
    val lowerLeft = negative(mult(S_inv, CA_inv))
    val lowerRight = S_inv
    val A_invB = mult(A_inv, B)
    val A_invBS_inv = mult(A_invB, S_inv)
    val upperRight = negative(A_invBS_inv)
    val upperLeft = add(A_inv, mult(A_invBS_inv, CA_inv))
    pasteSubMatrix(upperLeft, out, 0, 0)
    pasteSubMatrix(upperRight, out, 0, n)
    pasteSubMatrix(lowerLeft, out, n, 0)
    pasteSubMatrix(lowerRight, out, n, n)
    out
  }



  def genColPivoting(m : IR_VariableAccess, curColumn : Int, N : Int) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()

    val k = IR_VariableAccess("k", IR_IntegerDatatype)
    val maxColumn = IR_VariableAccess("maxCol", IR_IntegerDatatype)
    val absA = IR_VariableAccess("absA", IR_RealDatatype)
    val maxA = IR_VariableAccess("maxA", IR_RealDatatype)
    stmts += IR_Assignment(maxColumn, curColumn)
    stmts += IR_Assignment(maxA, IR_HighDimAccess(m, IR_ConstIndex(curColumn, curColumn)))
    stmts += IR_ForLoop(IR_VariableDeclaration(k, curColumn), IR_Lower(k, N), IR_PreIncrement(k), ListBuffer[IR_Statement](
      IR_Assignment(absA, IR_FunctionCall(IR_ExternalFunctionReference.fabs, ListBuffer[IR_Expression](IR_HighDimAccess(m, IR_ExpressionIndex(k, curColumn))))),
      IR_IfCondition(IR_Greater(absA, maxA), ListBuffer[IR_Statement](IR_Assignment(maxA, absA), IR_Assignment(maxColumn, k)), ListBuffer[IR_Statement]())
    ))
    val pointerAdd = IR_Addition(IR_AddressOf(IR_HighDimAccess(m, IR_ExpressionIndex(curColumn, 0))) + N)
    pointerAdd.annotate(IR_GenerateRuntimeInversion.pointerArithmetic)
    stmts += IR_IfCondition(IR_Neq(curColumn, maxColumn), ListBuffer[IR_Statement](
      IR_FunctionCall(IR_ExternalFunctionReference("std::swap_ranges", IR_UnitDatatype), ListBuffer[IR_Expression](
        IR_AddressOf(IR_HighDimAccess(m, IR_ExpressionIndex(curColumn, 0))),
        pointerAdd,
        IR_AddressOf(IR_HighDimAccess(m, IR_ExpressionIndex(maxColumn, 0)))
      )),
    ))
    stmts
  }

  def mirrorLU(A : IR_MatrixExpression) : ListBuffer[IR_Statement] = {
    val N = A.rows

    var copies = ListBuffer[IR_MatrixExpression]()
    copies += Duplicate(A)
    for (i <- 1 to N) copies += IR_MatrixSolveOps.unit(N)
    var copyAccs = ListBuffer[IR_VariableAccess]()
    for (i <- 0 until N - 1) {
      val acc = IR_VariableAccess("copies_" + i, A.datatype)
      copyAccs += acc
    }

    // fill not iterated areas with entries from last copy
    for (c <- 1 until N) {
      for (i <- 0 until c) {
        copies(c).set(i, i, IR_HighDimAccess(copyAccs(c - 1), IR_ExpressionIndex(i, i)))
        for (j <- i + 1 until N) {
          copies(c).set(i, j, IR_HighDimAccess(copyAccs(c - 1), IR_ExpressionIndex(i, j)))
        }
      }
      for (i <- 0 until c - 1) {
        for (j <- i + 1 until N) {
          copies(c).set(i, j, IR_HighDimAccess(copyAccs(c - 1), IR_ExpressionIndex(i, j)))
        }
      }
    }

    var copies_idx = 1
    for (i <- 0 until N) {
      for (j <- i + 1 until N) {
        //copies(copies_idx).set(j, i,  copies(copies_idx - 1).get(j, i) / copies(copies_idx - 1).get(i, i))
        copies(copies_idx).set(j, i, IR_HighDimAccess(copyAccs(copies_idx - 1), IR_ExpressionIndex(j, i)) / IR_HighDimAccess(copyAccs(copies_idx - 1), IR_ExpressionIndex(i, i)))
        for (k <- i + 1 until N) {
          //         copies(copies_idx).set(j, k, copies(copies_idx - 1).get(j, k) - copies(copies_idx).get(j, i) * copies(copies_idx - 1).get(i, k))
          copies(copies_idx).set(j, k, IR_HighDimAccess(copyAccs(copies_idx - 1), IR_ExpressionIndex(j, k)) - copies(copies_idx).get(j, i) * IR_HighDimAccess(copyAccs(copies_idx - 1), IR_ExpressionIndex(i, k)))
        }
      }
      copies_idx += 1
    }

    var stmts = ListBuffer[IR_Statement]()
    stmts += IR_VariableDeclaration(IR_RealDatatype, "absA")
    stmts += IR_VariableDeclaration(IR_RealDatatype, "maxA")
    stmts += IR_VariableDeclaration(IR_IntegerDatatype, "maxCol")
    for (i <- 0 until N) {
      val acc = IR_VariableAccess("copies_" + i, A.datatype)
      val decl = IR_VariableDeclaration(Duplicate(acc), Duplicate(copies(i)).asInstanceOf[IR_Expression]).asInstanceOf[IR_Statement]
      stmts += decl

      stmts ++= genColPivoting(Duplicate(acc), i, N)
    }
    for(i <- 0 until N) NodeCounter.countSubTree( copies(i),"mirror_" + i, None, None)
    stmts
  }
}
