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

import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixNodeUtilities._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.logger.Logger
import exastencils.optimization.ir._
import exastencils.util.ir._

// simple operations with matrices like addition or multiplication
object IR_BasicMatrixOperations {
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
        case sc if (isScalar(sc))                                                    => sc
        case _                                                                       => Logger.error(s"Argument is of unexpected type ${ exp.getClass.getTypeName }: $exp")
      }
    } catch {
      case e : ArrayIndexOutOfBoundsException => throw new ArrayIndexOutOfBoundsException
    }
  }

  def duplicateExpressions(access : IR_Expression, dt : IR_MatrixDatatype) = {
    var expressions = ListBuffer[IR_Expression]()
    for (row <- 0 until dt.sizeM)
      for (col <- 0 until dt.sizeN)
        expressions += IR_HighDimAccess(Duplicate(access), IR_ConstIndex(row, col))
    expressions.toArray
  }

  def getSize(in : IR_Expression) = {
    in match {
      case me : IR_MatrixExpression                                                  => (me.rows, me.columns)
      case va : IR_VariableAccess if (va.datatype.isInstanceOf[IR_MatrixDatatype])   => (va.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, va.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
      case s : IR_ScalarDatatype                                                     => (1, 1)
      case sva : IR_VariableAccess if (sva.datatype.isInstanceOf[IR_ScalarDatatype]) => (1, 1)
      case mdt : IR_MatrixDatatype => (mdt.sizeM, mdt.sizeN)
      case other                                                                                                                       => Logger.error("argument is of unexpected type: " + in)
    }
  }

  // copy and return a submatrix of 'from' of size 'n_rows' x 'n_cols' at position 'offset_rows', 'offset_cols'
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
        submatrix.set(i - offset_rows, j - offset_cols, n)
      }
    }
    submatrix
  }

  // insert a matrix 'source' of size 'n_rows' x 'n_cols' at position 'offset_rows', 'offset_cols' in 'target'
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
        var n = Duplicate(getElem(source, i - offset_rows, j - offset_cols))
        target.set(i, j, n)
      }
    }
  }

  // compiletime determinant per laplace expansion
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
    return smallMatrixDeterminant(tmp)
  }

  // transpose a matrix passed by a variable
  def transpose(source : IR_VariableAccess) : IR_MatrixExpression = {
    var ssize = IR_BasicMatrixOperations.getSize(source)
    var out = IR_MatrixExpression(source.datatype.resolveBaseDatatype, ssize._2, ssize._1)
    for (i <- 0 until ssize._1) {
      for (j <- 0 until ssize._2) {
        out.set(j, i, Duplicate(IR_BasicMatrixOperations.getElem(source, i, j)))
      }
    }
    out
  }

  // multiply two vectors/matrices per dot product
  def dotProduct(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      case (l @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), r @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
        var lsize = getSize(l)
        var rsize = getSize(r)
        (lsize, rsize) match {
          case ((lrows, lcols), (rrows, rcols)) if (lcols == rcols && lrows == rrows) =>
            var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), 1, 1)
            out.set(0, 0, IR_IntegerConstant(0))
            for (i <- 0 until rrows) {
              for (j <- 0 until rcols) {
                out.set(0, 0, IR_Addition(Duplicate(out.get(0, 0)), IR_Multiplication(getElem(l, i, j), getElem(r, i, j))))
              }
            }
            out
          case ((1, lcols), (rrows, 1)) if (lcols == rrows)                           =>
            var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), 1, 1)
            out.set(0, 0, IR_IntegerConstant(0))
            for (i <- 0 until rrows) {
              out.set(0, 0, IR_Addition(Duplicate(out.get(0, 0)), IR_Multiplication(getElem(l, 0, i), getElem(r, i, 0))))
            }
            out
          case _                                                                      => Logger.error("unsupported argument form: " + lsize + ", " + rsize + ", expected arguments of the same size")
        }
      case _                                                                                                                                                                              => Logger.error("unexpected argument types: " + left + ", " + right + ", expected matrix variables or expressions as input")
    }

  }

  // multiply two columnvectors of size 3 per crossProduct
  def crossProduct(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      case (l @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _)), r @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _))) =>
        var lsize = getSize(left)
        var rsize = getSize(right)
        if (lsize._1 != 3 || lsize._2 != 1 || rsize._1 != 3 || rsize._2 != 1)
          Logger.error("cross product only supported for two column vectors of size 3, arguments are of form: " + lsize + "," + rsize)
        var out = IR_MatrixExpression(IR_ResultingDatatype(l.datatype, r.datatype), 3, 1)
        out.set(0, 0, IR_Subtraction(IR_Multiplication(getElem(left, 1, 0), getElem(right, 2, 0)), IR_Multiplication(getElem(left, 2, 0), getElem(right, 1, 0))))
        out.set(1, 0, IR_Subtraction(IR_Multiplication(getElem(left, 2, 0), getElem(right, 0, 0)), IR_Multiplication(getElem(left, 0, 0), getElem(right, 2, 0))))
        out.set(2, 0, IR_Subtraction(IR_Multiplication(getElem(left, 0, 0), getElem(right, 1, 0)), IR_Multiplication(getElem(left, 1, 0), getElem(right, 0, 0))))
        out
      case _                                                                                                                                                                              => Logger.error("unexpected arguments: " + left + ", " + right + ", expected accesses to matrix variables or matrix expressions")
    }
  }

  // multiplicate two IR_MatrixExpressions
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

  // multiply multiple matrices of an IR_Multiplication
  def mult(mult : IR_Multiplication) : IR_MatrixExpression = {
    var result = IR_MatrixExpression(IR_IntegerDatatype, 1, 1)
    var firstMatrix = mult.factors.find(fac => isMatrix(fac)).getOrElse(Logger.error("no matrix in factors!"))
    var firstMatrixIdx = mult.factors.indexOf(firstMatrix)
    mult.factors.remove(firstMatrixIdx)
    mult.factors.prepend(firstMatrix)
    var tmp = mult.factors(0) match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        IR_MatrixNodeUtilities.accessToExpression(va)
      case x : IR_MatrixExpression                               =>
        Duplicate(x)
      case _                                                     =>
        Logger.error("unexpected type: " + mult.factors(0))
    }
    for (f <- 1 until mult.factors.length) {
      result = mult.factors(f) match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
          IR_BasicMatrixOperations.mult(tmp, IR_MatrixNodeUtilities.accessToExpression(va))
        case x : IR_MatrixExpression                    =>
          IR_BasicMatrixOperations.mult(tmp, x)
        case s if (isScalar(s)) =>
          IR_BasicMatrixOperations.elementwiseMultiplication(tmp, s)
        case _                                          =>
          Logger.error("unexpected type: " + mult.factors(f))
      }
      tmp = Duplicate(result)
    }
    result
  }

  // add two IR_MatrixExpressions, internal add function to use in e.g. compiletime inversions
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

  // add multiple matrices(summands of a IR_Addition) add two matrices of a IR_ElementwiseAddition
  def add(addition : IR_Expression) : IR_MatrixExpression = {
    addition match {
      case a : IR_Addition                          =>
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
      case eaddition @ IR_ElementwiseAddition(_, _) =>
        Logger.error("elementwise addition not yet supported")
      case _                                        => Logger.error("unexpected type: " + addition + ", expected IR_Addition or IR_ElementwiseAddition")
    }

  }

  //Internal subtraction: sub two IR_MatrixExpressions, internal sub function to use in e.g compiletime inversion
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

  //User subtraction: subtract two operands in a IR_Subtraction or IR_ElementwiseSubtraction if one of them is a matrix
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
              case x : IR_MatrixExpression                               =>
                IR_BasicMatrixOperations.sub(IR_Subtraction(negative(x), IR_Negative(sub.left)))
              case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
                IR_BasicMatrixOperations.sub(IR_Subtraction(negative(IR_MatrixNodeUtilities.accessToExpression(va)), IR_Negative(sub.left)))
              case _                                                     =>
                Logger.error(s"unexpected argument of type: ${sub.right.datatype} and basetype: ${sub.right.datatype.resolveBaseDatatype}, left side is of type: ${sub.left.datatype} and basetype: ${sub.left.datatype.resolveBaseDatatype}")
            }
          case _                                                    => Logger.error("unexpected argument: " + sub.left + ", expected matrix or scalar")
        }
      case esub : IR_ElementwiseSubtraction =>
        Logger.error("IR_ElementwiseSubtraction not yet supported")
      case _                                =>
        Logger.error("unexpected argument: " + subtraction + ", expected IR_Subtraction or IR_ElementwiseSubtraction")
    }
  }

  // multiplicate two matrices or a scalar and a matrix per element
  def elementwiseMultiplication(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar, matrix x matrix
      case (scalar, matrix) if (isScalar((scalar)) && isMatrix(matrix))                   =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case (matrix, scalar) if (isScalar((scalar)) && isMatrix(matrix))                   =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
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
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Multiplication(getElem(matrixLeft, i, j), getElem(matrixRight, i, j)))
          }
        }
        out
      case _                                                                                                                                    => Logger.error("unexpected argument combination: " + (left, right))
    }
  }

  // divide two matrices or a scalar and a matrix per element
  def elementwiseDivision(left : IR_Expression, right : IR_Expression) : IR_MatrixExpression = {
    (left, right) match {
      // scalar x matrix, matrix x scalar, matrix x matrix
      case (scalar @ (IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype)), matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(scalar, i, j), getElem(matrix, i, j)))
          }
        }
        out
      case (matrix @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), scalar @ (IR_VariableAccess(_, IR_RealDatatype | IR_DoubleDatatype | IR_FloatDatatype | IR_IntegerDatatype))) =>
        var size = getSize(matrix)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(matrix, i, j), getElem(scalar, i, j)))
          }
        }
        out
      case (matrixLeft @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))), matrixRight @ ((IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)))))                                       =>
        var size = getSize(matrixLeft)
        var sizeR = getSize(matrixRight)
        if (size != sizeR)
          Logger.error("sizes do not match: " + size + " vs " + sizeR)
        var out = IR_MatrixExpression(IR_ResultingDatatype(left.datatype, right.datatype), size._1, size._2)
        for (i <- 0 until size._1) {
          for (j <- 0 until size._2) {
            out.set(i, j, IR_Division(getElem(matrixLeft, i, j), getElem(matrixRight, i, j)))
          }
        }
        out
      case _                                                                                                                                                                           => Logger.error("unexpected argument combination")
    }
  }

  //TODO elementwise power does not parse
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

  // return a matrix with negative elements of input
  def negative(that : IR_MatrixExpression) : IR_MatrixExpression = {
    var out = IR_MatrixExpression(that.innerDatatype, that.rows, that.columns)
    for (i <- 0 until that.rows) {
      for (j <- 0 until that.columns) {
        //        out.set(i,j,IR_Subtraction(IR_RealConstant(0),Duplicate(that.get(i,j))))
        out.set(i, j, IR_Negative(Duplicate(that.get(i, j))))
      }
    }
    out
  }

  // return the sum of the diagonal elements of a matrix
  def trace(matrix : IR_Expression) : IR_Addition = {
    var sum = IR_Addition(IR_IntegerConstant(0))

    matrix match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        var s = getSize(va)
        if (s._1 != s._2)
          Logger.error("trace only for quadratic matrices supported, matrix is of form: " + s)
        for (i <- 0 until s._1) {
          sum = IR_Addition(sum, getElem(va, i, i))
        }
      case x @ IR_MatrixExpression(_, _, _)                      =>
        if (x.rows != x.columns)
          Logger.error("trace only for quadratic matrices supported, matrix is of form: " + (x.rows, x.columns))
        for (i <- 0 until x.rows) {
          sum = IR_Addition(sum, getElem(x, i, i))
        }
    }
    sum
  }
}

// methods to determine a inverse at compiletime
object IR_CompiletimeInversion {
  // head function that branches to specific inversions
  def inverse(that : IR_MatrixExpression, msi : IR_MatShape) : IR_MatrixExpression = {
    var matrixStructure = msi.shape
    if (that.rows != that.columns)
      Logger.error("inversion of non quadratic matrices not supported.")
    matrixStructure match {
      case "diagonal"
      => {
        val tmp = Duplicate(that)
        for (row <- 0 until that.rows) {
          tmp.set(row, row, IR_Division(IR_RealConstant(1.0), that.get(row, row)))
        }
        tmp
      }
      case "blockdiagonal"
      => {
        if (that.rows < 4)
          Logger.error("Blockdiagonal inversion not applicable for matrices < 4, use diagonal")
        var out = Duplicate(that)
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
              var subMatrix = IR_BasicMatrixOperations.copySubMatrix(that, offset, offset, sizeA, sizeA)

              // invert with GaussJordan method
              //var subMatrix_inv = gaussJordanInverse(subMatrix)
              var subMatrix_inv = inverse(subMatrix, IR_MatShape("filled"))

              // copy to out matrix
              IR_BasicMatrixOperations.pasteSubMatrix(subMatrix_inv, out, offset, offset)
            }
          }
        }
        out
      }
      case "schur"
      => {
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
          import exastencils.baseExt.ir.IR_BasicMatrixOperations.copySubMatrix
          import exastencils.baseExt.ir.IR_BasicMatrixOperations.mult
          import exastencils.baseExt.ir.IR_BasicMatrixOperations.sub
          import exastencils.baseExt.ir.IR_BasicMatrixOperations.pasteSubMatrix
          import exastencils.baseExt.ir.IR_BasicMatrixOperations.negative
          import exastencils.baseExt.ir.IR_BasicMatrixOperations.add
/*
          //TODO seperate implementation
          val dt = that.datatype.resolveBaseDatatype
          val helperMatrices = ListBuffer[IR_VariableDeclaration]()
          if(Knowledge.experimental_schurWithHelper) {
            var hm_S_inv = IR_VariableAccess("S_inv", IR_MatrixDatatype(dt, m, m))
            var hm_CA_inv = IR_VariableAccess("CA_inv", IR_MatrixDatatype(dt, m, n))
            var hm_CA_invB = IR_VariableAccess("CA_invB", IR_MatrixDatatype(dt, m, m))
            var hm_A_invB = IR_VariableAccess("A_invB", IR_MatrixDatatype(dt, n, m))
            var hm_A_invBS_inv = IR_VariableAccess("A_invBS_inv", IR_MatrixDatatype(dt, n, m))
            var hm_S_invCA_inv = IR_VariableAccess("S_invCA_inv", IR_MatrixDatatype(dt, m, n))
            var hm_A_invBS_invCA_inv = IR_VariableAccess("A_invBS_invCA_inv", IR_MatrixDatatype(dt, n, n))
            for(m <- List())
          }
          */
          var A = copySubMatrix(that, 0, 0, n, n)

          // build new matrix structure for submatrix A:
          val shapeA = IR_MatShape(msi.shape("A"))
          if(shapeA.shape == "blockdiagonal")
              shapeA.addInfo("block", msi.size("Ablock"))
          var A_inv = inverse(A, shapeA)
          IR_GeneralSimplify.doUntilDoneStandalone(A_inv)

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
        }
        out
      }
      case "cofactors"
      => {
        val inv_det = IR_IntegerConstant(1) / IR_BasicMatrixOperations.smallMatrixDeterminant(that)
        val tmp = IR_MatrixExpression(Some(that.innerDatatype.getOrElse(IR_RealDatatype)), that.rows, that.columns)
        for (row <- 0 until that.rows) {
          for (col <- 0 until that.columns) {
            tmp.set(col, row, IR_BasicMatrixOperations.calculateMatrixOfMinorsElement(that, row, col) * IR_DoubleConstant(math.pow(-1, row + col)) * inv_det)
          }
        }
        tmp
      }
      case "gaussJordan"
      => {
        gaussJordanInverse(that)
      }
      case "filled"
      => {
        that.rows match {
          case 1 =>
            IR_MatrixExpression(that.innerDatatype, 1, 1, Array(IR_Division(IR_RealConstant(1.0), that.get(0, 0))))

          case 2 =>
            val a = that.get(0, 0)
            val b = that.get(0, 1)
            val c = that.get(1, 0)
            val d = that.get(1, 1)
            val det : IR_Expression = IR_Division(IR_RealConstant(1.0), (a * d) - (b * c))
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
            gaussJordanInverse(that)
        }
      }

      case "Runtime"
             => Logger.error("'Runtime' matrix inversion chosen but in code path for compile time")
      case _ => Logger.error(s"""Unknown matrix inversion resolution strategy "${ matrixStructure }"""")
    }
  }

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
}
