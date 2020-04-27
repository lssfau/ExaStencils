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
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir._

// methods to determine whether a matrix is a diagonal-, blockdiagonal-, schurmatrix at compiletime
//TODO matrices have to be filled with constants
object IR_DetermineMatrixStructure {

  def evalIntegralWithAccesses(expr : IR_Expression) : Long = expr match {
    case IR_IntegerConstant(v)                                => v
    case IR_Addition(sums : ListBuffer[IR_Expression])        => sums.view.map(s => evalIntegralWithAccesses(s)).sum
    case IR_Subtraction(l : IR_Expression, r : IR_Expression) => evalIntegralWithAccesses(l) - evalIntegralWithAccesses(r)
    case IR_Multiplication(facs : ListBuffer[IR_Expression])  => facs.view.map(s => evalIntegralWithAccesses(s)).product
    case IR_Division(l : IR_Expression, r : IR_Expression)    => evalIntegralWithAccesses(l) / evalIntegralWithAccesses(r)
    case IR_Modulo(l : IR_Expression, r : IR_Expression)      => evalIntegralWithAccesses(l) % evalIntegralWithAccesses(r)
    case IR_Minimum(l : ListBuffer[IR_Expression])            => l.view.map(e => evalIntegralWithAccesses(e)).min
    case IR_Maximum(l : ListBuffer[IR_Expression])            => l.view.map(e => evalIntegralWithAccesses(e)).max
    //case IR_VariableAccess(_,IR_IntegerDatatype)              =>
    //case IR_HighDimAccess(base @ IR_VariableAccess(_, IR_MatrixDatatype(_,_,_)), idx @ IR_ExpressionIndex(_)) => evalIntegralWithAccesses(base.get(evalIntegralWithAccesses(idx.indices(0)).toInt,evalIntegralWithAccesses(idx.indices(1)).toInt))
    case _                                                    => Logger.error("unexpected type argument: " + expr)
  }


  def evaluateEntry(mat : IR_Expression, i : Int, j : Int) : Double = {
    var en : Double = 0
    mat match {
      case x @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))) =>
        var dt = x.datatype.resolveBaseDatatype
        if (dt == IR_IntegerDatatype)
          en = IR_SimplifyExpression.evalIntegral(IR_BasicMatrixOperations.getElem(x, i, j))
        else
          en = IR_SimplifyExpression.evalFloating(IR_BasicMatrixOperations.getElem(x, i, j))
      case _                                                                                   => Logger.error("unexpected argument type: " + mat + ", expected matrix expression or variable")
    }
    return en
  }

  // determine structure of 'matrix' (which must have compiletime evaluatable entries) and return it as a String + more specific structure information like blocksizes in case of Schur or Blockdiagonal matrices
  def isOfStructure(matrix : IR_Expression) : (String, Int, String, Int) = {
    var blocksize_A = 0
    var blocksize_D = 0
    matrix match {
      case mat @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _)) =>
        var size = IR_BasicMatrixOperations.getSize(mat)

        mat.datatype.resolveBaseDatatype match {
          case dt @ (IR_IntegerDatatype | IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype) =>

            // count blocksize of schur block
            var cont = true
            while ((blocksize_D < size._1) && cont == true) {
              // count on multiple lines to avoid mistakes due to random zeros
              var en0 : Double = evaluateEntry(mat, 0, size._1 - blocksize_D - 1)
              var en1 : Double = evaluateEntry(mat, size._1 - blocksize_D - 1, 0)
              var en2 : Double = evaluateEntry(mat, 1, size._1 - blocksize_D - 1)
              var en3 : Double = evaluateEntry(mat, size._1 - blocksize_D - 1, 1)

              // only if all lanes are zero break
              if (en0 == 0.0 && en1 == 0 && en2 == 0 && en3 == 0) {
                    cont = false
              }
              else
                blocksize_D += 1
            }
            // if we reached other end of matrix: its a Filled matrix
            if(blocksize_D == size._1 - 1) return ("Filled", -1, "", -1)

            // count blocksize of blockmatrix A if present (Schur form with size(mat) - blocksize_D as size of A block (upper left block in schur form)
            cont = true
            while (blocksize_A <= (size._1 - blocksize_D)/2 + 1 && cont == true) {
             var en0 : Double = evaluateEntry(mat, 0, blocksize_A)
              var en1 : Double = evaluateEntry(mat, blocksize_A, 0)

              if (en0 == 0.0 && en1 == 0) {
               cont = false
              } else {
                blocksize_A += 1
              }
            }
            // if more than half of A block is reached, blockdiagonal is not possible anymore -> Schur form with filled A block is filled matrix
            if(blocksize_A == (size._1 - blocksize_D)/2 + 1) {
              return ("Filled", -1, "", -1)
            }

            // average blocksize over block
            //blocksize_A = aestimateBlocksize(mat)

            // setup mask to check all entries of mat
            var mask = new Array[Array[Boolean]](size._1)
            for (i <- 0 until size._1) {
              mask(i) = new Array[Boolean](size._2)
            }
            var l = 0
            while (l < size._1) {
              for (i <- l until scala.math.min(l + blocksize_A, size._1)) {
                for (j <- l until scala.math.min(l + blocksize_A, size._2)) {
                  mask(i)(j) = true
                }
              }
              l += blocksize_A
            }
            for (i <- 0 until blocksize_D) {
              for (j <- 0 until size._1) {
                mask(size._1 - blocksize_D + i)(j) = true
                mask(j)(size._1 - blocksize_D + i) = true
              }
            }

            for (i <- 0 until size._1) {
              for (j <- 0 until size._2) {
                var en : Double = evaluateEntry(mat, i, j)

                // entry should be zero but is not -> can not invert with specific algorithm
                if (!mask(i)(j) && en != 0.0)
                  return ("Filled", -1, "", -1)
              }

            }
            // no schur form
            if (blocksize_D == 0) {
              // size of A block: A block is the whole matrix with blocksize_D == 0 (no schur form)
              if (blocksize_A == 1) {
                return ("Diagonal", -1, "", -1)
              }
              else if (blocksize_A == size._1)
                return ("Filled", -1, "", -1)
              else {
                return ("Blockdiagonal", blocksize_A, "", -1)
              }
            }
              // schur form
            else {
              // form of A block in schur form
              if (blocksize_A == 1) {
                return ("Schur", size._1 - blocksize_D, "Diagonal", -1)
              }
              else if (blocksize_A == size._1) {
                return ("Schur", size._1 - blocksize_D, "Filled", -1)
              }
              else {
                return ("Schur", size._1 - blocksize_D, "Blockdiagonal", blocksize_A)
              }
            }
          case _                                                                                  => Logger.error("unexpected datatype: " + mat.datatype.resolveBaseDatatype)
        }
      case _                                                                                       => Logger.error("unexpected argument type: " + matrix + ", expected matrix variable or expression")
    }

  }

  // do the same at runtime, entries do not have to be compiletime evaluatable
  def isOfStructureRuntime(matrix : IR_VariableAccess) : IR_Scope = {
    var func = IR_Scope(Nil)
    func
  }

  // aestimate the blocksize by taking the whole first block of a matrix into account
  //TODO blocksize 1 always has confirmation 1
  def aestimateBlocksize(matrix : IR_Expression) : Int = {
    matrix match {
      case mat @ (IR_MatrixExpression(_, _, _) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))) =>

        var size = IR_BasicMatrixOperations.getSize(matrix)
        var aestimators = new Array[Float](size._1)
        var accumulator = 0
        if (evaluateEntry(matrix, 0, 0) == 0) {
          aestimators(0) = 0
        }
        else {
          aestimators(0) = 1
        }
        for (i <- 1 until size._1/2) {
          for (j <- 0 until i) {
            if (evaluateEntry(matrix, i, j) != 0)
              accumulator += 1
            if (evaluateEntry(matrix, i, j) != 0)
              accumulator += 1
          }

          if (evaluateEntry(matrix, i, i) != 0)
            accumulator += 1

          aestimators(i) = accumulator.toFloat / ((i + 1) * (i + 1)).toFloat
        }

        var max = -1.0
        var maxIdx = -1
        for (i <- 0 until size._1) {
          if (aestimators(i) >= max) {
            max = aestimators(i)
            maxIdx = i
          }
        }
        print(aestimators)
        return maxIdx + 1
      case _                                                                                       => Logger.error("unexpected argument type: " + matrix)
    }
  }

}
