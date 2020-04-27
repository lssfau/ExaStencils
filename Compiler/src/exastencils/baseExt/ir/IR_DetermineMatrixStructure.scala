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

import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir._

// methods to determine whether a matrix is a diagonal-, blockdiagonal-, schurmatrix at compiletime
//TODO matrices have to be filled with constants
object IR_DetermineMatrixStructure {

  def isOfStructure(matrix : IR_Expression) : (String, Int, String, Int) = {
    var blocksize_A = 0
    var blocksize_D = 0
    matrix match {
      case mat @ (IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) | IR_MatrixExpression(_, _, _)) =>
        var size = IR_BasicMatrixOperations.getSize(mat)

        mat.datatype.resolveBaseDatatype match {
          case dt @ (IR_IntegerDatatype | IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype) =>
            var cont = true
            while (blocksize_A < size._1 && cont == true) {
              cont = false
              var en : Double = 0
              if (dt == IR_IntegerDatatype)
                en = IR_SimplifyExpression.evalIntegral(IR_BasicMatrixOperations.getElem(mat, 0, blocksize_A))
              else
                en = IR_SimplifyExpression.evalFloating(IR_BasicMatrixOperations.getElem(mat, 0, blocksize_A))

              if (en != 0.0) {
                cont = true
                blocksize_A += 1
              }
            }

            cont = true
            while ((blocksize_D < size._1 - blocksize_A) && cont == true) {
              cont = false
              var en : Double = 0
              if (dt == IR_IntegerDatatype)
                en = IR_SimplifyExpression.evalIntegral(IR_BasicMatrixOperations.getElem(mat, 0, size._1 - blocksize_D - 1))
              else
                en = IR_SimplifyExpression.evalFloating(IR_BasicMatrixOperations.getElem(mat, 0, size._1 - blocksize_D - 1))

              if (en != 0.0) {
                cont = true
                blocksize_D += 1
              }

            }

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
                var en : Double = 0.0
                if (dt == IR_IntegerDatatype)
                  en = IR_SimplifyExpression.evalIntegral(IR_BasicMatrixOperations.getElem(mat, i, j))
                else
                  en = IR_SimplifyExpression.evalFloating(IR_BasicMatrixOperations.getElem(mat, i, j))

                if (!mask(i)(j) && en != 0.0)
                  return ("Filled", -1, "",-1)
              }

            }
            if (blocksize_D == 0) {
              if (blocksize_A == 1) {
                  return("Diagonal", -1, "", -1)
              }
              else if (blocksize_A == size._1)
                  return("Filled", -1, "", -1)
              else {
                  return("Blockdiagonal", blocksize_A,"",-1)
              }
            }
            else {
              if (blocksize_A == 1) {
                return("Schur",size._1 - blocksize_D,"Diagonal",-1)
              }
              else if (blocksize_A == size._1) {
                return("Schur",size._1 - blocksize_D,"Filled",-1)
              }
              else {
                return("Schur",size._1 - blocksize_D,"Blockdiagonal",blocksize_A)
              }
            }
          case _                                                                                  => Logger.error("unexpected datatype: " + mat.datatype.resolveBaseDatatype)
        }
      case _                                                                                       => Logger.error("unexpected argument type: " + matrix + ", expected matrix variable or expression")
    }

  }

  def isOfStructureRuntime(matrix : IR_VariableAccess) : IR_Scope = {
    var func = IR_Scope(Nil)
    func
  }
}
