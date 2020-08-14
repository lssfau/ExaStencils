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

import exastencils.base.ir
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir._
import exastencils.baseExt
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression

// methods to determine whether a matrix is a diagonal-, blockdiagonal-, schurmatrix at compiletime
//TODO matrices have to be filled with constants
object IR_ClassifyMatShape {
  def apply(mat : IR_Expression) = {
    isOfShape(mat)
  }
  def apply(mat : ListBuffer[ListBuffer[IR_Addition]]) = {
    isOfShape(mat)
  }

  def evaluateEntry(mat : IR_Expression, i : Int, j : Int) : Double = {
    mat match {
      case x @ (IR_MatrixExpression(_, _, _,_) | IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))) =>
        val entry = IR_CompiletimeMatOps.getElem(x, i, j)
        entry match {
          case va : IR_VariableAccess                                                                                    => 1
          case hda : IR_HighDimAccess                                                                                    => 1
          case n if (n.datatype == IR_IntegerDatatype)                                                                   => IR_SimplifyExpression.evalIntegral(n)
          case n if (n.datatype == IR_RealDatatype || n.datatype == IR_FloatDatatype || n.datatype == IR_DoubleDatatype) => IR_SimplifyExpression.evalFloating(n)
          case _                                                                                                         => Logger.error(s"unexpected entry ${ entry }")
        }
      case _                                                                                     => Logger.error("unexpected argument type: " + mat + ", expected matrix expression or variable")
    }
  }

  // determine structure of 'matrix' (which must have compiletime evaluatable entries) and return it as a String + more specific structure information like blocksizes in case of Schur or Blockdiagonal matrices
  def isOfShape(matrix : IR_Expression) : IR_MatShape = {
    var blocksize_A = 0
    var blocksize_D = 0
    matrix match {
      case mat @ IR_MatrixExpression(_, _, _,_) =>
        var size = IR_CompiletimeMatOps.getSize(mat)
        if (size._1 == 1) return IR_MatShape("filled")

        mat.datatype.resolveBaseDatatype match {
          case _ @ (IR_IntegerDatatype | IR_RealDatatype | IR_FloatDatatype | IR_DoubleDatatype) =>

            // count blocksize of schur block
            var cont = true
            while ((blocksize_D < size._1) && cont == true) {
              // count on multiple lines to avoid mistakes due to random zeros
              val en0 : Double = evaluateEntry(mat, 0, size._1 - blocksize_D - 1)
              val en1 : Double = evaluateEntry(mat, size._1 - blocksize_D - 1, 0)
              val en2 : Double = evaluateEntry(mat, 1, size._1 - blocksize_D - 1)
              val en3 : Double = evaluateEntry(mat, size._1 - blocksize_D - 1, 1)

              // only if all lanes are zero break
              if (en0 == 0.0 && en1 == 0 && en2 == 0 && en3 == 0) {
                cont = false
              }
              else
                blocksize_D += 1
            }
            // if we reached other end of matrix: its a filled matrix
            if (blocksize_D == size._1) return IR_MatShape("filled")

            // count blocksize of blockmatrix A if present (Schur form with size(mat) - blocksize_D as size of A block (upper left block in schur form)
            cont = true
            while (blocksize_A <= (size._1 - blocksize_D) / 2 + 1 && cont == true) {
              val en0 : Double = evaluateEntry(mat, 0, blocksize_A)
              val en1 : Double = evaluateEntry(mat, blocksize_A, 0)

              if (en0 == 0.0 && en1 == 0) {
                cont = false
              } else {
                blocksize_A += 1
              }
            }

            // if more than half of A block is reached, blockdiagonal is not possible anymore -> Schur form with filled A block is filled matrix
            if (blocksize_A == (size._1 - blocksize_D) / 2 + 1) {
              return IR_MatShape("filled")
            }

            // average blocksize over block
            //blocksize_A = aestimateBlocksize(mat)

            var border = size._1 - blocksize_D
            for (i <- 0 until border) {
              var start = (i / blocksize_A) * blocksize_A + blocksize_A
              for (j <- start until border) {
                val en0 : Double = evaluateEntry(mat, i, j)
                val en1 : Double = evaluateEntry(mat, j, i)
                if (en0 != 0 || en1 != 0)
                  return IR_MatShape("filled")
              }
            }

            // no schur form
            if (blocksize_D == 0) {
              // size of A block: A block is the whole matrix with blocksize_D == 0 (no schur form)
              if (blocksize_A == 1) {
                IR_MatShape("diagonal")
              }
              else if (blocksize_A == size._1)
                IR_MatShape("filled")
              else {
                IR_MatShape("blockdiagonal").addInfo("block", blocksize_A)
              }
            }
            // schur form
            else {
              // form of A block in schur form
              if (blocksize_A == 1) {
                IR_MatShape("schur").addInfo("block",size._1 - blocksize_D).addInfo("A","diagonal")
              }
              else if (blocksize_A == size._1) {
                IR_MatShape("filled")
              }
              else {
                IR_MatShape("schur").addInfo("block",size._1 - blocksize_D).addInfo("A","blockdiagonal").addInfo("Ablock",blocksize_A)
              }
            }
          case _                                                                                  => Logger.error("unexpected datatype: " + mat.datatype.resolveBaseDatatype)
        }
      case _                                  => Logger.error("unexpected argument type: " + matrix + ", expected matrix variable or expression")
    }

  }

  // determine structure of 'matrix' (which must have compiletime evaluatable entries) and return it as a String + more specific structure information like blocksizes in case of Schur or Blockdiagonal matrices
  def isOfShape(mat : ListBuffer[ListBuffer[IR_Addition]]) : IR_MatShape = {
    if (mat.length == 1 || mat(0).length == 1) {
      return baseExt.ir.IR_MatShape("filled")
    }
    var blocksize_A = 0
    var blocksize_D = 0
    var size = mat.length

    // count blocksize of schur block
    var cont = true
    while ((blocksize_D < size) && cont == true) {
      // count on multiple lines to avoid mistakes due to random zeros
      //TODO right ordering of rows/cols?
      val add0 = mat(0)(size - blocksize_D - 1)
      val add1 = mat(size - blocksize_D - 1)(0)
      val add2 = mat(1)(size - blocksize_D - 1)
      val add3 = mat(size - blocksize_D - 1)(1)
      cont = false
      List[IR_Addition](add0, add1, add2, add3).foreach(add =>
        add.summands.foreach {
          case IR_RealConstant(0.0)   =>
          case IR_DoubleConstant(0.0) =>
          case IR_FloatConstant(0.0)  =>
          case IR_IntegerConstant(0)  =>
          case other                  =>
            cont = true
        })
      if (cont == true)
        blocksize_D += 1
    }
    // if we reached other end of matrix: its a filled matrix
    if (blocksize_D == size) return IR_MatShape("filled")

    // count blocksize of blockmatrix A if present (Schur form with size(mat) - blocksize_D as size of A block (upper left block in schur form))
    cont = true
    while (blocksize_A <= (size - blocksize_D) / 2 + 1 && cont == true) {
      val add0 = mat(0)(blocksize_A)
      val add1 = mat(blocksize_A)(0)
      cont = false
      List[IR_Addition](add0, add1).foreach(add =>
        add.summands.foreach {
          case IR_RealConstant(0.0)   =>
          case IR_DoubleConstant(0.0) =>
          case IR_FloatConstant(0.0)  =>
          case IR_IntegerConstant(0)  =>
          case other                  =>
            cont = true
        })
      if (cont != false)
        blocksize_A += 1
    }

    // if more than half of A block is reached, blockdiagonal is not possible anymore -> Schur form with filled A block is filled matrix
    // if blocksize
    if (blocksize_A >= (size - blocksize_D) / 2 + 1) {
      return IR_MatShape("filled")
    }

    // average blocksize over block
    //blocksize_A = aestimateBlocksize(mat)

    var border = size - blocksize_D
    for (i <- 0 until border) {
      var start = (i / blocksize_A) * blocksize_A + blocksize_A
      for (j <- start until border) {
        val add0 = mat(i)(j)
        val add1 = mat(j)(i)
        var nonzeroEntry = false
        List[IR_Addition](add0, add1).foreach(add =>
          add.summands.foreach {
            case IR_RealConstant(0.0)   =>
            case IR_DoubleConstant(0.0) =>
            case IR_FloatConstant(0.0)  =>
            case IR_IntegerConstant(0)  =>
            case other                  =>
              Logger.warn("Schur complement not possible")
              nonzeroEntry = true
          })
        if (nonzeroEntry)
          return IR_MatShape("filled")
      }
    }

    // no schur form
    if (blocksize_D == 0) {
      // size of A block: A block is the whole matrix with blocksize_D == 0 (no schur form)
      if (blocksize_A == 1) {
        IR_MatShape("diagonal")
      }
      else if (blocksize_A == size)
        IR_MatShape("filled")
      else {
        IR_MatShape("blockdiagonal").addInfo("block",blocksize_A)
      }
    }
    // schur form
    else {
      // form of A block in schur form
      if (blocksize_A == 1) {
        IR_MatShape("schur").addInfo("block",size - blocksize_D).addInfo("A","diagonal")
      }
      else if (blocksize_A == size) {
        IR_MatShape("filled")
      }
      else {
        IR_MatShape("schur").addInfo("block",size - blocksize_D).addInfo("A","blockdiagonal").addInfo("Ablock",blocksize_A)
      }
    }
  }

  // do the same at runtime, entries do not have to be compiletime evaluatable
  def isOfShapeRuntime() : IR_PlainFunction = {
    var debug = true
    var stmts = ListBuffer[IR_Statement]()
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var cont = IR_VariableAccess("cont", IR_BooleanDatatype)
    var blocksize_A_local = IR_VariableAccess("blocksize_A_local", IR_IntegerDatatype)
    var blocksize_D = IR_VariableAccess("blocksize_D_local", IR_IntegerDatatype)
    var en0 = IR_VariableAccess("en_0", IR_DoubleDatatype)
    var en1 = IR_VariableAccess("en_1", IR_DoubleDatatype)
    var en2 = IR_VariableAccess("en_2", IR_DoubleDatatype)
    var en3 = IR_VariableAccess("en_3", IR_DoubleDatatype)
    var earlyOut = IR_VariableAccess("earlyOut", IR_BooleanDatatype)
    var matrix = IR_VariableAccess("matrix", IR_PointerDatatype(IR_DoubleDatatype))
    var insize = IR_VariableAccess("insize", IR_IntegerDatatype)
    var structure = IR_VariableAccess("structure", IR_StringDatatype)
    var structure_A = IR_VariableAccess("structure_A", IR_StringDatatype)
    var blocksize = IR_VariableAccess("blocksize", IR_IntegerDatatype)
    var blocksize_A = IR_VariableAccess("blocksize_A", IR_IntegerDatatype)

    stmts += IR_VariableDeclaration(cont, IR_BooleanConstant(true))
    stmts += IR_VariableDeclaration(earlyOut, IR_BooleanConstant(false))
    stmts += IR_VariableDeclaration(blocksize_A_local, IR_IntegerConstant(0))
    stmts += IR_VariableDeclaration(blocksize_D, IR_IntegerConstant(0))
    stmts += IR_WhileLoop(IR_AndAnd(IR_Lower(blocksize_D, insize), IR_EqEq(cont, IR_BooleanConstant(true))), ListBuffer[IR_Statement](
      IR_VariableDeclaration(en0, IR_ArrayAccess(matrix, IR_IntegerConstant(0) + insize - blocksize_D - IR_IntegerConstant(1))),
      IR_VariableDeclaration(en1, IR_ArrayAccess(matrix, (insize - blocksize_D - IR_IntegerConstant(1)) * insize + IR_IntegerConstant(0))),
      IR_VariableDeclaration(en2, IR_ArrayAccess(matrix, insize + insize - blocksize_D - IR_IntegerConstant(1))),
      IR_VariableDeclaration(en3, IR_ArrayAccess(matrix, (insize - blocksize_D - IR_IntegerConstant(1)) * insize + IR_IntegerConstant(1))),
      IR_IfCondition(IR_AndAnd(IR_AndAnd(IR_EqEq(en0, IR_IntegerConstant(0)), IR_EqEq(en1, IR_IntegerConstant(0))), IR_AndAnd(IR_EqEq(en2, IR_IntegerConstant(0)), IR_EqEq(en3, IR_IntegerConstant(0)))),
        ListBuffer[IR_Statement](
          IR_Assignment(cont, IR_BooleanConstant(false)
          ))
        ,
        ListBuffer[IR_Statement](
          IR_Assignment(blocksize_D, IR_Addition(blocksize_D, IR_IntegerConstant(1)))
        ))
    ))
    stmts += IR_IfCondition(IR_EqEq(blocksize_D, insize), ListBuffer[IR_Statement](
      IR_Assignment(earlyOut, IR_BooleanConstant(true))
    ))
    stmts += IR_Assignment(cont, IR_BooleanConstant(true))
    stmts += IR_WhileLoop(IR_AndAnd(IR_LowerEqual(blocksize_A_local, IR_Addition(IR_Division(IR_Subtraction(insize, blocksize_D), IR_IntegerConstant(2)), IR_IntegerConstant(1))), IR_EqEq(cont, IR_BooleanConstant(true))), ListBuffer[IR_Statement](
      IR_VariableDeclaration(en0, IR_ArrayAccess(matrix, (IR_IntegerConstant(0) * insize + blocksize_A_local))),
      IR_VariableDeclaration(en1, IR_ArrayAccess(matrix, blocksize_A_local * insize + IR_IntegerConstant(0))),
      IR_IfCondition(IR_AndAnd(IR_EqEq(en0, IR_DoubleConstant(0.0)), IR_EqEq(en1, IR_DoubleConstant(0.0))), ListBuffer[IR_Statement](
        IR_Assignment(cont, IR_BooleanConstant(false))
      ),
        ListBuffer[IR_Statement](
          IR_Assignment(blocksize_A_local, IR_Addition(blocksize_A_local, IR_IntegerConstant(1)))
        ))
    ))

    var border = IR_VariableAccess("border", IR_IntegerDatatype)
    var start = IR_VariableAccess("start", IR_IntegerDatatype)
    var integerDivision = IR_VariableAccess("integerDivision", IR_IntegerDatatype)
    stmts += IR_VariableDeclaration(border, insize - blocksize_D)
    stmts += IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, border), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_VariableDeclaration(integerDivision, i / blocksize_A_local),
      IR_VariableDeclaration(start, (integerDivision * blocksize_A_local + blocksize_A_local)),
      IR_ForLoop(IR_VariableDeclaration(j, start), IR_Lower(j, border), IR_PreIncrement(j), ListBuffer[IR_Statement](
        IR_IfCondition(IR_OrOr(IR_Neq(IR_ArrayAccess(matrix, i * insize + j), IR_DoubleConstant(0.0)), IR_Neq(IR_ArrayAccess(matrix, j * insize + i), IR_DoubleConstant(0.0))), ListBuffer[IR_Statement](
          IR_Assignment(earlyOut, true),
          ir.IR_Break()
        ))
      ))
    ))

    stmts += IR_IfCondition(IR_EqEq(earlyOut, true), ListBuffer[IR_Statement](
      IR_Assignment(structure, IR_StringConstant("Filled"))
    ), ListBuffer[IR_Statement](
      IR_IfCondition(IR_EqEq(blocksize_D, IR_IntegerConstant(0)), ListBuffer[IR_Statement](
        IR_IfCondition(IR_EqEq(blocksize_A_local, IR_IntegerConstant(1)), ListBuffer[IR_Statement](
          IR_Assignment(structure, IR_StringConstant("Diagonal"))
        )),
        IR_IfCondition(IR_EqEq(blocksize_A_local, insize), ListBuffer[IR_Statement](
          IR_Assignment(structure, IR_StringConstant("Filled"))
        )),
        IR_IfCondition(IR_Neq(IR_OrOr(IR_EqEq(blocksize_A_local, IR_IntegerConstant(1)), IR_EqEq(blocksize_A_local, insize)), IR_BooleanConstant(true)), ListBuffer[IR_Statement](
          IR_Assignment(structure, IR_StringConstant("Blockdiagonal")),
          IR_Assignment(blocksize, blocksize_A_local)
        )),
      ), ListBuffer[IR_Statement](
        IR_Assignment(structure, IR_StringConstant("Schur")),
        IR_Assignment(blocksize, IR_Subtraction(insize, blocksize_D)),
        IR_IfCondition(IR_EqEq(blocksize_A_local, IR_IntegerConstant(1)), ListBuffer[IR_Statement](
          IR_Assignment(structure_A, IR_StringConstant("Diagonal"))
        )),
        IR_IfCondition(IR_EqEq(blocksize_A_local, insize), ListBuffer[IR_Statement](
          IR_Assignment(structure_A, IR_StringConstant("Filled"))
        )),
        IR_IfCondition(IR_Neq(IR_OrOr(IR_EqEq(blocksize_A_local, insize), IR_EqEq(blocksize_A_local, IR_IntegerConstant(1))), IR_BooleanConstant(true)), ListBuffer[IR_Statement](
          IR_Assignment(structure_A, IR_StringConstant("Blockdiagonal")),
          IR_Assignment(blocksize_A, blocksize_A_local)
        ))
      ))
    ))
    IR_PlainFunction("isOfStructure", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("matrix", IR_PointerDatatype(IR_DoubleDatatype)),
      IR_FunctionArgument("insize", IR_IntegerDatatype),
      IR_FunctionArgument("structure", IR_ReferenceDatatype(IR_StringDatatype)),
      IR_FunctionArgument("blocksize", IR_ReferenceDatatype(IR_IntegerDatatype)),
      IR_FunctionArgument("structure_A", IR_ReferenceDatatype(IR_StringDatatype)),
      IR_FunctionArgument("blocksize_A", IR_ReferenceDatatype(IR_IntegerDatatype))
    ),
      stmts)
  }

}
