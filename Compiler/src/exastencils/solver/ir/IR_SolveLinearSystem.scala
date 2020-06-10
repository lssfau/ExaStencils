package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_PlainInternalFunctionReference
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.matStructInfo
import exastencils.boundary.ir.IR_IsValidComputationPoint
import exastencils.core.Duplicate
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_SlotAccess

object IR_SolveLinearSystem {

  def apply(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean, msi : matStructInfo) = {
    msi.structure match {
      case "Schur" => schur(AVals, fVals, unknowns, jacobiType, relax, omitConditions, msi)
    }
  }

  def schur(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean, msi : matStructInfo) : ListBuffer[IR_Statement] = {
    def vecAcc(vec : ListBuffer[IR_VariableAccess], i0 : Int) = IR_HighDimAccess(vec(i0 / msi.blocksizeA), IR_ConstIndex(i0 % msi.blocksizeA))

    def matAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i0 / msi.blocksizeA), IR_ConstIndex(i0 % msi.blocksizeA, i1 % msi.blocksizeA))

    val stmts = ListBuffer[IR_Statement]()

    // blocksizes:
    /*
         bsizeA
         |  |                           _
        	A          B      U         F _  bsizeA
			        ...   ..  *  ..   =     ..
			            A  B      U         F _
          C  ...  C  D      V         G _  bsizeD
         |         |   |
            bsize  bsizeD
         |             |
                size
     */

    // declare variables for A,B,C,D,F,G
    val bsize = msi.blocksize
    val bsizeA = msi.blocksizeA
    val size = AVals.length
    val bsizeD = size - bsize

    val nComponents = bsize / bsizeA
    var A = ListBuffer[IR_VariableAccess]()
    var C = ListBuffer[IR_VariableAccess]()
    var B = ListBuffer[IR_VariableAccess]()
    var D = IR_VariableAccess("local_D", IR_MatrixDatatype(IR_RealDatatype, bsizeD, bsizeD))
    var U = ListBuffer[IR_VariableAccess]()
    var V = IR_VariableAccess("local_V", IR_MatrixDatatype(IR_RealDatatype, bsizeD, 1))
    var F = ListBuffer[IR_VariableAccess]()
    var G = IR_VariableAccess("local_G", IR_MatrixDatatype(IR_RealDatatype, bsizeD, 1))

    for (local <- List(D, G, V))
      stmts += IR_VariableDeclaration(local)
    for (i <- 0 until nComponents) {
      A += IR_VariableAccess(s"local_A${ i + 1 }${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA))
      B += IR_VariableAccess(s"local_B${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeD))
      U += IR_VariableAccess(s"local_U${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeD))
      F += IR_VariableAccess(s"local_F${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeD))
      C += IR_VariableAccess(s"local_C${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeD, bsizeA))
    }
    for (local <- List(A, B, U, F, C)) {
      for (component <- local) {
        stmts += IR_VariableDeclaration(component)
      }
    }
    U += V

    // construct rhs and matrix

    for (i <- 0 until size) {
      var innerStmts = ListBuffer[IR_Statement]()
      var boundaryStmts = ListBuffer[IR_Statement]()

      if (i < bsize) {
        // retrieve rhs
        innerStmts += IR_Assignment(vecAcc(F, i), fVals(i))
        // rhs for boundary
        boundaryStmts += IR_Assignment(vecAcc(F, i), Duplicate(unknowns(i)))

        // retrieve As diagonal blocks
        val baseI = i / bsizeA * bsizeA
        for (j <- baseI until baseI + bsizeA) {
          innerStmts += IR_Assignment(matAcc(A, i, j), AVals(i)(j))
          boundaryStmts += IR_Assignment(matAcc(A, i, j), IR_IntegerConstant(if(i == j) 1 else 0))
        }


        // retrieve Bs
        for (j <- bsize until bsize + bsizeD) {
          innerStmts += IR_Assignment(matAcc(B, i, j), AVals(i)(j))
          boundaryStmts += IR_Assignment(matAcc(B, i, j), IR_IntegerConstant(0))
        }
      }
      else {
        // retrieve C
        for (j <- 0 until bsize) {
          innerStmts += IR_Assignment(matAcc(C, i / bsizeA, j), AVals(i)(j))
          boundaryStmts += IR_Assignment(matAcc(C, i/bsizeA, j), IR_IntegerConstant(0))
        }

        // retrieve D
        for (j <- bsize until size) {
          innerStmts += IR_Assignment(IR_HighDimAccess(D, IR_ConstIndex(i, j)), AVals(i)(j))
          boundaryStmts += IR_Assignment(IR_HighDimAccess(D, IR_ConstIndex(i,j)), IR_IntegerConstant(0))
        }

        // retrieve G
        innerStmts += IR_Assignment(IR_HighDimAccess(G, IR_ConstIndex(i, 0)), fVals(i))
        boundaryStmts += IR_Assignment(IR_HighDimAccess(G, IR_ConstIndex(i)), Duplicate(unknowns(i)))
      }

      // implement check if current unknown is on/ beyond boundary - if required
      if (omitConditions) {
        stmts ++= innerStmts
      } else {
        stmts += IR_IfCondition(
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          innerStmts,
          boundaryStmts)
      }
    }

    // invert A blocks
    var A_inv = ListBuffer[IR_VariableAccess]()
    for (i <- 0 until nComponents) {
      A_inv += IR_VariableAccess(s"local_A_inv_${ i }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA))
    }
    for (i <- 0 until nComponents) {
      stmts += IR_VariableDeclaration(A_inv(i), IR_FunctionCall(IR_PlainInternalFunctionReference("inverse", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA)), A(i)))
    }

    var S = IR_VariableAccess("local_S", IR_MatrixDatatype(IR_RealDatatype, bsizeD, bsizeD))
    stmts += IR_VariableDeclaration(S, D)
    for (i <- 0 until nComponents) {
      stmts += IR_Assignment(S, C(i) * A_inv(i) * B(i), "-=")
    }

    var GTilde = IR_VariableAccess("local_GTilde", IR_MatrixDatatype(IR_RealDatatype, bsizeD, bsizeD))
    stmts += IR_VariableDeclaration(GTilde, G)
    for (i <- 0 until nComponents) {
      stmts += IR_Assignment(GTilde, C(i) * A_inv(i) * F(i), "-=")
    }

    // write to U
    stmts += IR_Assignment(V, IR_FunctionCall(IR_PlainInternalFunctionReference("inverse", S.datatype), S) * GTilde)
    for (i <- 0 until nComponents) {
      stmts += IR_Assignment(U(i), A_inv(i) * (F(i) - B(i) * V))
    }

    // write back results
    for (i <- 0 until size) {
      val dest = Duplicate(unknowns(i))
      if (jacobiType) dest.slot.asInstanceOf[IR_SlotAccess].offset += 1

      if (omitConditions) {
        if (relax.isEmpty)
          stmts += IR_Assignment(dest, vecAcc(U, i))
        else
          stmts += IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecAcc(U, i))
      } else {
        stmts += IR_IfCondition( // don't write back result on boundaries
          IR_IsValidComputationPoint(unknowns(i).field, Duplicate(unknowns(i).index)),
          if (relax.isEmpty)
            IR_Assignment(dest, vecAcc(U, i))
          else
            IR_Assignment(dest, Duplicate(unknowns(i)) * (1.0 - relax.get) + relax.get * vecAcc(U, i))
        )
      }
    }

    stmts
  }
}
