package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_PlainInternalFunctionReference
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.matStructInfo
import exastencils.field.ir.IR_FieldAccess

object IR_SolveLinearSystem {

  def apply(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean, msi : matStructInfo) = {
    msi.structure match {
      case "Schur" => schur(AVals, fVals, unknowns, jacobiType, relax, omitConditions, msi)
    }
  }

  def schur(AVals : ListBuffer[ListBuffer[IR_Expression]], fVals : ListBuffer[IR_Expression], unknowns : ListBuffer[IR_FieldAccess],
      jacobiType : Boolean, relax : Option[IR_Expression], omitConditions : Boolean, msi:matStructInfo) {
    def vecAcc(vec : ListBuffer[IR_VariableAccess], i0 : Int) = IR_HighDimAccess(vec(i0 / msi.blocksizeA - 1), IR_ConstIndex(i0 % msi.blocksizeA))
    def matAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i0 / msi.blocksizeA - 1), IR_ConstIndex(i0 % msi.blocksizeA, i1 % msi.blocksizeA))

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
    var D = IR_VariableAccess("local_D", IR_MatrixDatatype(IR_FloatDatatype, bsizeD, bsizeD))
    var U = ListBuffer[IR_VariableAccess]()
    var V = IR_VariableAccess("local_V", IR_MatrixDatatype(IR_FloatDatatype, bsizeD, 1))
    var F = ListBuffer[IR_VariableAccess]()
    var G = IR_VariableAccess("local_G", IR_MatrixDatatype(IR_FloatDatatype, bsizeD, 1))

    for (local <- List(D, G, V))
      stmts += IR_VariableDeclaration(local)
    for (i <- 0 until nComponents) {
      A += IR_VariableAccess(s"local_A${i}${i}", IR_MatrixDatatype(IR_FloatDatatype, bsizeA, bsizeA))
      B += IR_VariableAccess(s"local_B${ i }", IR_MatrixDatatype(IR_FloatDatatype, bsizeA, 1))
      U += IR_VariableAccess(s"local_U${ i }", IR_MatrixDatatype(IR_FloatDatatype, bsizeA, 1))
      F += IR_VariableAccess(s"local_F${ i }", IR_MatrixDatatype(IR_FloatDatatype, bsizeA, 1))
      C += IR_VariableAccess(s"local_C${ i }", IR_MatrixDatatype(IR_FloatDatatype, 1, bsizeA))
    }
    for (local <- List(A,B, U, F, C)) {
      for (component <- local) {
        stmts += IR_VariableDeclaration(component)
      }
    }

    // construct rhs and matrix
    var innerStmts = ListBuffer[IR_Statement]()

    for (i <- 0 until bsize) {
      // retrieve rhs
      innerStmts += IR_Assignment(vecAcc(F, i), fVals(i))

      // retrieve As diagonal blocks
      val baseI = i / bsizeA * bsizeA
      for (j <- baseI until baseI + bsizeA) {
        innerStmts += IR_Assignment(matAcc(A, i, j), AVals(i)(j))
      }

      // retrieve Bs
      for (j <- bsize until bsize + bsizeD) {
        innerStmts += IR_Assignment(matAcc(B, i, j), AVals(i)(j))
      }
    }

    for (i <- bsize until size) {
      // retrieve C
      for (j <- 0 until bsize) {
        innerStmts += IR_Assignment(matAcc(C, i, j), AVals(i)(j))
      }

      // retrieve D
      for (j <- bsize until size) {
        innerStmts += IR_Assignment(IR_HighDimAccess(D, IR_ConstIndex( i, j)), AVals(i)(j))
      }

      // retrieve G
      innerStmts += IR_Assignment(IR_HighDimAccess(G, IR_ConstIndex(i,0)), fVals(i))
    }

    //TODO omitConditions check
    stmts ++= innerStmts

    // invert A blocks
    var A_inv = ListBuffer[IR_VariableAccess]()
    for(i <- 0 until nComponents) {
      stmts += IR_VariableDeclaration(A_inv(i),IR_FunctionCall(IR_PlainInternalFunctionReference("inverse",IR_MatrixDatatype(IR_FloatDatatype,bsizeA,bsizeA)), A(i)))
    }

    var S = IR_VariableAccess("local_S",IR_MatrixDatatype(IR_FloatDatatype,bsizeD,bsizeD))
    stmts += IR_VariableDeclaration(S,D)
    for(i <- 0 until nComponents) {
      stmts += IR_Assignment(S,C(i)*A_inv(i)*B(i) ,"-=")
    }

    var GTilde = IR_VariableAccess("local_G~",IR_MatrixDatatype(IR_FloatDatatype,bsizeD,bsizeD))
    stmts += IR_VariableDeclaration(GTilde,G)
    for(i <- 0 until nComponents) {
      stmts += IR_Assignment(GTilde,C(i)*A_inv(i)*F(i) ,"-=")
    }

    // write to U
    stmts += IR_Assignment(V,IR_FunctionCall(IR_PlainInternalFunctionReference("inverse", S.datatype),S) * GTilde)
    for(i <- 0 until nComponents) {
      stmts += IR_Assignment(U(i), A_inv(i) * (F(i) - B(i)*V))
    }

  }
}
