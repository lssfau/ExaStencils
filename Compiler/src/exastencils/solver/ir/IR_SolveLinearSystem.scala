
package exastencils.solver.ir


import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_PlainInternalFunctionReference
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_DetermineMatrixStructure
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.core
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object IR_SolveLinearSystem {
  def apply(A : IR_VariableAccess , u : IR_VariableAccess, f : IR_VariableAccess) : IR_SolveLinearSystem = {
    new IR_SolveLinearSystem(A, f, u)
  }
}

case class IR_SolveLinearSystem(A : IR_VariableAccess,  u : IR_VariableAccess, f : IR_VariableAccess ) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "solveLinearSystem(" + A.prettyprint(out) + ", " + f.prettyprint(out) + ")"
  def expand() : Transformation.OutputType = {
    val msi = IR_DetermineMatrixStructure.isOfStructure(A)
    val (m, n) = A.datatype match {
      case mat : IR_MatrixDatatype => (mat.sizeM, mat.sizeN)
      case _ => Logger.error(s"unexpected datatype of A: ${A.datatype}")
    }
    val k = f.datatype match {
      case mat : IR_MatrixDatatype => mat.sizeM
      case _ => Logger.error(s"unexpected datatype of f: ${A.datatype}")
    }
    u.datatype match {
      case mat : IR_MatrixDatatype =>
        if(mat.sizeN != k) Logger.error("u and f do not match in size")
        if(mat.sizeM != m) Logger.error("u and A do not match in size")
    }
    msi.structure match {
      case "Diagonal" =>
        var stmts = ListBuffer[IR_Statement]()
        for (i <- 0 until m) {
          stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i, 0)), IR_Division(IR_IntegerConstant(1), IR_HighDimAccess(A, IR_ConstIndex(i,i))))
        }
        stmts
      //TODO in case of schur: solve with A-Decomposition to helper matrices
      // or with schur inversion of sysstem matrix A
      case "Schur" =>
        var stmts = ListBuffer[IR_Statement]()

        def vecAcc(vec : ListBuffer[IR_VariableAccess], i0 : Int) = IR_HighDimAccess(vec(i0 / msi.blocksizeA), IR_ConstIndex(i0 % msi.blocksizeA))

        def rowBlockMatAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i0 / msi.blocksizeA), IR_ConstIndex(i0 % msi.blocksizeA, i1 % msi.blocksizeA))

        def colBlockMatAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i1 / msi.blocksizeA), IR_ConstIndex(i0 % msi.blocksizeA, i1 % msi.blocksizeA))

        // declare variables for A,B,C,D,F,G
        val bsize = msi.blocksize
        val bsizeA = msi.blocksizeA
        val size = A.rows
        val bsizeD = size - bsize

        val nComponents = bsize / bsizeA
        var A_blocks = ListBuffer[IR_VariableAccess]()
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
          A_blocks += IR_VariableAccess(s"local_A${ i + 1 }${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA))
          B += IR_VariableAccess(s"local_B${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeD))
          U += IR_VariableAccess(s"local_U${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeD))
          F += IR_VariableAccess(s"local_F${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeD))
          C += IR_VariableAccess(s"local_C${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeD, bsizeA))
        }
        for (local <- List(A_blocks, B, U, F, C)) {
          for (component <- local) {
            stmts += IR_VariableDeclaration(component)
          }
        }
        U += V

        // construct rhs and matrix

        for (i <- 0 until size) {
          var innerStmts = ListBuffer[IR_Statement]()

          if (i < bsize) {
            // retrieve rhs
            innerStmts += IR_Assignment(vecAcc(F, i), f.get(i, 0))

            // retrieve As diagonal blocks
            val baseI = i / bsizeA * bsizeA
            for (j <- baseI until baseI + bsizeA) {
              innerStmts += IR_Assignment(rowBlockMatAcc(A_blocks, i, j), A.get(i, j))
            }

            // retrieve Bs
            for (j <- bsize until bsize + bsizeD) {
              innerStmts += IR_Assignment(rowBlockMatAcc(B, i, j), A.get(i, j))
            }
          }
          else {
            // retrieve C
            for (j <- 0 until bsize) {
              innerStmts += IR_Assignment(colBlockMatAcc(C, i, j), A.get(i, j))
            }

            // retrieve D
            for (j <- bsize until size) {
              innerStmts += IR_Assignment(IR_HighDimAccess(D, IR_ConstIndex(i, j)), A.get(i, j))
            }

            // retrieve G
            innerStmts += IR_Assignment(IR_HighDimAccess(G, IR_ConstIndex(i, 0)), f.get(i, 0))
          }
          stmts ++= innerStmts
        }

        // invert A blocks
        var A_inv = ListBuffer[IR_VariableAccess]()
        for (i <- 0 until nComponents) {
          A_inv += IR_VariableAccess(s"local_A_inv_${ i }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA))
        }
        for (i <- 0 until nComponents) {
          stmts += IR_VariableDeclaration(A_inv(i), IR_FunctionCall(IR_PlainInternalFunctionReference("inverse", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA)), A_blocks(i)))
        }

        // calculate schur matrix
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
        stmts += IR_Assignment(V, IR_FunctionCall(IR_PlainInternalFunctionReference("inverse", S.datatype), S) * GTilde)

        // write to Ui
        for (i <- 0 until nComponents) {
          stmts += IR_Assignment(U(i), A_inv(i) * (F(i) - B(i) * V))
        }
        // compile u
        for (i <- 0 until N) {
          stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i)), U(i))
        }
        stmts
      // Fallback: solve by inverting A
      //TODO solve with special invert or full invert
      case _ => IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), mutable.ListBuffer[IR_Expression](A, IR_StringConstant(msi.structure), IR_IntegerConstant(msi.blocksize))), f))
    }

  }
}