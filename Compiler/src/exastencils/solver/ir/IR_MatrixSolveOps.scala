package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir
import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Addition
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_GreaterEqual
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_LowerEqual
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_PlainInternalFunctionReference
import exastencils.base.ir.IR_Power
import exastencils.base.ir.IR_PreDecrement
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_RealConstant
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_ReferenceDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_CompiletimeMatOps.copySubMatrix
import exastencils.baseExt.ir.IR_CompiletimeMatOps.transpose
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion.localLUDecomp
import exastencils.baseExt.ir.IR_MatShape
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.NodeCounter
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify

object IR_MatrixSolveOps {
  var systemCounter : Int = 0

  def forwardBackwardSub(LU : IR_MatrixExpression, b : IR_MatrixExpression, P : Array[Int]) : IR_MatrixExpression = {
    // solve
    val N = LU.columns
    if (N != LU.rows) Logger.error("can only decompose quadratic matrices with LU")
    var u_loc = IR_MatrixExpression(LU.innerDatatype, LU.columns, 1)
    for (i <- 0 until N) {
      u_loc.set(i, 0, Duplicate(b.get(P(i), 0)))
      for (k <- 0 until i) {
        u_loc.set(i, 0, Duplicate(u_loc.get(i, 0)) - Duplicate(LU.get(i, k)) * Duplicate(u_loc.get(k, 0)))
      }
    }
    for (i <- N - 1 to 0 by -1) {
      for (k <- i + 1 until N) {
        u_loc.set(i, 0, Duplicate(u_loc.get(i, 0)) - Duplicate(LU.get(i, k)) * Duplicate(u_loc.get(k, 0)))
      }
      u_loc.set(i, 0, Duplicate(u_loc.get(i, 0)) / Duplicate(LU.get(i, i)))
    }
    if (Knowledge.experimental_checkCTPivots) {
      u_loc.annotate("checkCTInversionPivots", LU.popAnnotationAs[IR_MatrixExpression]("checkCTInversionPivots"))
    }
    u_loc
  }

  def schurDomainDecomp(A : IR_MatrixExpression, u : IR_VariableAccess, f : IR_VariableAccess, msi : IR_MatShape) : Transformation.OutputType = {
    var stmts = ListBuffer[IR_Statement]()
    // blocksizes
    val bsize = msi.size("block")
    val bsizeA = msi.shape("A") match {
      case "blockdiagonal" => msi.size("Ablock")
      case "diagonal"      => 1
      case _               => Logger.error(s"unexpected shape of Ablock: ${ msi.shape("A") }")
    }
    val size = A.columns
    val bsizeD = size - bsize
    val nComponents = bsize / bsizeA

    // accesses for lists of variable accesses representing system matrix, rhs and solution
    def vecAcc(vec : ListBuffer[IR_VariableAccess], i0 : Int) = IR_HighDimAccess(vec(i0 / bsizeA), IR_ConstIndex(i0 % bsizeA))

    def rowBlockMatAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i0 / bsizeA), IR_ConstIndex(i0 % bsizeA, i1 % bsizeA))

    def colBlockMatAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i1 / bsizeA), IR_ConstIndex(i0 % bsizeA, i1 % bsizeA))

    def genericAcc(mat : IR_Expression, i0 : Int, i1 : Int) = IR_CompiletimeMatOps.getElem(mat, i0, i1)

    // declare variables for A,B,C,D,F,G
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
        innerStmts += IR_Assignment(vecAcc(F, i), genericAcc(f, i, 0))

        // retrieve As diagonal blocks
        val baseI = i / bsizeA * bsizeA
        for (j <- baseI until baseI + bsizeA) {
          innerStmts += IR_Assignment(rowBlockMatAcc(A_blocks, i, j), genericAcc(A, i, j))
        }

        // retrieve Bs
        for (j <- bsize until bsize + bsizeD) {
          innerStmts += IR_Assignment(rowBlockMatAcc(B, i, j), genericAcc(A, i, j))
        }
      }
      else {
        // retrieve C
        for (j <- 0 until bsize) {
          innerStmts += IR_Assignment(colBlockMatAcc(C, i, j), genericAcc(A, i, j))
        }

        // retrieve D
        for (j <- bsize until size) {
          innerStmts += IR_Assignment(IR_HighDimAccess(D, IR_ConstIndex(i, j)), genericAcc(A, i, j))
        }

        // retrieve G
        innerStmts += IR_Assignment(IR_HighDimAccess(G, IR_ConstIndex(i, 0)), genericAcc(f, i, 0))
      }
      stmts ++= innerStmts
    }

    // invert A blocks
    var A_inv = ListBuffer[IR_VariableAccess]()
    for (i <- 0 until nComponents) {
      A_inv += IR_VariableAccess(s"local_A_inv_${ i + 1 }${ i + 1 }", IR_MatrixDatatype(IR_RealDatatype, bsizeA, bsizeA))
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

    var GTilde = IR_VariableAccess("local_GTilde", IR_MatrixDatatype(IR_RealDatatype, bsizeD, 1))
    stmts += IR_VariableDeclaration(GTilde, G)
    for (i <- 0 until nComponents) {
      stmts += IR_Assignment(GTilde, C(i) * A_inv(i) * F(i), "-=")
    }
    stmts += IR_Assignment(V, IR_FunctionCall(IR_PlainInternalFunctionReference("inverse", S.datatype), S) * GTilde)

    // write to Ui blockwise
    for (i <- 0 until nComponents) {
      stmts += IR_Assignment(U(i), A_inv(i) * (F(i) - (B(i) * V)))
    }
    // compile u
    for (i <- 0 until size) {
      stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i, 0)), rowBlockMatAcc(U, i, 0))
    }
    stmts
  }

  def genForwardBackwardSub(A : IR_Access, P : IR_VariableAccess, f : IR_VariableAccess, u : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()
    val N = u.datatype.asInstanceOf[IR_MatrixDatatype].sizeM
    var i = IR_VariableAccess("i", IR_IntegerDatatype)
    var j = IR_VariableAccess("j", IR_IntegerDatatype)
    var k = IR_VariableAccess("k", IR_IntegerDatatype)
    var tmp_idx = IR_VariableAccess("tmp_idx", IR_IntegerDatatype)
    stmts += IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_VariableDeclaration(tmp_idx, IR_ArrayAccess(P, i)),
      IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_HighDimAccess(f, IR_ExpressionIndex(tmp_idx))),
      IR_ForLoop(IR_VariableDeclaration(k, 0), IR_Lower(k, i), IR_PreIncrement(k), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_Multiplication(IR_HighDimAccess(A, IR_ExpressionIndex(i, k)), IR_HighDimAccess(u, IR_ExpressionIndex(k))), "-=")
      ))
    ))
    stmts += IR_ForLoop(IR_VariableDeclaration(i, N - 1), IR_GreaterEqual(i, 0), IR_PreDecrement(i),
      IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, N), IR_PreIncrement(k),
        IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_Multiplication(IR_HighDimAccess(A, IR_ExpressionIndex(i, k)), IR_HighDimAccess(u, IR_ExpressionIndex(k))), "-=")
      ),
      IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_HighDimAccess(A, IR_ExpressionIndex(i, i)), "/=")
    )
    stmts
  }

  def genLUSolveInlined(A : IR_Access, m : Int, f : IR_VariableAccess, u : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    var stmts = ListBuffer[IR_Statement]()

    // pivoting array
    var P = IR_VariableAccess("P", IR_ArrayDatatype(IR_IntegerDatatype, m + 1))
    stmts += IR_VariableDeclaration(P)

    // lu
    stmts ++= localLUDecomp(A, P, m, IR_IntegerConstant(0), IR_IntegerConstant(0))

    // forward backward sub
    stmts ++= genForwardBackwardSub(A, P, f, u)
    stmts
  }

  def genLUSolveAsFunction(m : Int) : IR_PlainFunction = {
    val A = IR_VariableAccess("A", IR_MatrixDatatype(IR_DoubleDatatype, m, m))
    val f = IR_VariableAccess("f", IR_MatrixDatatype(IR_DoubleDatatype, m, 1))
    val u = IR_VariableAccess("u", IR_MatrixDatatype(IR_DoubleDatatype, m, 1))
    var pfunc = IR_PlainFunction(s"LUSolve_${ m }x${ m }", IR_UnitDatatype, ListBuffer[IR_FunctionArgument](
      IR_FunctionArgument("A", IR_ReferenceDatatype(IR_MatrixDatatype(IR_DoubleDatatype, m, m))),
      IR_FunctionArgument("f", IR_ReferenceDatatype(IR_MatrixDatatype(IR_DoubleDatatype, m, 1))),
      IR_FunctionArgument("u", IR_ReferenceDatatype(IR_MatrixDatatype(IR_DoubleDatatype, m, 1)))
    ),
      genLUSolveInlined(A, m, f, u)
    )
    pfunc.allowInlining = false
    pfunc
  }

  def genBlockdiagonal(AasExpr : IR_MatrixExpression,u : IR_VariableAccess, f : IR_VariableAccess, msi : IR_MatShape, m : Int, bsize : Int) : ListBuffer[IR_Statement] = {
    import exastencils.baseExt.ir.IR_CompiletimeMatOps.copySubMatrix
    val stmts = ListBuffer[IR_Statement]()
    val Ablocks = ListBuffer[IR_VariableAccess]()
    val ublocks = ListBuffer[IR_VariableAccess]()
    val fblocks = ListBuffer[IR_VariableAccess]()

    // produce A blocks
    for (i <- 0 until m / bsize) {
      val Aii = IR_VariableAccess(s"MatSys_${ systemCounter }_A${ i }${ i }", IR_MatrixDatatype(IR_DoubleDatatype, bsize, bsize))
      val fi = IR_VariableAccess(s"MatSys_${ systemCounter }_f${ i }", IR_MatrixDatatype(IR_DoubleDatatype, bsize, 1))
      val ui = IR_VariableAccess(s"MatSys_${ systemCounter }_u${ i }", IR_MatrixDatatype(IR_DoubleDatatype, bsize, 1))
      fi.annotate("noMatrixExpressionSetup")
      ui.annotate("noMatrixExpressionSetup")
      Aii.annotate("noMatrixExpressionSetup")
      Ablocks += Aii
      fblocks += fi
      ublocks += ui
      stmts += IR_VariableDeclaration(Aii, copySubMatrix(AasExpr, i * bsize, i * bsize, bsize, bsize))
      stmts += IR_VariableDeclaration(fi)
      stmts += IR_VariableDeclaration(ui)
      val start = f + i * bsize
      start.annotate(IR_GenerateRuntimeInversion.pointerArithmetic)
      val end = f + (i + 1) * bsize
      end.annotate(IR_GenerateRuntimeInversion.pointerArithmetic)
      stmts += IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression](start, end, fi))
    }
    f.annotate("noMatrixExpressionSetup")
    u.annotate("noMatrixExpressionSetup")

    // produce a function call to LUSolve for all blocks
    for (i <- 0 until m / bsize) {
      stmts += IR_FunctionCall(IR_PlainInternalFunctionReference(s"LUSolve_${ bsize }x${ bsize }", IR_UnitDatatype), Ablocks(i), fblocks(i), ublocks(i))
    }
    if (!IR_UserFunctions.get.functions.exists(f => f.name == s"LUSolve_${ bsize }x${ bsize }")) {
      IR_UserFunctions.get += genLUSolveAsFunction(bsize)
    }

    // write back to u
    for (i <- 0 until m / bsize) {
      val end = ublocks(i) + bsize
      end.annotate(IR_GenerateRuntimeInversion.pointerArithmetic)
      val target = u + bsize * i
      target.annotate(IR_GenerateRuntimeInversion.pointerArithmetic)

      stmts += IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype), ListBuffer[IR_Expression](ublocks(i), end, target))
    }
    systemCounter += 1
    stmts
  }

  import exastencils.baseExt.ir.IR_CompiletimeMatOps.mult

  def blockdiagonal(AasExpr : IR_MatrixExpression,u : IR_VariableAccess, f : IR_VariableAccess, msi : IR_MatShape,  m : Int, bsize : Int) : ListBuffer[IR_Statement] = {
    import exastencils.baseExt.ir.IR_CompiletimeMatOps.copySubMatrix
    val bsize = msi.size("block")
    val stmts = ListBuffer[IR_Statement]()
    val Ablocks = ListBuffer[IR_MatrixExpression]()
    // collect diagonal blocks
    for (i <- 0 until m / bsize) {
      Ablocks += copySubMatrix(AasExpr, i * bsize, i * bsize, bsize, bsize)
    }
    // solve system for each block
    val fexpr = IR_MatNodeUtils.accessToMatExpr(f)
    for (i <- 0 until m / bsize) {
      val ublock : IR_MatrixExpression =
        if (AasExpr.hasAnnotation("SolveMatSys:fallback_inverse")) {
          mult(IR_CompiletimeMatOps.cofactorInverse(Ablocks(i)), copySubMatrix(fexpr, i * bsize, 0, bsize, 1))
        } else {
          val LUP = IR_CompiletimeMatOps.LUDecomp(Ablocks(i))
          IR_MatrixSolveOps.forwardBackwardSub(LUP._1, copySubMatrix(fexpr, i * bsize, 0, bsize, 1), LUP._2)
        }
      for (j <- 0 until bsize) {
        stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i * bsize + j)), ublock.get(j, 0))
      }
    }
    stmts
  }

  var QRGLcolCounter : Int = 0
  var QRGLSysCounter : Int = 0
  def houseHolderMatrixWiki(x : IR_MatrixExpression, len : Int) : (IR_MatrixExpression, ListBuffer[IR_Statement]) = {
    val m = x.rows

    /* value of alpha needed for rt decision-> store in separate variable */
    val alphaAcc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_alpha_" + QRGLcolCounter, IR_RealDatatype)
    val alphaDecl = IR_VariableDeclaration(alphaAcc, norm(x))
    val x_1_Acc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_x_1_" + QRGLcolCounter, IR_RealDatatype)
    val x_1_Decl = IR_VariableDeclaration(x_1_Acc, x.get(0, 0))
    val decision = IR_IfCondition(IR_GreaterEqual(x_1_Acc, 0), ListBuffer[IR_Statement](
      IR_Assignment(alphaAcc, IR_Negative(alphaAcc))
    ))

    // x.set(0, 0, pivAcc - normAcc_x)
    x.set(0, 0, x.get(0, 0) - alphaAcc)
    val v = IR_MatrixExpression(x.innerDatatype, m, 1)

    val u_norm_Acc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_u_norm_" + QRGLcolCounter, IR_RealDatatype)
    val u_norm_Decl = IR_VariableDeclaration(u_norm_Acc, norm(x))

    QRGLcolCounter += 1

    for (i <- 0 until m) {
      v.set(i, 0, x.get(i, 0) / u_norm_Acc)
    }

    val tmp = IR_CompiletimeMatOps.mult(v, IR_CompiletimeMatOps.transpose(v))
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        if (i == j)
          tmp.set(i, j, IR_RealConstant(1) - 2 * tmp.get(i, j))
        else
          tmp.set(i, j, IR_RealConstant(-2) * tmp.get(i, j))
      }
    }

    (tmp, ListBuffer[IR_Statement](alphaDecl, x_1_Decl, decision, u_norm_Decl))
  }

  def houseHolderMatrixGolubLoan(x : IR_MatrixExpression, fullsize : Int) : (IR_MatrixExpression, ListBuffer[IR_Statement]) = {
    /* m = length(x) , sigma = x(2:m)T x(2:m) , v = [ x(2:m) ] */
    val m = x.rows
    val x_tail = IR_CompiletimeMatOps.copySubMatrix(x, 1, 0, m - 1, 1)
    val sigma = IR_CompiletimeMatOps.dotProduct(x_tail, x_tail)
    /* rt decision uses sigma -> save it in separate declaration */
    val sigmaAcc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_sigma_" + QRGLcolCounter, IR_RealDatatype)
    val sigmaDecl = IR_VariableDeclaration(sigmaAcc, sigma)

    /* rt decision depending on x_1 -> save it in separate declaration */
    val x_1Acc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_x_1_" + QRGLcolCounter, IR_RealDatatype)
    val x_1Decl = IR_VariableDeclaration(x_1Acc, x.get(0, 0))

    /* µ = (x(1)^2 + sigma)^(1/2) */
    val mu = IR_Power(x_1Acc * x_1Acc + sigmaAcc, 1.0 / 2.0)
    /* rt decision uses mu -> save it in separate declaration */
    val muAcc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_mu_" + QRGLcolCounter, IR_RealDatatype)
    val muDecl = IR_VariableDeclaration(muAcc, mu)

    /* rt decision modifies v_1 -> save it in separate declaration */
    val v_1Acc = IR_VariableAccess("QRMat_" + QRGLSysCounter + "_v_1_" + QRGLcolCounter, IR_RealDatatype)
    val v_1Decl = IR_VariableDeclaration(v_1Acc) /* add initial expressions at runtime */

    QRGLcolCounter += 1

    /* decision at runtime:
    if x(l) <= 0
    v(l) = x(l) - mu
    else
    v(l) = -sigma/(x(l) + mu)
    end
    */
    val decision = IR_IfCondition(IR_LowerEqual(x_1Acc, 0), ListBuffer[IR_Statement](
      IR_Assignment(v_1Acc, x_1Acc - muAcc)
    ), ListBuffer[IR_Statement](
      IR_Assignment(v_1Acc, IR_Negative(sigmaAcc / (x_1Acc + muAcc)))
    ))

    /* beta = (2*v(1)^2)/(sigma + v(1)^2),  v = x_tail/v(1)  */
    val beta = (2 * v_1Acc * v_1Acc) / (sigmaAcc + v_1Acc * v_1Acc)
    val v = IR_MatrixExpression(IR_RealDatatype, m, 1)
    for (i <- 1 until m) {
      v.set(i, 0, x.get(i, 0) / v_1Acc)
    }
    v.set(0, 0, 1.0)

    /* calculate HH matrix: Q_n = I - beta*v*v^T */
    /*
    var Q_n = unit(fullsize)
    val offset = fullsize - m
    for(i <- offset until fullsize) {
      for(j <- offset until fullsize) {
        if(i != j)
          Q_n.set(i,j, IR_Negative(beta * v.get(i - offset,0) * v.get(j - offset,0)))
        else
          Q_n.set(i,j, 1 - beta * v.get(i - offset,0) * v.get(j - offset,0))
      }
    }
    */
    var Q_n = unit(m)
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        if (i != j)
          Q_n.set(i, j, IR_Negative(beta * v.get(i, 0) * v.get(j, 0)))
        else
          Q_n.set(i, j, 1 - beta * v.get(i, 0) * v.get(j, 0))
      }
    }

    /* collect declarations */
    val decls = ListBuffer[IR_Statement](sigmaDecl, x_1Decl, muDecl, v_1Decl, decision)
    (Q_n, decls)
  }

  def norm(x : IR_MatrixExpression) : IR_Expression = {
    var norm = IR_Addition(IR_RealConstant(0))
    val m = if (x.rows == 1) x.columns else x.rows
    for (i <- 0 until m) {
      norm.summands += x.get(i, 0) * x.get(i, 0)
    }
    ir.IR_Power(norm, 1.0 / 2.0)
  }

  def unit(size : Int) : IR_MatrixExpression = {
    val tmp = IR_MatrixExpression(IR_RealDatatype, size, size)
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (i == j) {
          tmp.set(i, i, 1.0)
        } else tmp.set(i, j, 0.0)
      }
    }
    tmp
  }

  def specializedLMult(A : IR_MatrixExpression, B : IR_MatrixExpression, column_offset : Int) : Unit = {
    var tmp = IR_MatrixExpression(IR_RealDatatype, A.rows, B.columns)
    // calculate res in tmp
    for (i <- 0 until A.rows) {
      for (j <- column_offset until A.columns) {
        var T = IR_Addition(IR_RealConstant(0))
        for (k <- column_offset until A.columns) {
          T.summands += A.get(i, k) * B.get(k - column_offset, j - column_offset)
        }
        tmp.set(i, j - column_offset, Duplicate(T))
      }
    }
    // copy to position in A
    for (i <- 0 until A.rows) {
      for (j <- column_offset until A.columns) {
        A.set(i, j, tmp.get(i, j - column_offset))
      }
    }
  }

  def specializedRMult(A : IR_MatrixExpression, B : IR_MatrixExpression, row_offset : Int, column_offset : Int) : Unit = {
    var tmp = IR_MatrixExpression(IR_RealDatatype, A.rows, A.columns)
    val m = A.rows
    // calculate res in tmp
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        var T = IR_Addition(IR_RealConstant(0))
        for (k <- 0 until m) {
          T.summands += A.get(i, k) * B.get(row_offset + k, column_offset + j)
        }
        tmp.set(i, j, Duplicate(T))
      }
    }

    // copy to position in A
    for (i <- 0 until m) {
      for (j <- 0 until m) {
        B.set(i + row_offset, j + column_offset, tmp.get(i, j))
      }
    }
  }

  def QRDecomp(A : IR_MatrixExpression) : (IR_MatrixExpression, IR_MatrixExpression) = {
    val m = A.columns
    var R = Duplicate(A)
    var Q = unit(m)
    var pivBuffer = ListBuffer[IR_Statement]()

    // zero out all m columns
    for (i <- 0 until m - 1) {
      val x_i = copySubMatrix(R, i, i, m - i, 1)
      val tmp = houseHolderMatrixWiki(x_i, m)
      //val tmp = houseHolderMatrixGolubLoan(x_i, m)
      val Q_i = tmp._1
      pivBuffer ++= tmp._2

      IR_GeneralSimplify.applyStandalone(Q_i)
      //val qdeck = IR_VariableDeclaration(Q_i.datatype, "Q_" + i, Duplicate(Q_i))
      //pivBuffer += qdeck

      NodeCounter.countSubTree(Q_i, "Q_i", None, None)

      // zero out columns to retrieve R
      specializedRMult(Q_i, R, i, i)
      IR_GeneralSimplify.applyStandalone(R)

      NodeCounter.countSubTree(R, "R_i", None, None)


      // accumulate Q
      if (i == 0)
        Q = transpose(Q_i)
      else
        specializedLMult(Q, transpose(Q_i), i)
      IR_GeneralSimplify.applyStandalone(Q)

      //val qdeck2 = IR_VariableDeclaration(Q.datatype, "Q_whole_" + i, Duplicate(Q))
      //pivBuffer += qdeck2

    }

    // val decl = IR_VariableDeclaration(Q.datatype, "Q", Duplicate(Q))
    // pivBuffer += decl

    NodeCounter.countSubTree(Q, "Q_presimpl", None, None)
    NodeCounter.countSubTree(R, "R_presimpl", None, None)

    IR_GeneralSimplify.applyStandalone(Q)
    IR_GeneralSimplify.applyStandalone(R)

    NodeCounter.countSubTree(Q, "Q_postsimpl", None, None)
    NodeCounter.countSubTree(R, "R_postsimpl", None, None)

    R.annotate("QRPivot", pivBuffer)
    QRGLSysCounter += 1
    (Q, R)
  }
}
