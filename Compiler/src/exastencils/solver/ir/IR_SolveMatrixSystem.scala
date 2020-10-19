
package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base
import exastencils.base.ir
import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Addition
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_GreaterEqual
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_PlainFunction
import exastencils.base.ir.IR_PlainInternalFunctionReference
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
import exastencils.prettyprinting.PpStream

/// solve local linear system
object IR_SolveMatrixSystem {
  def apply(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess) : IR_SolveMatrixSystem = {
    new IR_SolveMatrixSystem(A, u, f)
  }

}

case class IR_SolveMatrixSystem(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess, shape : Option[IR_MatShape] = None) extends IR_Statement {
  var systemCounter : Int = 0

  override def prettyprint(out : PpStream) : Unit = out << "solveMatSys" << A.prettyprint(out) << ", " << f.prettyprint(out)

  def expand(AasExpr : IR_MatrixExpression) : Transformation.OutputType = {

    val msi : IR_MatShape = if (AasExpr.shape.isDefined) AasExpr.shape.get else shape.getOrElse(IR_MatShape("filled"))
    val (m, n) = AasExpr.datatype match {
      case mat : IR_MatrixDatatype => (mat.sizeM, mat.sizeN)
      case _                       => Logger.error(s"unexpected datatype of A: ${ A.datatype }")
    }
    if (m != n) Logger.error("expected quadratic system matrix")
    val (k, i) = f.datatype match {
      case mat : IR_MatrixDatatype            => (mat.sizeM, mat.sizeN)
      case s if (IR_MatNodeUtils.isScalar(f)) => (1, 1)
      case _                                  => Logger.error(s"unexpected datatype of f: ${ A.datatype }")
    }
    if (k != n) Logger.error("f and A do not match in size")
    u.datatype match {
      case mat : IR_MatrixDatatype            =>
        if (mat.sizeM != n) Logger.error("u and A do not match in size")
        if (mat.sizeN != i) Logger.error("u and f do not match in size")
      case s if (IR_MatNodeUtils.isScalar(u)) =>
      case _                                  => Logger.error(s"unexpected datatype of f: ${ A.datatype }")
    }

    if (Knowledge.experimental_matrixDebugConfig)
      Logger.warn(s"Solving linear system with the following configuration: ${ Knowledge.experimental_resolveLocalMatSys }, ${ msi.shape },  ${ m }, ${ n }")

    // scalar system
    if (m == 1 && n == 1) {
      IR_Assignment(u, IR_Division(f, A))
    } else {
      msi.shape match {
        case "diagonal" =>
          var stmts = ListBuffer[IR_Statement]()
          for (i <- 0 until m) {
            stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i, 0)), IR_Division(IR_HighDimAccess(f, IR_ConstIndex(i)), IR_HighDimAccess(A, IR_ConstIndex(i, i))))
          }
          stmts

        // in case of schur: solve with A-Decomposition to helper matrices
        // only for blocksize of D == 1
        case "schur" if (m - msi.size("block") == 1) =>
          schurDomainDecomp(AasExpr, msi)

        // blockdiagonal systems are solved by LU at runtime or compiletime
        case _ if (msi.shape == "blockdiagonal") =>
          val bsize = msi.size("block")
          if (Knowledge.experimental_resolveLocalMatSys == "Runtime")
            genBlockdiagonal(msi, AasExpr, m, bsize)
          else blockdiagonal(msi, AasExpr, m, bsize)

        case _ if(msi.shape == "QR") => {
          // QR decomp
          val QR = qrDecomp(IR_MatNodeUtils.exprToMatExpr(AasExpr))
          val y = IR_CompiletimeMatOps.mult(IR_CompiletimeMatOps.transpose(QR._1), IR_MatNodeUtils.accessToMatExpr(f))

          NodeCounter.countSubTree(QR._1, "Q", None, None)
          NodeCounter.countSubTree(QR._2, "R", None, None)
          NodeCounter.countSubTree(y, "y", None, None)


          // solve resulting systems: orthogonal by inversion, triangular by backward substitution
          val x = IR_MatrixExpression(IR_RealDatatype, m, 1)
          for(i <- 0 until m)
            x.set(i,0,IR_RealConstant(0))
          for (i <- m - 1 to 0 by -1) {
            for (k <- i + 1 until m) {
              x.set(i, 0, Duplicate(x.get(i, 0)) - Duplicate(QR._2.get(i, k)) * Duplicate(QR._2.get(k, 0)))
            }
            x.set(i, 0, Duplicate(x.get(i, 0)) / Duplicate(QR._2.get(i, i)))
          }
          NodeCounter.countSubTree(x, "x", None, None)

          // transfer annotations
          x.annotate("QRPivot", QR._1.popAnnotationAs("QRPivot"))
          IR_Assignment(u, x)
        }

        // solve by inverting A with given structure for Schur with size(D) > 1  or fallback due to impossible pivoting in LU
        case _ if A.hasAnnotation("SolveMatSys:fallback_inverse") =>
          if (msi.shape == "fallback_inverse") msi.shape = "cofactors"
          val args = ListBuffer[IR_Expression](A) ++= msi.toExprList()
          IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), args), f))

        // solve with lu
        case _ =>
          if (Knowledge.experimental_matrixDebugConfig)
            Logger.warn(s"solving localMatSys with lu at ${ Knowledge.experimental_resolveLocalMatSys }")

          if (Knowledge.experimental_resolveLocalMatSys == "Runtime") {
            var stmts = ListBuffer[IR_Statement]()
            var AasAcc = IR_VariableAccess("A", IR_MatrixDatatype(AasExpr.innerDatatype.get, AasExpr.rows, AasExpr.columns))
            stmts += IR_VariableDeclaration(AasAcc, AasExpr)
            stmts ++= genLUSolveInlined(AasAcc, m, f, u)

          } else {
            val LUP = IR_CompiletimeMatOps.LUDecomp(AasExpr)
            IR_Assignment(u, IR_CompiletimeMatOps.forwardBackwardSub(LUP._1, IR_MatNodeUtils.accessToMatExpr(f), LUP._2))
          }
      }
    }
  }

  var QRpivCounter = 0
  var QRsysCounter = 0

  def houseHolderMatrixWiki(x : IR_MatrixExpression, len : Int) : (IR_MatrixExpression, ListBuffer[IR_VariableDeclaration]) = {
    val simplified = IR_CompiletimeMatOps.evalNumExprWrapper(x.get(0, 0))
    if (simplified.isDefined) x.set(0, 0, simplified.get)
    val alpha = if(x.get(0,0).asInstanceOf[IR_RealConstant].v >= 0) IR_Negative(norm(x)) else norm(x)
 /*   val normAcc_x = IR_VariableAccess("sys_" + QRsysCounter + "QRNorm_x_" + QRpivCounter , IR_RealDatatype)
   val normDecl_x = IR_VariableDeclaration(normAcc_x, alpha)

    val pivAcc = IR_VariableAccess("sys_" + QRsysCounter + "QRPiv_" + QRpivCounter , IR_RealDatatype)
    val piv = IR_VariableDeclaration(pivAcc, Duplicate(x.get(0, 0)))
*/

   // x.set(0, 0, pivAcc - normAcc_x)
    x.set(0, 0, Duplicate(x.get(0,0)) - alpha)
    val m = x.rows
    val v = IR_MatrixExpression(x.innerDatatype, m, 1)
    val norm_u = norm(x)
/*
    val normAcc_u = IR_VariableAccess("sys_" + QRsysCounter + "QRNorm_u_" + QRpivCounter , IR_RealDatatype)
    QRpivCounter += 1
    val normDecl_u = IR_VariableDeclaration(normAcc_u, norm_u)
*/

    for (i <- 0 until m) {
      v.set(i, 0, x.get(i, 0) / norm_u)
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

    (tmp, ListBuffer[IR_VariableDeclaration](/*piv, normDecl_x, normDecl_u*/))
  }

  /*
  def houseHolderMatrixGolubLoan(x : IR_MatrixExpression) : IR_MatrixExpression = {
    val m = x.rows
    val x_tail = IR_CompiletimeMatOps.copySubMatrix(x,1,0, m-1, 1)
    val sigma = IR_CompiletimeMatOps.dotProduct(x_tail, x_tail)
    val mu = IR_Power(x.get(0,0)*x.get(0,0) + sigma, 1.0/2.0)
    val beta = 2*
  }*/

  def norm(x : IR_MatrixExpression) : IR_Expression = {
    var norm = IR_Addition(IR_RealConstant(0))
    val m = if (x.rows == 1) x.columns else x.rows
    for (i <- 0 until m) {
      norm.summands += x.get(i, 0) * x.get(i, 0)
    }
    ir.IR_Power(norm, 1.0 / 2.0)
  }

  // multiplication of a quadratic submatrix of A with a smaller, identical sized quadratic matrix B
  def QLMultSubmat(A : IR_MatrixExpression, B : IR_MatrixExpression, y : Int, x : Int) : Unit = {
    var tmp = IR_MatrixExpression(IR_RealDatatype, B.rows, B.columns)
    val m = B.rows
    // calculate res in tmp
    for(i <- 0 until m) {
      for(j <- 0 until m) {
        var T = IR_Addition(IR_RealConstant(0))
        for(k <- 0 until m) {
          T.summands += A.get(y + i, x + k) * B.get(k, j)
        }
        tmp.set(i,j,T)
      }
    }
    // copy to position in A
    for(i <- 0 until m) {
      for (j <- 0 until m) {
        A.set(y + i, x + j, tmp.get(i,j))
      }
    }
  }
  def QRMultSubmat(A : IR_MatrixExpression, B : IR_MatrixExpression, y : Int, x : Int) : Unit = {
    var tmp = IR_MatrixExpression(IR_RealDatatype, A.rows, A.columns)
    val m = A.rows
    // calculate res in tmp
    for(i <- 0 until m) {
      for(j <- 0 until m) {
        var T = IR_Addition(IR_RealConstant(0))
        for(k <- 0 until m) {
          T.summands += A.get(k,j) * B.get(y + k, x + j)
        }
        tmp.set(i,j,T)
      }
    }
    // copy to position in A
    for(i <- 0 until m) {
      for (j <- 0 until m) {
        B.set(y + i, x + j, tmp.get(i,j))
      }
    }
  }

  import exastencils.baseExt.ir.IR_CompiletimeMatOps.copySubMatrix
  import exastencils.baseExt.ir.IR_CompiletimeMatOps.transpose


  def qrDecomp(A : IR_MatrixExpression) : (IR_MatrixExpression, IR_MatrixExpression) = {

    val m = A.columns
    var R = Duplicate(A)
    var Q = IR_MatrixExpression(A.innerDatatype, m, m)
    var pivBuffer = ListBuffer[IR_VariableDeclaration]()

    // zero out all m columns
    for (i <- 0 until m) {
      val x_i = copySubMatrix(R, i, i, m - i, 1)
      val tmp = houseHolderMatrixWiki(x_i, m)
      val Q_i = tmp._1
      pivBuffer ++= tmp._2

      // zero out columns to retrieve R
      QRMultSubmat(Q_i, R, i, i)

      // accumulate Q
      if (i == 0) {
        Q = transpose(Q_i)
      } else {
        QLMultSubmat(Q, transpose(Q_i), i, i)
      }

      for (i <- 0 until m) {
        for (j <- 0 until m) {
          var simplified = IR_CompiletimeMatOps.evalNumExprWrapper(R.get(i, j))
          if (simplified.isDefined) R.set(i, j, simplified.get)
          simplified = IR_CompiletimeMatOps.evalNumExprWrapper(Q.get(i, j))
          if (simplified.isDefined) Q.set(i, j, simplified.get)
        }
      }

      IR_GeneralSimplify.applyStandalone(Q)
      IR_GeneralSimplify.applyStandalone(R)


      NodeCounter.countSubTree(Q, "Q", None, None)
      NodeCounter.countSubTree(Q_i, "Q_" + i, None, None)
      NodeCounter.countSubTree(R, "R_" + i, None, None)
    }



    Q.annotate("QRPivot", pivBuffer)
    (Q, R)
  }

  def schurDomainDecomp(A : IR_MatrixExpression, msi : IR_MatShape) : Transformation.OutputType = {
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
    stmts += base.ir.IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, N), IR_PreIncrement(i), ListBuffer[IR_Statement](
      IR_VariableDeclaration(tmp_idx, IR_ArrayAccess(P, i)),
      IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_HighDimAccess(f, IR_ExpressionIndex(tmp_idx))),
      base.ir.IR_ForLoop(IR_VariableDeclaration(k, 0), IR_Lower(k, i), IR_PreIncrement(k), ListBuffer[IR_Statement](
        IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_Multiplication(IR_HighDimAccess(A, IR_ExpressionIndex(i, k)), IR_HighDimAccess(u, IR_ExpressionIndex(k))), "-=")
      ))
    ))
    stmts += base.ir.IR_ForLoop(IR_VariableDeclaration(i, N - 1), IR_GreaterEqual(i, 0), IR_PreDecrement(i),
      base.ir.IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, N), IR_PreIncrement(k),
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

  def genBlockdiagonal(msi : IR_MatShape, AasExpr : IR_MatrixExpression, m : Int, bsize : Int) : ListBuffer[IR_Statement] = {
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

  def blockdiagonal(msi : IR_MatShape, AasExpr : IR_MatrixExpression, m : Int, bsize : Int) : ListBuffer[IR_Statement] = {
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
        IR_CompiletimeMatOps.forwardBackwardSub(LUP._1, copySubMatrix(fexpr, i * bsize, 0, bsize, 1), LUP._2)
      }
      for (j <- 0 until bsize) {
        stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i * bsize + j)), ublock.get(j, 0))
      }
    }
    stmts
  }

}