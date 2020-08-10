
package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_GreaterEqual
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_PlainInternalFunctionReference
import exastencils.base.ir.IR_PreDecrement
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_BasicMatrixOperations
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateRuntimeInversion.localLUDecomp
import exastencils.baseExt.ir.IR_MatShape
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.prettyprinting.PpStream

object IR_SolveLinearSystem {
  def apply(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess) : IR_SolveLinearSystem = {
    new IR_SolveLinearSystem(A, u, f)
  }

  def apply(A : IR_Expression, u : IR_Expression, f : IR_Expression) = {
    var uacc : IR_VariableAccess = u match {
      case acc : IR_VariableAccess => acc
      case _                       => Logger.error(s"unexpected datatype for u ${ u.datatype }")
    }
    var facc : IR_VariableAccess = f match {
      case acc : IR_VariableAccess => acc
      case _                       => Logger.error(s"unexpected datatype for f ${ f.datatype }")
    }
  }
}

case class IR_SolveLinearSystem(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess) extends IR_Statement {

  override def prettyprint(out : PpStream) : Unit = out << "solveLES" << A.prettyprint(out) << ", " << f.prettyprint(out)

  def expand(AasExpr : IR_MatrixExpression) : Transformation.OutputType = {

    val msi : IR_MatShape = AasExpr.shape.getOrElse(IR_MatShape("filled"))
    Logger.warn(s"Solving linear system with the following configuration: ${ Knowledge.experimental_resolveInverseFunctionCall }, " + msi.toStringList())
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
          schurDomainDecomp(AasExpr)
        // Fallback1: solve by inverting A with given structure for Schur with size(D) > 1 or blockdiagonal
        case _ if (msi.shape != "filled") => IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), ListBuffer[IR_Expression](A) ++= msi.toExprList()), f))
        case _ if (m > 1 && m < 4) => // solve by inverse for small matrices
          IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), ListBuffer[IR_Expression](A) ++= msi.toExprList()), f))
        // Fallback2: solve with lu for filled matrices larger than 3
        case _ if (m > 3)          =>
          Logger.warn(s"solving LES with lu")
          if (Knowledge.experimental_resolveInverseFunctionCall == "Runtime") {

            var stmts = ListBuffer[IR_Statement]()
            var P = IR_VariableAccess("P", IR_ArrayDatatype(IR_IntegerDatatype, m + 1))
            var AasAcc = IR_VariableAccess("A", IR_MatrixDatatype(AasExpr.innerDatatype.get, AasExpr.rows, AasExpr.columns))
            var fasAcc = IR_VariableAccess("f", IR_MatrixDatatype(AasExpr.innerDatatype.get, AasExpr.rows, 1))
            stmts += IR_VariableDeclaration(AasAcc, AasExpr)
            stmts += IR_VariableDeclaration(fasAcc, f)
            stmts += IR_VariableDeclaration(P)

            // lu
            stmts ++= localLUDecomp(AasAcc, P, m, IR_IntegerConstant(0), IR_IntegerConstant(0))
            // forward backward sub
            stmts ++= genForwardBackwardSub(AasAcc, P, fasAcc, u)
          } else
          //TODO solve evaluation problem here: if A consists of variables i can not get the value of the entry
          try {
            IR_Assignment(u, luSolveCT(AasExpr, IR_MatNodeUtils.accessToExpression(f)))
          } catch {
            case ev : EvaluationException =>
              Logger.warn("matrix entry not evaluatable, switching to inversion strategy!")
              IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), ListBuffer[IR_Expression](A) ++= msi.toExprList()), f))
          }
      }
    }
  }

  def luSolveCT(A : IR_MatrixExpression, b : IR_MatrixExpression) : IR_MatrixExpression = {
    //var LU = IR_MatrixExpression(that.innerDatatype, that.rows, that.columns)
    var LU = Duplicate(A)
    val N = A.columns
    if (N != A.rows) Logger.error("can only decompose quadratic matrices with LU")
    var P = new Array[Int](N)
    for (i <- 0 until N) P(i) = i
    var absA : Double = 0.0
    //Logger.error(s"matrix before ${LU}")
    for (i <- 0 until N) {

      // pivot
      var maxA : Double = 0.0
      var imax : Int = i
      for (k <- i until N) {
  /*
        var value_at_ki : Double = 0.0
        try {
          value_at_ki = IR_SimplifyExpression.evalFloating(LU.get(k, i))
        } catch {
          case ev : EvaluationException => Logger.error("value not evaluatable, can not pivot here!, switching to ")
        }
*/

        var value_at_ki : Double = A.get(i,k) match {
          case n : IR_Number => n.value.asInstanceOf[Number].doubleValue
          case _                                      => throw EvaluationException("entry not evaluatable")
        }

        absA = value_at_ki.abs
        if (absA > maxA) {
          maxA = absA
          imax = k
        }
      }
      //        if(maxA < scala.math.pow(10,-15)) Logger.error("very small entry is largest, unstable!")
      if (imax != i) {
        var tmp = P(i)
        P(i) = P(imax)
        P(imax) = tmp
        for (j <- 0 until N) {
          var tmp = Duplicate(LU.get(i, j))
          LU.set(i, j, Duplicate(LU.get(imax, j)))
          LU.set(imax, j, tmp)
        }
      }

      // decompose
      for (j <- i + 1 until N) {
        val tmp1 = Duplicate(LU.get(j, i)) / Duplicate(LU.get(i, i))
        LU.set(j, i, tmp1)
        for (k <- i + 1 until N) {
          val tmp2 = Duplicate(LU.get(j, k)) - (Duplicate(LU.get(j, i)) * Duplicate(LU.get(i, k)))
          LU.set(j, k, tmp2)
        }
      }
    }

    // solve
    var u_loc = IR_MatrixExpression(A.innerDatatype, A.columns, 1)
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

    u_loc
  }

  def schurDomainDecomp(A : IR_MatrixExpression) : Transformation.OutputType = {
    var stmts = ListBuffer[IR_Statement]()
    val msi = A.shape.get
    // blocksizes
    val bsize = msi.size("block")
    val bsizeA = msi.size("Ablock")
    val size = A.columns
    val bsizeD = size - bsize
    val nComponents = bsize / bsizeA

    // accesses for lists of variable accesses representing system matrix, rhs and solution
    def vecAcc(vec : ListBuffer[IR_VariableAccess], i0 : Int) = IR_HighDimAccess(vec(i0 / bsizeA), IR_ConstIndex(i0 % bsizeA))

    def rowBlockMatAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i0 / bsizeA), IR_ConstIndex(i0 % bsizeA, i1 % bsizeA))

    def colBlockMatAcc(mat : ListBuffer[IR_VariableAccess], i0 : Int, i1 : Int) = IR_HighDimAccess(mat(i1 / bsizeA), IR_ConstIndex(i0 % bsizeA, i1 % bsizeA))

    def genericAcc(mat : IR_Expression, i0 : Int, i1 : Int) = IR_BasicMatrixOperations.getElem(mat, i0, i1)

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

  def genForwardBackwardSub(A : IR_VariableAccess, P : IR_VariableAccess, f : IR_VariableAccess, u : IR_VariableAccess) : ListBuffer[IR_Statement] = {
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

}