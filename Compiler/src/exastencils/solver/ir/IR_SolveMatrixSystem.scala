
package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base
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
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_PlainInternalFunctionReference
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_PreDecrement
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ClassifyMatShape
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.baseExt.ir.IR_MatShape
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.core.NodeCounter
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// solve local linear system
object IR_SolveMatrixSystem {
  def apply(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess) : IR_SolveMatrixSystem = {
    new IR_SolveMatrixSystem(A, u, f)
  }

}

case class IR_SolveMatrixSystem(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess, shape : Option[IR_MatShape] = None) extends IR_Statement {

  override def prettyprint(out : PpStream) : Unit = out << "solveMatSys" << A.prettyprint(out) << ", " << f.prettyprint(out)
  var local_A_counter : Int = 0

  def expand() : Transformation.OutputType = {

    val AasExpr = retrieveMatExpr()

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
          val args = ListBuffer[IR_Expression](A) ++= msi.toExprList()
          //Logger.warn("switching back to inversion")
          //IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), args), f))

          IR_MatrixSolveOps.schurDomainDecomp(AasExpr, u, f, msi)

        // blockdiagonal systems are solved by LU at runtime or compiletime
        case _ if (msi.shape == "blockdiagonal") =>
          val bsize = msi.size("block")
          if (Knowledge.experimental_resolveLocalMatSys == "Runtime")
            IR_MatrixSolveOps.genBlockdiagonal(AasExpr, u, f, msi, m, bsize)
          else IR_MatrixSolveOps.blockdiagonal(AasExpr, u, f, msi, m, bsize)

        case _ if (msi.shape == "QR") => {
          // QR decomp
          val QR = IR_MatrixSolveOps.QRDecomp(IR_MatNodeUtils.exprToMatExpr(AasExpr))

          // solve resulting systems: orthogonal by inversion, triangular by backward substitution
          val y = IR_CompiletimeMatOps.mult(IR_CompiletimeMatOps.transpose(QR._1), IR_MatNodeUtils.accessToMatExpr(f))

          if (Knowledge.experimental_matrixDebugConfig) {
            NodeCounter.countSubTree(QR._1, "Q", None, None)
            NodeCounter.countSubTree(QR._2, "R", None, None)
            NodeCounter.countSubTree(y, "y", None, None)
          }

          // transfer annotations
          y.annotate("QRPivot", QR._2.popAnnotationAs("QRPivot"))

          // generate backward sub as there the most nodes are produced
          var i = IR_VariableAccess("i", IR_IntegerDatatype)
          var k = IR_VariableAccess("k", IR_IntegerDatatype)
          var RAcc = IR_VariableAccess("QRSolve_R_" + IR_MatrixSolveOps.QRGLSysCounter, QR._2.datatype)
          ListBuffer[IR_Statement](
            IR_Assignment(u, y),
            IR_VariableDeclaration(RAcc, QR._2),
            base.ir.IR_ForLoop(IR_VariableDeclaration(i, m - 1), IR_GreaterEqual(i, 0), IR_PreDecrement(i),
              base.ir.IR_ForLoop(IR_VariableDeclaration(k, i + 1), IR_Lower(k, m), IR_PreIncrement(k),
                IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_Multiplication(IR_HighDimAccess(RAcc, IR_ExpressionIndex(i, k)), IR_HighDimAccess(u, IR_ExpressionIndex(k))), "-=")
              ),
              IR_Assignment(IR_HighDimAccess(u, IR_ExpressionIndex(i)), IR_HighDimAccess(RAcc, IR_ExpressionIndex(i, i)), "/=")
            ))
        }

        // solve by inverting A with given structure for Schur with size(D) > 1  or fallback due to impossible pivoting in LU
        case _ if A.hasAnnotation("SolveMatSys:fallback_inverse") =>
          if (msi.shape == "fallback_inverse") msi.shape = "cofactors"
          val args = ListBuffer[IR_Expression](A) ++= msi.toExprList()
          IR_Assignment(u, IR_Multiplication(IR_FunctionCall(IR_ExternalFunctionReference("inverse", A.datatype), args), f))

        // solve with lu
        case _ =>
          if (Knowledge.experimental_matrixDebugConfig) {
            Logger.warn(s"solving localMatSys with lu at ${ Knowledge.experimental_resolveLocalMatSys }")
          }

          if (Knowledge.experimental_resolveLocalMatSys == "Runtime") {
            var stmts = ListBuffer[IR_Statement]()
            var AasAcc = IR_VariableAccess("local_A_{local_A_counter}", IR_MatrixDatatype(AasExpr.innerDatatype.get, AasExpr.rows, AasExpr.columns))
            local_A_counter++
            stmts += IR_VariableDeclaration(AasAcc, AasExpr)

            if (!IR_UserFunctions.get.functions.exists(f => f.name == s"LUSolve_${ m }x${ m }")) {
              IR_UserFunctions.get += IR_MatrixSolveOps.genLUSolveAsFunction(m)
            }
            stmts += IR_FunctionCall(IR_PlainInternalFunctionReference(s"LUSolve_${ m }x${ m }", IR_UnitDatatype), ListBuffer[IR_Expression](AasAcc, f, u))
         //   stmts ++= IR_MatrixSolveOps.genLUSolveInlined(AasAcc, m, f, u)
          } else {
            val LUP = IR_CompiletimeMatOps.LUDecomp(AasExpr)
            val sol = IR_MatrixSolveOps.forwardBackwardSub(LUP._1, IR_MatNodeUtils.accessToMatExpr(f), LUP._2)
            if (Knowledge.experimental_matrixDebugConfig)
              NodeCounter.countSubTree(sol, "x_LU", None, None)
            IR_Assignment(u, sol)
          }
      }
    }
  }

  def retrieveMatExpr() : IR_MatrixExpression = {
    A match {
      case x : IR_MatrixExpression =>
        // to classify and const -> classify
        if (Knowledge.experimental_classifyLocMat) {
          x.shape = Some(IR_ClassifyMatShape(x))
          x
        }
        // const and shape not to classify
        else x
      // else: variable access: find initialization expression in declaration
      case va : IR_VariableAccess =>
        val initOpt : Option[IR_Expression] = IR_ResolveLocalSolve.variableCollector.getConstInitVal(va.name)
        if (initOpt.isEmpty) {
          if (Knowledge.experimental_classifyLocMat || (shape.isDefined && shape.get.toClassify()))
            Logger.error("can not classify local matrix if system matrix is not constant!")
          else if (Knowledge.experimental_resolveLocalMatSys == "compiletime") {
            Logger.warn("Compiletime LU without effective pivoting will most likely fail, switching back to inversion with cofactors!")
            val A = IR_MatNodeUtils.accessToMatExpr(va)
            A.annotate("SolveMatSys:fallback_inverse")
            A
          } else {
            IR_MatNodeUtils.accessToMatExpr(va)
          }
        } else {
          if (Knowledge.experimental_matrixDebugConfig)
            Logger.warn("pivoting initial expression for solveMatSys")
          val A = initOpt.get
          if (Knowledge.experimental_classifyLocMat || (shape.isDefined && shape.get.toClassify())) {
            if (!A.isInstanceOf[IR_MatrixExpression]) Logger.error("can not classify nonmatrix init value")
            A.asInstanceOf[IR_MatrixExpression].shape = Some(IR_ClassifyMatShape(A.asInstanceOf[IR_MatrixExpression]))
            if(Knowledge.experimental_matrixDebugConfig)
              Logger.warn(s"classified local matrix as: ${A.asInstanceOf[IR_MatrixExpression].shape.get.toStringList()}")
          }
          A match {
            case n : IR_Number => IR_MatrixExpression.fromSingleExpression(n.datatype, 1, 1, n)
            case me : IR_MatrixExpression => me
          }
        }
    }
  }

}
