
package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base
import exastencils.base.ir.{IR_Assignment, IR_ConstIndex, IR_Division, IR_Expression, IR_ExpressionIndex, IR_ExternalFunctionReference, IR_FunctionCall, IR_GreaterEqual, IR_HighDimAccess, IR_IntegerDatatype, IR_Lower, IR_Multiplication, IR_Number, IR_PlainInternalFunctionReference, IR_PreDecrement, IR_PreIncrement, IR_RealConstant, IR_RealDatatype, IR_Statement, IR_UnitDatatype, IR_VariableAccess, IR_VariableDeclaration}
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_ClassifyMatShape
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.baseExt.ir.IR_MatShape
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.baseExt.ir.IR_MatOperations.{IR_EvalMOpRuntimeExe, IR_GenerateRuntimeInversion}
import exastencils.config.Knowledge
import exastencils.core.NodeCounter
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import isl.Val

/// solve local linear system
object IR_SolveMatrixSystem {
  def apply(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess) : IR_SolveMatrixSystem = {
    new IR_SolveMatrixSystem(A, u, f)
  }
  var local_A_count : Int = 0
}

case class IR_SolveMatrixSystem(A : IR_Expression, u : IR_VariableAccess, f : IR_VariableAccess, shape : Option[IR_MatShape] = None) extends IR_Statement {

  override def prettyprint(out : PpStream) : Unit = out << "solveMatSys" << A.prettyprint(out) << ", " << f.prettyprint(out)

  def expand() : Transformation.OutputType = {

    val AasExpr = retrieveMatExpr()
    var AasAcc = IR_VariableAccess(s"local_A_${IR_SolveMatrixSystem.local_A_count}", IR_MatrixDatatype(AasExpr.innerDatatype.get, AasExpr.rows, AasExpr.columns))


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

    val timeOfExe = IR_EvalMOpRuntimeExe("localsystem", m)
    if (Knowledge.experimental_matrixDebugConfig)
      Logger.warn(s"Solving linear system with the following configuration: ${ Knowledge.experimental_resolveLocalMatSys }, ${ msi.shape },  ${ m }, ${ n }")


    // scalar system
    if (m == 1 && n == 1) {
      IR_Assignment(u, IR_Division(f, A))
    } else {
      msi.shape match {
        case "diagonal" =>
          if (Knowledge.experimental_matrixDebugConfig)
            Logger.warn("Solving diagonal local system directly")
          var stmts = ListBuffer[IR_Statement]()
          for (i <- 0 until m) {
            stmts += IR_Assignment(IR_HighDimAccess(u, IR_ConstIndex(i, 0)), IR_Division(IR_HighDimAccess(f, IR_ConstIndex(i)), IR_HighDimAccess(A, IR_ConstIndex(i, i))))
          }
          stmts

        case "schur" =>
          // certain schur shapes solvable by domain decomposition
          if(m - msi.size("block") == 1) {
            if (Knowledge.experimental_matrixDebugConfig)
              Logger.warn("Solving local system by Schur domain decomposition ")
            // in case of schur: solve with A-Decomposition to helper matrices
            // only for blocksize of D == 1
            IR_MatrixSolveOps.schurDomainDecomp(AasExpr, u, f, msi)
          } else {

            if (Knowledge.experimental_matrixDebugConfig)
              Logger.warn(s"Solving local system by Schur inversion at ${timeOfExe}")
            // else use schur inversion to solve the system
            var stmts = ListBuffer[IR_Statement]()
            var AInv = IR_VariableAccess(s"local_AInv_${IR_SolveMatrixSystem.local_A_count}", IR_MatrixDatatype(AasExpr.innerDatatype.get, AasExpr.rows, AasExpr.columns))
            IR_SolveMatrixSystem.local_A_count = IR_SolveMatrixSystem.local_A_count + 1
            if(timeOfExe == "Runtime") {
              stmts += IR_VariableDeclaration(AInv, IR_RealConstant(0))
              stmts += IR_VariableDeclaration(AasAcc, AasExpr)
              stmts += IR_GenerateRuntimeInversion.schurInlined(AasAcc,msi.size("block"),msi.shape("block"),msi.size("Ablock"),AInv)
            } else {
              stmts += IR_VariableDeclaration(AInv, IR_CompiletimeMatOps.schur(AasExpr, n-msi.size("block"), msi.size("block"), msi))
            }
            stmts += IR_Assignment(u, AInv*f)
          }
        // blockdiagonal systems are solved by LU at runtime or compiletime
        case _ if (msi.shape == "blockdiagonal") =>
          val bsize = msi.size("block")
          if (Knowledge.experimental_matrixDebugConfig)
            Logger.warn(s"Solving blockdiagonal local system at ${timeOfExe}")
          if (timeOfExe == "Runtime") {
            IR_MatrixSolveOps.genBlockdiagonal(AasExpr, u, f, msi, m, bsize)
          } else {
            IR_MatrixSolveOps.blockdiagonal(AasExpr, u, f, msi, m, bsize)
          }

        case _ if (msi.shape == "QR") => {
          if(Knowledge.experimental_matrixDebugConfig)
            Logger.warn("Solving local system by QR Decomposition at Runtime")
          // QR decomp
          val QR = IR_MatrixSolveOps.QRDecomp(IR_MatNodeUtils.exprToMatExpr(AasExpr))

          // solve resulting systems: orthogonal by inversion, triangular by backward substitution
          val y = IR_CompiletimeMatOps.mult(IR_CompiletimeMatOps.transpose(QR._1), IR_MatNodeUtils.accessToMatExpr(f))

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

        // solve with lu at runtime or cofactor at compiletime
        case _ =>

          if (timeOfExe == "Runtime") {
            if(Knowledge.experimental_matrixDebugConfig)
              Logger.warn("Solving local system by LU Decomposition at Runtime")
            var stmts = ListBuffer[IR_Statement]()
            stmts += IR_VariableDeclaration(AasAcc, AasExpr)
            IR_SolveMatrixSystem.local_A_count = IR_SolveMatrixSystem.local_A_count + 1

            // inline LUSolve if CUDA is enabled to avoid separate compilation units
            if(Knowledge.cuda_enabled) {
              stmts ++= IR_MatrixSolveOps.genLUSolveInlined(AasAcc, m, f, u)
            } else {
              if (!IR_UserFunctions.get.functions.exists(f => f.name == s"LUSolve_${ m }x${ m }")) {
                IR_UserFunctions.get += IR_MatrixSolveOps.genLUSolveAsFunction(m)
              }
              stmts += IR_FunctionCall(IR_PlainInternalFunctionReference(s"LUSolve_${ m }x${ m }", IR_UnitDatatype), ListBuffer[IR_Expression](AasAcc, f, u))
            }
          } else {
            if(Knowledge.experimental_matrixDebugConfig)
              Logger.warn("Solving local system by Cofactor inversion at Compiletime")
            // obsolete
            //val LUP = IR_CompiletimeMatOps.LUDecomp(AasExpr)
            //val sol = IR_MatrixSolveOps.forwardBackwardSub(LUP._1, IR_MatNodeUtils.accessToMatExpr(f), LUP._2)
            // solve system by inversion, but:
            // at compiletime no pivoting possible, such that LU decomposition is unstable
            // performance drawback only at compiletime
            IR_Assignment(u, IR_CompiletimeMatOps.inverse(AasExpr,shape.getOrElse(IR_MatShape("filled")))*f)
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
          else {
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
