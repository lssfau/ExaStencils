package exastencils.hack.ir

import scala.collection.mutable.ListBuffer
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.parallelization.api.cuda._
import exastencils.parallelization.api.mpi._

/// HACK_IR_ResolveSpecialFunctionsAndConstants

// TODO: split according to functionality and move to appropriate packages
object HACK_IR_ResolveSpecialFunctionsAndConstants extends DefaultStrategy("ResolveSpecialFunctionsAndConstants") {
  var collector = new StackCollector
  this.register(collector)

  def calculateDeterminant(m: IR_MatrixExpression): IR_Expression = {
    if(m.rows != m.columns) {
      Logger.error("determinant for non-quadratic matrices not implemented")
      // FIXME Nullzeilen/-spalten ergänzen
    }
    if (m.rows <= 0) {
      Logger.error("MatrixExpression of size <= 0")
    } else if (m.rows == 1) {
      return m.expressions(0)(0)
    } else if (m.rows == 2) {
      return m.expressions(0)(0) * m.expressions(1)(1) - m.expressions(0)(1) * m.expressions(1)(0)
    } else if (m.rows == 3) {
      return m.expressions(0)(0) * m.expressions(1)(1) * m.expressions(2)(2) +
        m.expressions(0)(1) * m.expressions(1)(2) * m.expressions(2)(0) +
        m.expressions(0)(2) * m.expressions(1)(0) * m.expressions(2)(1) -
        m.expressions(2)(0) * m.expressions(1)(1) * m.expressions(0)(2) -
        m.expressions(2)(1) * m.expressions(1)(2) * m.expressions(0)(0) -
        m.expressions(2)(2) * m.expressions(1)(0) * m.expressions(0)(1)
    } else {
      var det: IR_Expression = 0
      for (i <- 0 until m.rows) {
        var matrixExps = ListBuffer[ListBuffer[IR_Expression]]()
        for (row <- 0 until m.rows) {
          if (row != i) {
            var matrixCol = ListBuffer[IR_Expression]()
            for (col <- 1 until m.columns) {
              matrixCol += m.expressions(row)(col)
            }
            matrixExps += matrixCol
          }
        }
        var tmp = IR_MatrixExpression(Some(m.datatype), matrixExps)
        det += m.expressions(i)(0) * calculateDeterminant(tmp) * math.pow(-1, i)
      }
      return det
    }
  }

  def calculateMatrixOfMinorsElement(m:IR_MatrixExpression, forRow:Integer, forColumn:Integer) : IR_Expression = {
    if(m.rows != m.columns) {
      Logger.error("matrix of minors for non-quadratic matrices not implemented ")
    }
    var matrixExps = ListBuffer[ListBuffer[IR_Expression]]()
    for (row <- 0 until m.rows) {
      if (row != forRow) {
        var matrixCol = ListBuffer[IR_Expression]()
        for (col <- 0 until m.columns) {
          if(col != forColumn) {
            matrixCol += m.expressions(row)(col)
          }
        }
        matrixExps += matrixCol
      }
    }
    return calculateDeterminant(IR_MatrixExpression(Some(m.datatype), matrixExps))
  }

  this += new Transformation("SearchAndReplace", {
    // functions
    // FIXME: datatypes for function accesses

    case IR_FunctionCall(IR_UserFunctionAccess("concat", _), args) => Logger.error("Concat expression is deprecated => will be deleted soon")

    // Vector functions
    case f: IR_FunctionCall if f.name == "cross" || f.name == "crossproduct" =>
      f.arguments.foreach(a => if ((f.arguments(0).isInstanceOf[IR_VectorExpression] || f.arguments(0).isInstanceOf[IR_VectorExpression])
        && a.getClass != f.arguments(0).getClass) Logger.error("Must have matching types!"))
      f.arguments.foreach(a => if (a.asInstanceOf[IR_VectorExpression].length != f.arguments(0).asInstanceOf[IR_VectorExpression].length) Logger.error("Vectors must have matching lengths"))
      if (f.arguments.length + 1 != f.arguments(0).asInstanceOf[IR_VectorExpression].length) Logger.error("Must have matching number of vector arguments!")
      // For now: Restrict to 3 dimensions
      if (f.arguments.length != 2) Logger.error("Cross product only defined for 2D vectors!")

      val x = f.arguments(0).asInstanceOf[IR_VectorExpression]
      val y = f.arguments(1).asInstanceOf[IR_VectorExpression]
      if (!x.isConstant || !y.isConstant) {
        f // do nothing for vectors containing variable expressions
      } else {
        val r = ListBuffer[IR_Expression](x(1) * y(2) - x(2) * y(1), x(2) * y(0) - x(0) * y(2), x(0) * y(1) - x(1) * y(0))
        IR_VectorExpression(x.innerDatatype, r, x.rowVector)
      }
    // Matrix functions
    case x: IR_FunctionCall if x.name == "det" && x.arguments.size == 1 =>
      x.arguments(0) match {
        case m: IR_MatrixExpression => calculateDeterminant(m)
        case _ => x
      }
    case x: IR_FunctionCall if x.name == "inverse" =>
      if (x.arguments.size == 1) {
        if (x.arguments(0).isInstanceOf[IR_MatrixExpression]) {
          val m = x.arguments(0).asInstanceOf[IR_MatrixExpression]
          if (m.rows == 2 && m.columns == 2) {
            val a = m.expressions(0)(0)
            val b = m.expressions(0)(1)
            val c = m.expressions(1)(0)
            val d = m.expressions(1)(1)
            val det : IR_Expression = 1.0 / (a * d - b * c)
            IR_MatrixExpression(m.innerDatatype, ListBuffer(ListBuffer(det * d, det * b * (-1)), ListBuffer(det * c * (-1), det * a)))
          } else if (m.rows == 3 && m.columns == 3) {
            val a = m.expressions(0)(0)
            val b = m.expressions(0)(1)
            val c = m.expressions(0)(2)
            val d = m.expressions(1)(0)
            val e = m.expressions(1)(1)
            val f = m.expressions(1)(2)
            val g = m.expressions(2)(0)
            val h = m.expressions(2)(1)
            val i = m.expressions(2)(2)
            val A = e * i - f * h
            val B = -1 * (d * i - f * g)
            val C = d * h - e * g
            val D = -1 * (b * i - c * h)
            val E = a * i - c * g
            val F = -1 * (a * h - b * g)
            val G = b * f - c * e
            val H = -1 * (a * f - c * d)
            val I = a * e - b * d
            val det = a * A + b * B + c * C
            IR_MatrixExpression(m.innerDatatype, ListBuffer(ListBuffer(A / det, D / det, G / det), ListBuffer(B / det, E / det, H / det), ListBuffer(C / det, F / det, I / det)))
          } else if (m.rows == m.columns) {
            val inv_det = 1.0 / calculateDeterminant(m)
            var matrixExps = ListBuffer[ListBuffer[IR_Expression]]()
            for(row <- 0 until m.rows) {
              var matrixCol = ListBuffer[IR_Expression]()
              for(col <- 0 until m.columns) {
                matrixCol += IR_RealConstant(0)
              }
              matrixExps += matrixCol
            }

            for (row <- 0 until m.rows) {
              for (col <- 0 until m.columns) {
                matrixExps(col)(row) = calculateMatrixOfMinorsElement(m, row, col) * math.pow(-1, row + col) * inv_det
              }
            }
            IR_MatrixExpression(Some(m.datatype), matrixExps)
          } else {
            x
          }
        } else {
          x
        }
      } else {
        x
      }

    // FIXME: HACK to realize application functionality
    case func: IR_Function if "Application" == func.name =>
      func.returntype = IR_IntegerDatatype
      func.name = "main"
      if (!func.parameters.isEmpty)
        Logger.warning("function Application is not allowed to have parameters, omitting them")
      func.parameters = ListBuffer(IR_FunctionArgument("argc", IR_IntegerDatatype), IR_FunctionArgument("argv", IR_SpecialDatatype("char**")))
      func.allowFortranInterface = false
      func.allowInlining = false
      //if (true) {
      //func.body.append(new ConditionStatement(new MPI_IsRootProc,
      //  """#ifdef TRACK_CALLS
      //CallTracker::PrintCallStack();
      //#endif"""))
      //func.body.append("""#ifdef TRACK_CALLS
      //CallTracker::ClearCallStack();
      //#endif""")
      //}
      if (Knowledge.cuda_enabled) {
        func.body.prepend(CUDA_Init())
        func.body.append(CUDA_Finalize())
      }
      if (Knowledge.mpi_enabled) {
        func.body.prepend(MPI_Init)
        func.body.append(MPI_Finalize)
      }
      func.body.append(IR_Return(0))
      func

    // FIXME: IR_UserFunctionAccess's

    case IR_FunctionCall(IR_UserFunctionAccess("isOnBoundaryOf", _), args) =>
      IR_IsOnBoundary(
        args(0).asInstanceOf[IR_FieldAccess].fieldSelection,
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid)
      )

    case IR_FunctionCall(IR_UserFunctionAccess("isOnEastBoundaryOf", _), args) =>
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, DefaultNeighbors.getNeigh(Array(1, 0, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnWestBoundaryOf", _), args) =>
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, DefaultNeighbors.getNeigh(Array(-1, 0, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnNorthBoundaryOf", _), args) =>
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, DefaultNeighbors.getNeigh(Array(0, 1, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnSouthBoundaryOf", _), args) =>
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, DefaultNeighbors.getNeigh(Array(0, -1, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnTopBoundaryOf", _), args) =>
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, DefaultNeighbors.getNeigh(Array(0, 0, 1)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

    case IR_FunctionCall(IR_UserFunctionAccess("isOnBottomBoundaryOf", _), args) =>
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, DefaultNeighbors.getNeigh(Array(0, 0, -1)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

    case IR_ElementwiseAddition(left, right) => IR_FunctionCall("elementwiseAdd", ListBuffer(left, right))
    case IR_ElementwiseSubtraction(left, right) => IR_FunctionCall("elementwiseSub", ListBuffer(left, right))
    case IR_ElementwiseMultiplication(left, right) => IR_FunctionCall("elementwiseMul", ListBuffer(left, right))
    case IR_ElementwiseDivision(left, right) => IR_FunctionCall("elementwiseDiv", ListBuffer(left, right))
    case IR_ElementwiseModulo(left, right) => IR_FunctionCall("elementwiseMod", ListBuffer(left, right))
    // FIXME: IR_UserFunctionAccess
    case IR_FunctionCall(IR_UserFunctionAccess("dot", _), args) => IR_FunctionCall("dotProduct", args)
  })
}
