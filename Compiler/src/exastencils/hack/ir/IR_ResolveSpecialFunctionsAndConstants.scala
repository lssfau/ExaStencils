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

  this += new Transformation("SearchAndReplace", {
    // functions
    // FIXME: datatypes for function accesses

    case IR_FunctionCall(IR_UserFunctionAccess("concat", _), args) => Logger.error("Concat expression is deprecated => will be deleted soon")

    // Vector functions
    case f : IR_FunctionCall if f.name == "cross" || f.name == "crossproduct" => {
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
    }

    // Matrix functions
    case x : IR_FunctionCall if x.name == "inverse" => {
      if (x.arguments.size == 1) {
        if (x.arguments(0).isInstanceOf[IR_MatrixExpression]) {
          val m = x.arguments(0).asInstanceOf[IR_MatrixExpression]
          if (m.rows == 2 && m.columns == 2) {
            val a = m.expressions(0)(0)
            val b = m.expressions(0)(1)
            val c = m.expressions(1)(0)
            val d = m.expressions(1)(1)
            val det = 1.0 / (a * d - b * c)
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
          } else {
            x
          }
        } else {
          x
        }
      } else {
        x
      }
    }

    // FIXME: HACK to realize application functionality
    case func : IR_Function if "Application" == func.name => {
      func.returntype = IR_IntegerDatatype
      func.name = "main"
      func.parameters = ListBuffer(IR_FunctionArgument("argc", IR_IntegerDatatype), IR_FunctionArgument("argv", IR_SpecialDatatype("char**"))) ++ func.parameters
      func.allowFortranInterface = false
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
    }

    // FIXME: IR_UserFunctionAccess's

    case IR_FunctionCall(IR_UserFunctionAccess("isOnBoundaryOf", _), args) =>
      IR_IsOnBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection,
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))

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

    case IR_ElementwiseAddition(left, right)       => IR_FunctionCall("elementwiseAdd", ListBuffer(left, right))
    case IR_ElementwiseSubtraction(left, right)    => IR_FunctionCall("elementwiseSub", ListBuffer(left, right))
    case IR_ElementwiseMultiplication(left, right) => IR_FunctionCall("elementwiseMul", ListBuffer(left, right))
    case IR_ElementwiseDivision(left, right)       => IR_FunctionCall("elementwiseDiv", ListBuffer(left, right))
    case IR_ElementwiseModulo(left, right)         => IR_FunctionCall("elementwiseMod", ListBuffer(left, right))
    // FIXME: IR_UserFunctionAccess
    case IR_FunctionCall(IR_UserFunctionAccess("dot", _), args) => IR_FunctionCall("dotProduct", args)
  })
}
