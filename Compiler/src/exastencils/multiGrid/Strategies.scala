package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.mpi._
import exastencils.util._

object ResolveSpecialFunctions extends DefaultStrategy("ResolveSpecialFunctions") {
  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(StringConstant("diag"), args) => args(0) match {
      case access : StencilAccess =>
        access.stencil.entries.find(e => {
          var ret = true
          for (dim <- 0 until Knowledge.dimensionality)
            ret &= (IntegerConstant(0) == e.offset(dim))
          ret
        }).get.weight
      case access : StencilFieldAccess => {
        var index = Duplicate(access.index)
        index(Knowledge.dimensionality) = 0 // FIXME: this assumes the center entry to be in pos 0
        new FieldAccess(FieldSelection(access.stencilFieldSelection.field, access.stencilFieldSelection.slot, 0, access.stencilFieldSelection.fragIdx), index)
      }
      case _ => {
        println("WARN: diag with unknown arg")
        FunctionCallExpression(StringConstant("diag"), args)
      }
    }

    // HACK to realize intergrid operations
    case FunctionCallExpression(StringConstant("ToCoarser"), args) => args(0) match {
      case conv : StencilConvolution => {
        var stencilConvolution = Duplicate(conv)
        for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
          stencilConvolution.fieldAccess.index(i) = 2 * stencilConvolution.fieldAccess.index(i)
        stencilConvolution
      }
      case access : FieldAccess => {
        var fieldAccess = Duplicate(access)
        for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
          fieldAccess.index(i) = 2 * fieldAccess.index(i)
        fieldAccess
      }
      case access : StencilFieldAccess => {
        var stencilFieldAccess = Duplicate(access)
        for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
          stencilFieldAccess.index(i) = 2 * stencilFieldAccess.index(i)
        stencilFieldAccess
      }
    }
    case FunctionCallExpression(StringConstant("ToFiner"), args) => args(0) match {
      case conv : StencilConvolution => {
        var stencilConvolution = Duplicate(conv)
        for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
          stencilConvolution.fieldAccess.index(i) = stencilConvolution.fieldAccess.index(i) / 2
        stencilConvolution
      }
      case access : FieldAccess => {
        var fieldAccess = Duplicate(access)
        for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
          fieldAccess.index(i) = fieldAccess.index(i) / 2
        fieldAccess
      }
      case access : StencilFieldAccess => {
        var stencilFieldAccess = Duplicate(access)
        for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
          stencilFieldAccess.index(i) = stencilFieldAccess.index(i) / 2
        stencilFieldAccess
      }
    }

    // HACK to realize return functionality -> FIXME: move to specialized node
    case ExpressionStatement(FunctionCallExpression(StringConstant("return"), args)) =>
      args.size match {
        case 0 => "return" : Statement
        case 1 => ("return " ~ args(0)) : Statement
        case _ => "ERROR - unsupported return function statement" : Statement
      }

    // FIXME: UGLY HACK to realize native code functionality
    case FunctionCallExpression(StringConstant("native"), args) =>
      args(0).asInstanceOf[StringConstant]

    // HACK to realize time measurement functionality -> FIXME: move to specialized node
    case ExpressionStatement(FunctionCallExpression(StringConstant("startTimer"), args)) =>
      if (Knowledge.testNewTimers)
        ExpressionStatement(iv.Timer(args(0)) ~ ".Start()")
      else
        ListBuffer[Statement](
          "StopWatch " ~ args(0),
          args(0) ~ ".reset()")

    case ExpressionStatement(FunctionCallExpression(StringConstant("stopTimer"), args)) =>
      if (Knowledge.testNewTimers)
        ExpressionStatement(iv.Timer(args(0)) ~ ".Stop()")
      else
        new Scope(ListBuffer[Statement](
          "double timeTaken = " ~ args(0) ~ ".getTimeInMilliSec()",
          (if (Knowledge.useMPI) new MPI_Allreduce("&timeTaken", new RealDatatype, 1, BinaryOperators.Addition) else NullStatement),
          (if (Knowledge.useMPI) "timeTaken /= mpiSize" else NullStatement),
          args(1) ~ " += timeTaken"))

    case FunctionCallExpression(StringConstant("addFromTimer"), args) =>
      if (Knowledge.testNewTimers)
        args(1) ~ " += " ~ iv.Timer(args(0)) ~ ".getTotalTimeInMilliSec()"
      else
        StringConstant("Not supported: addFromTimer")

    case FunctionCallExpression(StringConstant("getMeanFromTimer"), args) =>
      if (Knowledge.testNewTimers)
        iv.Timer(args(0)) ~ ".getMeanTimeInMilliSec()"
      else
        StringConstant("Not supported: getMeanFromTimer")

    case FunctionCallExpression(StringConstant("getTotalFromTimer"), args) =>
      if (Knowledge.testNewTimers)
        iv.Timer(args(0)) ~ ".getTotalTimeInMilliSec()"
      else
        StringConstant("Not supported: getTotalFromTimer")

    // HACK for print functionality
    case ExpressionStatement(FunctionCallExpression(StringConstant("print"), args)) =>
      new PrintStatement(args)
    case ExpressionStatement(FunctionCallExpression(StringConstant("printField"), args)) =>
      new PrintFieldStatement(args(0), args(1).asInstanceOf[FieldAccess].fieldSelection)

    // FIXME: HACK to realize application functionality
    case func : FunctionStatement if (StringConstant("Application") == func.name) => {
      func.returntype = new IntegerDatatype
      func.name = "main"
      func.parameters = ListBuffer(VariableAccess("argc", Some("int")), VariableAccess("argv", Some("char**"))) ++ func.parameters
      //if (true) {
      //func.body.append(new ConditionStatement(new MPI_IsRootProc,
      //  """#ifdef TRACK_CALLS
      //CallTracker::PrintCallStack();
      //#endif"""))
      //func.body.append("""#ifdef TRACK_CALLS
      //CallTracker::ClearCallStack();
      //#endif""")
      //}
      if (Knowledge.useMPI) {
        func.body.prepend(new MPI_Init)
        func.body.append(new MPI_Finalize)
      }
      func.body.append("return 0")
      func
    }
  })
}
