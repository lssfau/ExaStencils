package exastencils.multiGrid

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.util._
import exastencils.multiGrid._
import exastencils.strategies._
import exastencils.mpi._

object ResolveSpecialFunctions extends DefaultStrategy("ResolveSpecialFunctions") {
  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(StringConstant("diag"), args) =>
      args(0) match {
        case access : StencilAccess =>
          access.stencil.entries(0).weight
        case access : StencilFieldAccess => {
          var index = Duplicate(access.index)
          index(Knowledge.dimensionality) = 0 // FIXME: this assumes the center entry to be in pos 0
          new FieldAccess(FieldSelection(access.stencilFieldSelection.field, access.stencilFieldSelection.slot, 0, access.stencilFieldSelection.fragIdx), index)
        }
      }

    // HACK to realize intergrid operations
    case FunctionCallExpression(StringConstant("ToCoarser"), args) =>
      var stencilConvolution = Duplicate(args(0).asInstanceOf[StencilConvolution])
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilConvolution.fieldAccess.index(i) = 2 * stencilConvolution.fieldAccess.index(i)
      stencilConvolution
    case FunctionCallExpression(StringConstant("ToFiner"), args) =>
      var stencilConvolution = Duplicate(args(0).asInstanceOf[StencilConvolution])
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilConvolution.fieldAccess.index(i) = stencilConvolution.fieldAccess.index(i) / 2
      stencilConvolution

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
      new StatementBlock(ListBuffer[Statement](
        "StopWatch " ~ args(0),
        args(0) ~ ".reset()"))
    case ExpressionStatement(FunctionCallExpression(StringConstant("stopTimer"), args)) =>
      new Scope(ListBuffer[Statement](
        "double timeTaken = " ~ args(0) ~ ".getTimeInMilliSec()",
        if (Knowledge.useMPI)
          new StatementBlock(ListBuffer[Statement](
          new MPI_Allreduce("&timeTaken", new RealDatatype, 1, BinaryOperators.Addition),
          "timeTaken /= mpiSize"))
        else
          new NullStatement,
        args(1) ~ " += timeTaken"))

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
      if (Knowledge.useMPI) {
        func.body.prepend(new MPI_Init)
        func.body.append(new MPI_Finalize)
      }
      func.body.append("return 0")
      func
    }
  })
}
