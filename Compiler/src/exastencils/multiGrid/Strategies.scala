package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.IRLevelCollector
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.mpi._
import exastencils.util._

object ResolveIntergridIndices extends DefaultStrategy("ResolveIntergridIndices") {
  val collector = new IRLevelCollector
  this.register(collector)

  this += new Transformation("ModifyIndices", {
    case access : FieldAccess if SimplifyExpression.evalIntegral(access.fieldSelection.level) < collector.getCurrentLevel => {
      var fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = fieldAccess.index(i) / 2
      fieldAccess
    }
    case access : FieldAccess if SimplifyExpression.evalIntegral(access.fieldSelection.level) > collector.getCurrentLevel => {
      var fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = 2 * fieldAccess.index(i)
      fieldAccess
    }
    case access : StencilFieldAccess if SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) < collector.getCurrentLevel => {
      var stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = stencilFieldAccess.index(i) / 2
      stencilFieldAccess
    }
    case access : StencilFieldAccess if SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) > collector.getCurrentLevel => {
      var stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = 2 * stencilFieldAccess.index(i)
      stencilFieldAccess
    }
  })
}

object ResolveDiagFunction extends DefaultStrategy("ResolveDiagFunction") {
  var collector = new StackCollector
  this.register(collector)

  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(StringConstant("diag"), args) => args(0) match {
      case access : StencilAccess =>
        val centralOffset = new MultiIndex(Array.fill(Knowledge.dimensionality)(0))
        access.stencil.findStencilEntry(centralOffset).get.coefficient
      case access : StencilFieldAccess => {
        var index = Duplicate(access.index)
        index(Knowledge.dimensionality) = 0 // FIXME: this assumes the center entry to be in pos 0
        new FieldAccess(FieldSelection(access.stencilFieldSelection.field, access.stencilFieldSelection.level, access.stencilFieldSelection.slot, Some(0), access.stencilFieldSelection.fragIdx), index)
      }
      case _ => {
        Logger.warn("diag with unknown arg " + args(0))
        FunctionCallExpression(StringConstant("diag"), args)
      }
    }
  })
}

object ResolveSpecialFunctionsAndConstants extends DefaultStrategy("ResolveSpecialFunctionsAndConstants") {
  var collector = new StackCollector
  this.register(collector)

  this += new Transformation("SearchAndReplace", {
    // constants
    // currently done directly on l4 before progressing to ir

    // functions

    // HACK to implement min/max functions
    case FunctionCallExpression(StringConstant("min"), args) => MinimumExpression(args)
    case FunctionCallExpression(StringConstant("max"), args) => MaximumExpression(args)

    // FIXME: UGLY HACK to realize native code functionality
    case FunctionCallExpression(StringConstant("native"), args) =>
      args(0).asInstanceOf[StringConstant]

    // HACK to realize time measurement functionality -> FIXME: move to specialized node
    case ExpressionStatement(FunctionCallExpression(StringConstant("startTimer"), args)) =>
      if (Knowledge.l3tmp_genAdvancedTimers)
        ExpressionStatement(FunctionCallExpression("startTimer", ListBuffer(iv.Timer(args(0)))))
      else
        ListBuffer[Statement](
          "StopWatch " ~ args(0),
          "resetTimer(" ~ args(0) ~ ")")

    case ExpressionStatement(FunctionCallExpression(StringConstant("stopTimer"), args)) =>
      if (Knowledge.l3tmp_genAdvancedTimers)
        ExpressionStatement(FunctionCallExpression("stopTimer", ListBuffer(iv.Timer(args(0)))))
      else
        new Scope(ListBuffer[Statement](
          "double timeTaken = " ~ "getTimeInMS(" ~ args(0) ~ ")",
          (if (Knowledge.mpi_enabled) new MPI_Allreduce("&timeTaken", new RealDatatype, 1, "+") else NullStatement),
          (if (Knowledge.mpi_enabled) "timeTaken /= mpiSize" else NullStatement),
          args(1) ~ " += timeTaken"))

    case ExpressionStatement(FunctionCallExpression(StringConstant("addFromTimer"), args)) =>
      if (Knowledge.l3tmp_genAdvancedTimers)
        AssignmentStatement(args(1), FunctionCallExpression("getTotalTime", ListBuffer(iv.Timer(args(0)))), "+=")
      else
        ExpressionStatement(StringConstant("Not supported: addFromTimer"))

    case FunctionCallExpression(StringConstant("getMeanFromTimer"), args) =>
      if (Knowledge.l3tmp_genAdvancedTimers)
        FunctionCallExpression("getMeanTime", ListBuffer(iv.Timer(args(0))))
      else
        StringConstant("Not supported: getMeanFromTimer")

    case FunctionCallExpression(StringConstant("getTotalFromTimer"), args) =>
      if (Knowledge.l3tmp_genAdvancedTimers)
        FunctionCallExpression("getTotalTime", ListBuffer(iv.Timer(args(0))))
      else
        StringConstant("Not supported: getTotalFromTimer")

    // HACK for print functionality
    case ExpressionStatement(FunctionCallExpression(StringConstant("print"), args)) =>
      new PrintStatement(args)
    case ExpressionStatement(FunctionCallExpression(StringConstant("printField"), args)) =>
      new PrintFieldStatement(args(0), args(1).asInstanceOf[FieldAccess].fieldSelection)

    // FIXME: HACK to realize application functionality
    case func : FunctionStatement if ("Application" == func.name) => {
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
      if (Knowledge.mpi_enabled) {
        func.body.prepend(new MPI_Init)
        func.body.append(new MPI_Finalize)
      }
      func.body.append("return 0")
      func
    }
  })
}
