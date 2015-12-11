package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.communication._
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

  // TODO: checking for being inside a valid level scope is currently required for setting up geometric information of grids with varying cell sizes
  // TODO: think about if this case (access outside of a loop) should be supported

  this += new Transformation("ModifyIndices", {
    case access : FieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.fieldSelection.level) < collector.getCurrentLevel => {
      var fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = fieldAccess.index(i) / 2
      fieldAccess
    }
    case access : FieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.fieldSelection.level) > collector.getCurrentLevel => {
      var fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = 2 * fieldAccess.index(i)
      fieldAccess
    }
    case access : StencilFieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) < collector.getCurrentLevel => {
      var stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = stencilFieldAccess.index(i) / 2
      stencilFieldAccess
    }
    case access : StencilFieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) > collector.getCurrentLevel => {
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
    case FunctionCallExpression("diag", args) => args(0) match {
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
        FunctionCallExpression("diag", args)
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
    case FunctionCallExpression("min", args) => MinimumExpression(args)
    case FunctionCallExpression("max", args) => MaximumExpression(args)

    // FIXME: UGLY HACK to realize native code functionality
    case FunctionCallExpression("native", args) =>
      args(0).asInstanceOf[StringConstant]

    case FunctionCallExpression("concat", args) =>
      ConcatenationExpression(args)

    // HACK to realize time measurement functionality -> FIXME: move to specialized node
    case ExpressionStatement(FunctionCallExpression("startTimer", args)) =>
      ExpressionStatement(FunctionCallExpression("startTimer", ListBuffer(iv.Timer(args(0)))))

    case ExpressionStatement(FunctionCallExpression("stopTimer", args)) =>
      ExpressionStatement(FunctionCallExpression("stopTimer", ListBuffer(iv.Timer(args(0)))))

    case FunctionCallExpression("getMeanFromTimer", args) =>
      FunctionCallExpression("getMeanTime", ListBuffer(iv.Timer(args(0))))

    case FunctionCallExpression("getTotalFromTimer", args) =>
      FunctionCallExpression("getTotalTime", ListBuffer(iv.Timer(args(0))))

    // HACK for print functionality
    case ExpressionStatement(FunctionCallExpression("print", args)) =>
      new PrintStatement(args)
    case ExpressionStatement(FunctionCallExpression("printField", args)) => {
      args.length match {
        case 1 => // option 1: only field -> deduce name
          new PrintFieldStatement("\"" + args(0).asInstanceOf[FieldAccess].fieldSelection.field.identifier + ".dat\"", args(0).asInstanceOf[FieldAccess].fieldSelection)
        case 2 => // option 2: filename and field
          new PrintFieldStatement(args(0), args(1).asInstanceOf[FieldAccess].fieldSelection)
        case 3 => //option 3: filename, file and condition
          new PrintFieldStatement(args(0), args(1).asInstanceOf[FieldAccess].fieldSelection, args(2))
      }
    }

    case ExpressionStatement(FunctionCallExpression("buildString", args)) =>
      new BuildStringStatement(args(0), args.slice(1, args.size))

    // FIXME: HACK to realize application functionality
    case func : FunctionStatement if ("Application" == func.name) => {
      func.returntype = IntegerDatatype
      func.name = "main"
      func.parameters = ListBuffer(VariableAccess("argc", Some("int")), VariableAccess("argv", Some("char**"))) ++ func.parameters
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
      if (Knowledge.mpi_enabled) {
        func.body.prepend(new MPI_Init)
        func.body.append(new MPI_Finalize)
      }
      func.body.append(new ReturnStatement(Some(new IntegerConstant(0))))
      func
    }

    case FunctionCallExpression("isOnBoundaryOf", args) => {
      IsOnBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection)
    }

    case FunctionCallExpression("isOnEastBoundaryOf", args) => {
      IsOnSpecBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection, Fragment.getNeigh(Array(1, 0, 0)))
    }
    case FunctionCallExpression("isOnWestBoundaryOf", args) => {
      IsOnSpecBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection, Fragment.getNeigh(Array(-1, 0, 0)))
    }
    case FunctionCallExpression("isOnNorthBoundaryOf", args) => {
      IsOnSpecBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, 1, 0)))
    }
    case FunctionCallExpression("isOnSouthBoundaryOf", args) => {
      IsOnSpecBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, -1, 0)))
    }
    case FunctionCallExpression("isOnTopBoundaryOf", args) => {
      IsOnSpecBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, 0, 1)))
    }
    case FunctionCallExpression("isOnBottomBoundaryOf", args) => {
      IsOnSpecBoundary(args(0).asInstanceOf[FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, 0, -1)))
    }
  })
}
