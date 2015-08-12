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

    // Vector functions
    case f : FunctionCallExpression if (f.name == "cross" || f.name == "crossproduct") => {
      f.arguments.foreach(a => if ((f.arguments(0).isInstanceOf[RowVectorExpression] || f.arguments(0).isInstanceOf[ColumnVectorExpression])
        && a.getClass != f.arguments(0).getClass) Logger.error("Must have matching vector types!"))
      f.arguments.foreach(a => if (a.asInstanceOf[VectorExpression].length != f.arguments(0).asInstanceOf[VectorExpression].length) Logger.error("Vectors must have matching lengths"))
      if (f.arguments.length + 1 != f.arguments(0).asInstanceOf[VectorExpression].length) Logger.error("Must have matching number of vector arguments!")
      // For now: Restrict to 3 dimensions
      if (f.arguments.length != 2) Logger.error("Cross product only defined for 3D vectors!")

      val x = f.arguments(0).asInstanceOf[VectorExpression]
      val y = f.arguments(1).asInstanceOf[VectorExpression]
      if (!x.isConstant || !y.isConstant) {
        f // do nothing for vectors containing variable expressions
      } else {
        val r = ListBuffer[Expression](x(1) * y(2) - x(2) * y(1), x(2) * y(0) - x(0) * y(2), x(0) * y(1) - x(1) * y(0))
        f.arguments(0) match {
          case x : RowVectorExpression    => RowVectorExpression(r)
          case x : ColumnVectorExpression => ColumnVectorExpression(r)
        }
      }
    }

    // HACK for print functionality
    case ExpressionStatement(FunctionCallExpression("print", args)) =>
      new PrintStatement(args)
    case ExpressionStatement(FunctionCallExpression("printField", args)) =>
      new PrintFieldStatement(args(0), args(1).asInstanceOf[FieldAccess].fieldSelection)

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
  })
}
