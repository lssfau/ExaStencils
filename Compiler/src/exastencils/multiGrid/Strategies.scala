package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.communication._
import exastencils.core._
import exastencils.core.collectors.IRLevelCollector
import exastencils.core.collectors.StackCollector
import exastencils.cuda._
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
      StringLiteral(args(0).asInstanceOf[StringConstant].value)

    case FunctionCallExpression("concat", args) =>
      new ConcatenationExpression(args.map(a => if (a.isInstanceOf[StringConstant]) StringLiteral(a.asInstanceOf[StringConstant].value) else a))

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
      f.arguments.foreach(a => if ((f.arguments(0).isInstanceOf[VectorExpression] || f.arguments(0).isInstanceOf[VectorExpression])
        && a.getClass != f.arguments(0).getClass) Logger.error("Must have matching types!"))
      f.arguments.foreach(a => if (a.asInstanceOf[VectorExpression].length != f.arguments(0).asInstanceOf[VectorExpression].length) Logger.error("Vectors must have matching lengths"))
      if (f.arguments.length + 1 != f.arguments(0).asInstanceOf[VectorExpression].length) Logger.error("Must have matching number of vector arguments!")
      // For now: Restrict to 3 dimensions
      if (f.arguments.length != 2) Logger.error("Cross product only defined for 2D vectors!")

      val x = f.arguments(0).asInstanceOf[VectorExpression]
      val y = f.arguments(1).asInstanceOf[VectorExpression]
      if (!x.isConstant || !y.isConstant) {
        f // do nothing for vectors containing variable expressions
      } else {
        val r = ListBuffer[Expression](x(1) * y(2) - x(2) * y(1), x(2) * y(0) - x(0) * y(2), x(0) * y(1) - x(1) * y(0))
        VectorExpression(x.datatype, r, x.rowVector)
      }
    }

    // Matrix functions
    case x : FunctionCallExpression if x.name == "inverse" => {
      if (x.arguments.size == 1) {
        if (x.arguments(0).isInstanceOf[MatrixExpression]) {
          var m = x.arguments(0).asInstanceOf[MatrixExpression]
          if (m.rows == 2 && m.columns == 2) {
            var a = m.expressions(0)(0)
            var b = m.expressions(0)(1)
            var c = m.expressions(1)(0)
            var d = m.expressions(1)(1)
            var det = 1.0 / (a * d - b * c)
            MatrixExpression(m.datatype, ListBuffer(ListBuffer(det * d, det * b * (-1)), ListBuffer(det * c * (-1), det * a)))
          } else if (m.rows == 3 && m.columns == 3) {
            var a = m.expressions(0)(0)
            var b = m.expressions(0)(1)
            var c = m.expressions(0)(2)
            var d = m.expressions(1)(0)
            var e = m.expressions(1)(1)
            var f = m.expressions(1)(2)
            var g = m.expressions(2)(0)
            var h = m.expressions(2)(1)
            var i = m.expressions(2)(2)
            var A = e * i - f * h
            var B = -1 * (d * i - f * g)
            var C = d * h - e * g
            var D = -1 * (b * i - c * h)
            var E = a * i - c * g
            var F = -1 * (a * h - b * g)
            var G = (b * f - c * e)
            var H = -1 * (a * f - c * d)
            var I = (a * e - b * d)
            var det = a * A + b * B + c * C
            MatrixExpression(m.datatype, ListBuffer(ListBuffer(A / det, D / det, G / det), ListBuffer(B / det, E / det, H / det), ListBuffer(C / det, F / det, I / det)))
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
      if (Knowledge.experimental_cuda_enabled) {
        func.body.prepend(new CUDA_Init)
        func.body.append(new CUDA_Finalize)
      }
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

    case ElementwiseAdditionExpression(left, right)       => FunctionCallExpression("elementwiseAdd", ListBuffer(left, right))
    case ElementwiseSubtractionExpression(left, right)    => FunctionCallExpression("elementwiseSub", ListBuffer(left, right))
    case ElementwiseMultiplicationExpression(left, right) => FunctionCallExpression("elementwiseMul", ListBuffer(left, right))
    case ElementwiseDivisionExpression(left, right)       => FunctionCallExpression("elementwiseDiv", ListBuffer(left, right))
    case ElementwiseModuloExpression(left, right)         => FunctionCallExpression("elementwiseMod", ListBuffer(left, right))
    case FunctionCallExpression("dot", args)              => FunctionCallExpression("dotProduct", args)
  })
}
