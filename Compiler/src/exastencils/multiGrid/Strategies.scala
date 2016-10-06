package exastencils.multiGrid

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir._
import exastencils.config._
import exastencils.core._
import exastencils.core.collectors._
import exastencils.cuda._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.field.ir._
import exastencils.knowledge.Fragment
import exastencils.logger._
import exastencils.mpi.ir._
import exastencils.stencil.ir._
import exastencils.strategies.ReplaceStringConstantsStrategy
import exastencils.util._

object ResolveIntergridIndices extends DefaultStrategy("ResolveIntergridIndices") {
  val collector = new IRLevelCollector
  this.register(collector)

  // TODO: checking for being inside a valid level scope is currently required for setting up geometric information of grids with varying cell sizes
  // TODO: think about if this case (access outside of a loop) should be supported

  this += new Transformation("ModifyIndices", {
    case fct : IR_FunctionCall if "changeLvlAndIndices" == fct.name => {
      // extract information from special function call
      val fieldAccess = fct.arguments(0).asInstanceOf[IR_FieldAccess]
      Logger.warn("Performing index adaptation for " + fieldAccess.fieldSelection.field.codeName)
      val newLevel = fct.arguments(1)

      // adapt per dimension / (n+1)d is reserved
      for (dim <- 0 until Knowledge.dimensionality) {
        val idxAdaption = fct.arguments(2 + dim)

        // insert old index into index adaptation function
        ReplaceStringConstantsStrategy.toReplace = "i"
        ReplaceStringConstantsStrategy.replacement = Duplicate(fieldAccess.index(dim))
        var newIdx = Duplicate(idxAdaption)
        ReplaceStringConstantsStrategy.applyStandalone(newIdx)

        // overwrite old index
        fieldAccess.index(dim) = newIdx
      }

      fieldAccess
    }

    case access : IR_FieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.fieldSelection.level) < collector.getCurrentLevel        => {
      var fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = fieldAccess.index(i) / 2
      fieldAccess
    }
    case access : IR_FieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.fieldSelection.level) > collector.getCurrentLevel        => {
      var fieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        fieldAccess.index(i) = 2 * fieldAccess.index(i)
      fieldAccess
    }
    case access : IR_StencilFieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) < collector.getCurrentLevel => {
      var stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = stencilFieldAccess.index(i) / 2
      stencilFieldAccess
    }
    case access : IR_StencilFieldAccess if collector.inLevelScope &&
      SimplifyExpression.evalIntegral(access.stencilFieldSelection.level) > collector.getCurrentLevel => {
      var stencilFieldAccess = Duplicate(access)
      for (i <- 0 until Knowledge.dimensionality) // (n+1)d is reserved
        stencilFieldAccess.index(i) = 2 * stencilFieldAccess.index(i)
      stencilFieldAccess
    }
  }, false /* don't do this recursively -> avoid double adaptation for cases using special functions */)
}

object ResolveDiagFunction extends DefaultStrategy("ResolveDiagFunction") {
  var collector = new StackCollector
  this.register(collector)

  this += new Transformation("SearchAndReplace", {
    // FIXME: IR_UserFunctionAccess
    case IR_FunctionCall(IR_UserFunctionAccess("diag", _), args) => args(0) match {
      case access : IR_StencilAccess      =>
        val centralOffset = IR_ExpressionIndex(Array.fill(Knowledge.dimensionality)(0))
        access.stencil.findStencilEntry(centralOffset).get.coefficient
      case access : IR_StencilFieldAccess => {
        var index = Duplicate(access.index)
        index(Knowledge.dimensionality) = 0 // FIXME: this assumes the center entry to be in pos 0
        new IR_FieldAccess(IR_FieldSelection(access.stencilFieldSelection.field, access.stencilFieldSelection.level, access.stencilFieldSelection.slot, Some(0), access.stencilFieldSelection.fragIdx), index)
      }
      case _                              => {
        Logger.warn("diag with unknown arg " + args(0))
        IR_FunctionCall("diag", args)
      }
    }
  })
}

object ResolveSpecialFunctionsAndConstants extends DefaultStrategy("ResolveSpecialFunctionsAndConstants") {
  var collector = new StackCollector
  this.register(collector)

  this += new Transformation("SearchAndReplace", {
    // functions
    // FIXME: datatypes for function accesses

    case IR_FunctionCall(IR_UserFunctionAccess("concat", _), args) => Logger.error("Concat expression is deprecated => will be deleted soon")

    // Vector functions
    case f : IR_FunctionCall if (f.name == "cross" || f.name == "crossproduct") => {
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
          var m = x.arguments(0).asInstanceOf[IR_MatrixExpression]
          if (m.rows == 2 && m.columns == 2) {
            var a = m.expressions(0)(0)
            var b = m.expressions(0)(1)
            var c = m.expressions(1)(0)
            var d = m.expressions(1)(1)
            var det = 1.0 / (a * d - b * c)
            IR_MatrixExpression(m.innerDatatype, ListBuffer(ListBuffer(det * d, det * b * (-1)), ListBuffer(det * c * (-1), det * a)))
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
    case func : IR_Function if ("Application" == func.name) => {
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
        func.body.prepend(new CUDA_Init)
        func.body.append(new CUDA_Finalize)
      }
      if (Knowledge.mpi_enabled) {
        func.body.prepend(MPI_Init)
        func.body.append(MPI_Finalize)
      }
      func.body.append(new IR_Return(Some(new IR_IntegerConstant(0))))
      func
    }

    // FIXME: IR_UserFunctionAccess's

    case IR_FunctionCall(IR_UserFunctionAccess("isOnBoundaryOf", _), args) => {
      IR_IsOnBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection,
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }

    case IR_FunctionCall(IR_UserFunctionAccess("isOnEastBoundaryOf", _), args)   => {
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, Fragment.getNeigh(Array(1, 0, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }
    case IR_FunctionCall(IR_UserFunctionAccess("isOnWestBoundaryOf", _), args)   => {
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, Fragment.getNeigh(Array(-1, 0, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }
    case IR_FunctionCall(IR_UserFunctionAccess("isOnNorthBoundaryOf", _), args)  => {
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, 1, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }
    case IR_FunctionCall(IR_UserFunctionAccess("isOnSouthBoundaryOf", _), args)  => {
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, -1, 0)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }
    case IR_FunctionCall(IR_UserFunctionAccess("isOnTopBoundaryOf", _), args)    => {
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, 0, 1)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }
    case IR_FunctionCall(IR_UserFunctionAccess("isOnBottomBoundaryOf", _), args) => {
      IR_IsOnSpecBoundary(args(0).asInstanceOf[IR_FieldAccess].fieldSelection, Fragment.getNeigh(Array(0, 0, -1)),
        IR_LoopOverDimensions.defIt(args(0).asInstanceOf[IR_FieldAccess].fieldSelection.field.fieldLayout.numDimsGrid))
    }

    case IR_ElementwiseAdditionExpression(left, right)       => IR_FunctionCall("elementwiseAdd", ListBuffer(left, right))
    case IR_ElementwiseSubtractionExpression(left, right)    => IR_FunctionCall("elementwiseSub", ListBuffer(left, right))
    case IR_ElementwiseMultiplicationExpression(left, right) => IR_FunctionCall("elementwiseMul", ListBuffer(left, right))
    case IR_ElementwiseDivisionExpression(left, right)       => IR_FunctionCall("elementwiseDiv", ListBuffer(left, right))
    case IR_ElementwiseModuloExpression(left, right)         => IR_FunctionCall("elementwiseMod", ListBuffer(left, right))
    // FIXME: IR_UserFunctionAccess
    case IR_FunctionCall(IR_UserFunctionAccess("dot", _), args) => IR_FunctionCall("dotProduct", args)
  })
}
