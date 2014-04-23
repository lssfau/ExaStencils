package exastencils.multiGrid

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.multiGrid._
import exastencils.strategies._
import exastencils.primitives._
import exastencils.mpi._

object SetupMultiGrid extends Strategy("Setting up multi-grid") {
  this += new Transformation("Adding basic functions to multi-grid", {
    case mg : MultiGrid =>
      /* DISABLED MG FUNCTION      
      mg.functions_HACK += new PerformRestriction;
      mg.functions_HACK += new PerformProlongation;
      */
      Some(mg);
  });

  val fieldCollection = StateManager.findFirst[FieldCollection]().get;
  this += new Transformation("Adding specialized functions to multi-grid", {
    case mg : MultiGrid =>
      /* DISABLED MG FUNCTION      
      for (level <- 0 to Knowledge.maxLevel) {
        // FIXME: choice by enum
        mg.functions_HACK += new PerformSmoothing(
          fieldCollection.getFieldByIdentifier("Solution", level).get,
          fieldCollection.getFieldByIdentifier("RHS", level).get,
          level);
      }
      for (level <- 0 to Knowledge.maxLevel) {
        mg.functions_HACK += new UpdateResidual(
          fieldCollection.getFieldByIdentifier("Residual", level).get,
          fieldCollection.getFieldByIdentifier("Solution", level).get,
          fieldCollection.getFieldByIdentifier("RHS", level).get,
          level);
      }
      for (level <- 0 to Knowledge.maxLevel) {
        mg.functions_HACK += new PerformVCycle(level);
      }
      for (level <- 0 to Knowledge.maxLevel) {
        mg.functions_HACK += new SetSolZero(
          fieldCollection.getFieldByIdentifier("Solution", level).get,
          level);
      }
      mg.functions_HACK += new GetGlobalResidual(
        fieldCollection.getFieldByIdentifier("Residual", Knowledge.maxLevel).get);
      */

      Some(mg);
  });
}

object ResolveSpecialFunctions extends Strategy("ResolveSpecialFunctions") {
  this += new Transformation("SearchAndReplace", {
    case FunctionCallExpression(StringConstant("diag"), args) =>
      StateManager.findFirst[StencilCollection]().get.getStencilByIdentifier(
        args(0).asInstanceOf[UnresolvedStencilAccess].stencilIdentifier,
        args(0).asInstanceOf[UnresolvedStencilAccess].level).get.entries(0).weight

    // HACK to realize intergrid operations
    case FunctionCallExpression(StringConstant("ToCoarser"), args) =>
      var stencilConvolution = Duplicate(args(0).asInstanceOf[StencilConvolution])
      stencilConvolution.targetIdx = new MultiIndex(DimArray().map(i => (2 * (dimToString(i) : Expression)) : Expression))
      stencilConvolution
    case FunctionCallExpression(StringConstant("ToFiner"), args) =>
      var stencilConvolution = Duplicate(args(0).asInstanceOf[StencilConvolution])
      stencilConvolution.targetIdx = new MultiIndex(DimArray().map(i => ((dimToString(i) : Expression) / 2) : Expression))
      stencilConvolution

    // HACK to realize print function -> FIXME
    case ExpressionStatement(FunctionCallExpression(StringConstant("print"), args)) =>
      new Scope(ListBuffer[Statement](
        new MPI_SetRankAndSize,
        new ConditionStatement(new MPI_IsRootProc,
          ("std::cout << " : Expression) ~ args.reduceLeft((l, e) => l ~ "<< \" \" <<" ~ e) ~ "<< std::endl")))

    // HACK to realize return functionality -> FIXME: move to specialized node
    case ExpressionStatement(FunctionCallExpression(StringConstant("return"), args)) =>
      args.size match {
        case 0 => "return" : Statement
        case 1 => ("return " ~ args(0)) : Statement
        case _ => "ERROR - unsupported return function statement" : Statement
      }
  })
}