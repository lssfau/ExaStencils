package exastencils.multiGrid

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.multiGrid._
import exastencils.strategies._
import exastencils.primitives._

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