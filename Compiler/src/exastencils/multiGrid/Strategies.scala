package exastencils.multiGrid

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.multiGrid._
import exastencils.strategies._
import exastencils.primitives._

object SetupMultiGrid extends Strategy("Setting up multi-grid") {
  this += new Transformation("Adding basic functions to multi-grid", {
    case mg : MultiGrid =>
      mg.functions_HACK += new PerformSmoothing;
      mg.functions_HACK += new UpdateResidual;
      mg.functions_HACK += new PerformRestriction;
      mg.functions_HACK += new PerformProlongation;
      Some(mg);
  });

  val fieldCollection = FindFirstOccurence.find[FieldCollection].get;
  this += new Transformation("Adding specialized functions to multi-grid", {
    case mg : MultiGrid =>
      for (level <- (0 to Knowledge.maxLevel)) {
        mg.functions_HACK += new PerformVCycle(fieldCollection, level);
      }
      for (level <- (0 to Knowledge.maxLevel)) {
        mg.functions_HACK += new SetSolZero(fieldCollection.getFieldByName("Solution").get, level);
      }
      mg.functions_HACK += new GetGlobalResidual(fieldCollection.getFieldByName("Residual").get);
      Some(mg);
  });
}