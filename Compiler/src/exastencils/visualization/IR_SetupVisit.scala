package exastencils.visualization

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldCollection
import exastencils.hack.ir.HACK_IR_Native

/// IR_SetupVisit

object IR_SetupVisit extends DefaultStrategy("Setup Visit functions") {
  def setupFct_SimGetVariable() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += HACK_IR_Native("sim_data* s = (sim_data*) cbdata;")
    fctBody += HACK_IR_Native("visit_handle h = VISIT_INVALID_HANDLE;")

    // n-way loop IR_LoopOverDimensions
    // IR_Assignment
    // IR_FieldAccess

    for (field <- IR_FieldCollection.objects) {
      val numDims = field.layout.numDimsGrid
      val numPointsTmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).sum

      fctBody += IR_IfCondition(
        HACK_IR_Native("(strcmp(name, \"" + field.name + "\") == 0 && " + field.level + " == s->curLevel)"),
        HACK_IR_Native(
          s"""		double tmp[$numPointsTmp];
             |		for(int i2 = 0; i2 < 128; i2++) {
             |			for(int i1 = 0; i1 < 128; i1++) {
             |				for(int i0 = 0; i0 < 128; i0++) {
             |					tmp[(n*n*i2) + (n*i1) + i0] = fieldData_Solution[s->curLevel-2][(130*134*i2) + (134*i1) + i0 + (np2*np2 + np2 + 1)]; //take the interior nodes to get the correct visualization
             |				}
             |			}
             |		}
             |		if (VisIt_VariableData_alloc(&h) == VISIT_OKAY) {
             |			VisIt_VariableData_setDataD(h, VISIT_OWNER_SIM, 1, n*n*n, tmp);
             |		}
             |""".stripMargin)
      )
    }

    IR_PlainFunction(
      "SimGetVariable",
      IR_SpecialDatatype("visit_handle"),
      ListBuffer(IR_FunctionArgument("domain", IR_IntegerDatatype)), // TODO: others (int domain, const char* name, void* cbdata)
      fctBody)
  }

  this += Transformation("..", {
    case fctCollection : IR_UserFunctions => // TODO: replace with VisFunctions
      fctCollection += setupFct_SimGetVariable()

      //Settings.pathsInc = (Settings.pathsInc :+ "$VISIT_PATH/include").distinct
      //fctCollection.externalDependencies += "visit.h"

      // TODO: other functions

      fctCollection
  })
}
