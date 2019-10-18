//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.{ IR_Native, _ }
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldCollection

object IR_SetupVisit extends DefaultStrategy("Setup Visit functions") {
  def setupFct_SimGetVariable() : IR_Function = {
    val fctBody = ListBuffer[IR_Statement]()

    fctBody += IR_Native("sim_data* s = (sim_data*) cbdata;")
    fctBody += IR_Native("visit_handle h = VISIT_INVALID_HANDLE;")

    // n-way loop IR_LoopOverDimensions
    // IR_Assignment
    // IR_FieldAccess

    for (field <- IR_FieldCollection.objects) {
      val numDims = field.layout.numDimsGrid
      val numPointsTmp = (0 until numDims).map(d => field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)).sum

      fctBody += IR_IfCondition(
        IR_Native("(strcmp(name, \"" + field.name + "\") == 0 && " + field.level + " == s->curLevel)"),
        IR_Native(
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
