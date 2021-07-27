package exastencils.visualization.ir.visit

import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection

object IR_SetupVisit extends DefaultStrategy("Setup Visit functions") {

  /* TODO:
    * Warning for fields with manually set innerPoints != fragLength(d) * 2^level (+1)
    * Utilize virtual fields for node/cell positions
    * too many copies -> read docs for ghost layer mechanism from VisIt
    * assumes rectangular domains -> generally blockstructured grids not supported
    * interpolate face-centered variables to cell-centered
  */

  import IR_VisItUtil._

  this += Transformation("..", {
    case fctCollection : IR_UserFunctions =>
      if (Knowledge.dimensionality > 1) {
        fctCollection += IR_VisItSimGetVariable()
      }
      fctCollection += IR_VisItSimGetMesh()
      fctCollection += IR_VisItSimGetMetaData()
      fctCollection += IR_VisItControlCommandCallback()
      fctCollection += IR_VisItInitialization()
      fctCollection += IR_VisItDestroy()
      fctCollection += IR_VisItMainloop()

      if (Knowledge.mpi_enabled) {
        fctCollection += IR_VisItProcessVisItCommand()
        fctCollection += IR_VisItSimGetDomainList()
        fctCollection += IR_VisItBroadcastIntCallback()
        fctCollection += IR_VisItBroadcastStringCallback()
        fctCollection += IR_VisItSlaveProcessCallback()
      }

      Settings.pathsInc = (Settings.pathsInc :+ "$(SIMV2DIR)/include").distinct
      Settings.pathsLib = (Settings.pathsLib :+ "$(SIMV2DIR)/lib").distinct
      Settings.additionalLibs += "simV2"
      Settings.additionalLibs += "dl"
      fctCollection.externalDependencies += "string.h"
      fctCollection.externalDependencies += "stdlib.h"
      if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) fctCollection.externalDependencies += "stdexcept"
      if (Knowledge.mpi_enabled) {
        fctCollection.externalDependencies += "stdio.h"
      }
      fctCollection.externalDependencies += "VisItControlInterface_V2.h"
      fctCollection.externalDependencies += "VisItDataInterface_V2.h"
      if (Platform.targetCompiler == "MSVC") {
        fctCollection.externalDependencies += "direct.h"
      } else {
        fctCollection.externalDependencies += "unistd.h"
      }

      fctCollection

    case globalCollection : IR_GlobalCollection =>
      globalCollection.variables += cur_level_decl
      globalCollection.variables += visit_runMode_decl
      globalCollection.variables += visit_updatePlots_decl
      if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) globalCollection.variables += visit_scaleCurvemesh

      // coordinate arrays for 2 and 3 dim. rectilinear meshes
      if (Knowledge.dimensionality > 1) {
        for (coords_decl <- coords_arrays.distinct) globalCollection.variables += coords_decl
      }
      // coordinate arrays for 2 and 3 dim. curvilinear meshes(partially consisting of 1d or 2d variables)
      if (Knowledge.dimensionality < 3) {
        for (curveCoords_decl <- curveCoords_arrays.distinct) globalCollection.variables += curveCoords_decl
      }

      globalCollection.variables += commandNames_decl

      globalCollection
  })
}
