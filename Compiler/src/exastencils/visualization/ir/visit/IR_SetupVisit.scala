package exastencils.visualization.ir.visit

import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldCollection
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.visualization.ir.visit.IR_VisItGlobals._

object IR_SetupVisit extends DefaultStrategy("Setup Visit functions") {

  /* TODO:
    * Utilize virtual fields for node/cell positions
    * too many copies -> read docs for ghost layer mechanism from VisIt
    * assumes rectangular domains -> generally blockstructured grids not supported
    * only slot = 0 is considered
    * Higher-order datatypes unsupported
  */

  import IR_VisItUtil._

  override def apply(applyAtNode : Option[Node]) : Unit = {

    // check if innerPoints were set manually in layout
    for (field <- IR_FieldCollection.objects) {
      for (d <- 0 until field.numDimsGrid) {
        val numDupOrInner = field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d)
        field.layout.localization match {
          case IR_AtNode | IR_AtFaceCenter(`d`) =>
            if (numDupOrInner != Knowledge.domain_fragmentLengthAsVec(d) * (1 << field.level) + 1)
              Logger.error("VisIt interface currently only supports fields bound to a mesh with \"fragmentLength(d) * 2^level +1\" grid nodes.")
          case IR_AtCellCenter | IR_AtFaceCenter(_) =>
            if (numDupOrInner != Knowledge.domain_fragmentLengthAsVec(d) * (1 << field.level) + 0)
              Logger.error("VisIt interface currently only supports fields bound to a mesh with \"fragmentLength(d) * 2^level +0\" cells.")
        }
      }
    }

    super.apply(applyAtNode)
  }

  this += Transformation("Add dependencies, new user functions and globals", {
    case fctCollection : IR_UserFunctions =>
      if (Knowledge.dimensionality > 1)
        fctCollection += IR_VisItSimGetVariable()
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
      globalCollection.variables += curLevelDecl
      globalCollection.variables += runModeDecl
      globalCollection.variables += updatePlotsDecl
      if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) globalCollection.variables += scaleCurvemeshDecl

      // coordinate arrays for 2 and 3 dim. rectilinear meshes
      if (Knowledge.dimensionality > 1) {
        for (coords <- coordsArrays.distinct) globalCollection.variables += IR_VariableDeclaration(coords)
      }
      // coordinate arrays for 2 and 3 dim. curvilinear meshes (partially consisting of 1d or 2d variables)
      if (Knowledge.dimensionality < 3) {
        for (curveCoords <- curveCoordsArrays.distinct) globalCollection.variables += IR_VariableDeclaration(curveCoords)
      }

      globalCollection.variables += commandNamesDecl

      globalCollection
  })
}
