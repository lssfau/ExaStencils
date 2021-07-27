package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir._

/// IR_VisItSimGetMetaData
// metadata handling for VisIt couplings

case class IR_VisItSimGetMetaData() extends IR_FuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction =  {
    val fctBody = ListBuffer[IR_Statement]()
    val ifBody = ListBuffer[IR_Statement]()

    val mdDecl = IR_VariableDeclaration(visitHandle, "metadata", visitInvalidHandle)

    var new_name_identifier = "" // detects new field name

    // simulation metadata(mode, time, cycle)
    val modeDecl = IR_VariableDeclaration(IR_IntegerDatatype, "mode", IR_TernaryCondition(IR_VariableAccess(runModeDecl), IR_Native("VISIT_SIMMODE_RUNNING"), IR_Native("VISIT_SIMMODE_STOPPED")))
    ifBody += modeDecl
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setMode"), IR_VariableAccess(mdDecl), IR_VariableAccess(modeDecl))
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setCycleTime"), IR_VariableAccess(mdDecl), IR_VariableAccess(simCycleDecl), IR_VariableAccess(simTimeDecl))

    if (Knowledge.dimensionality > 1) {
      for (coords_decl <- coordsArrays) {
        val mmdDecl = IR_VariableDeclaration(visitHandle, "meshMetadata_" + coords_decl.name.drop(6), visitInvalidHandle)

        ifBody += mmdDecl
        ifBody += IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mmdDecl))) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), IR_VariableAccess(mmdDecl), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + coords_decl.name.drop(6))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), IR_VariableAccess(mmdDecl), IR_Native("VISIT_MESHTYPE_RECTILINEAR")),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), IR_VariableAccess(mmdDecl), Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), IR_VariableAccess(mmdDecl), Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), IR_VariableAccess(mmdDecl), Knowledge.domain_numBlocks),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), IR_VariableAccess(mdDecl), IR_VariableAccess(mmdDecl)))
        )
      }
    }
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      // display one dimensional variables as a 2d curvilinear mesh consisting of the created coordinate array and the variable values
      // display two dimensional variables as a 3d curvilinear
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != new_name_identifier) {
          val mmdDecl = IR_VariableDeclaration(visitHandle, "meshMetadata_" + field.name, visitInvalidHandle)

          ifBody += mmdDecl
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mmdDecl))) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), IR_VariableAccess(mmdDecl), IR_StringConstant("curv" + (Knowledge.dimensionality + 1) + "d_" + field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), IR_VariableAccess(mmdDecl), IR_Native("VISIT_MESHTYPE_CURVILINEAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), IR_VariableAccess(mmdDecl), Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), IR_VariableAccess(mmdDecl), Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), IR_VariableAccess(mmdDecl), Knowledge.domain_numBlocks),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), IR_VariableAccess(mdDecl), IR_VariableAccess(mmdDecl)))
          )
          new_name_identifier = field.name
        }
      }
    }

    new_name_identifier = "" // reset

    if (Knowledge.dimensionality > 1) {
      // variable metadata
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != new_name_identifier) {
          val vmdDecl = IR_VariableDeclaration(visitHandle, "varMetadata_" + field.name, visitInvalidHandle)
          val locName = field.layout.localization.name
          val varCentering = if (locName == "Cell") IR_Native("VISIT_VARCENTERING_ZONE") else IR_Native("VISIT_VARCENTERING_NODE")
          val meshname = if (locName == "Node") "Node" else if (locName == "Cell") "Zone" else "Face_" + locName.charAt(locName.length() - 1)

          ifBody += vmdDecl
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_alloc"), IR_AddressOf(IR_VariableAccess(vmdDecl))) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setName"), IR_VariableAccess(vmdDecl), IR_StringConstant(field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setMeshName"), IR_VariableAccess(vmdDecl), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + meshname)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setType"), IR_VariableAccess(vmdDecl), IR_Native("VISIT_VARTYPE_SCALAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setCentering"), IR_VariableAccess(vmdDecl), varCentering),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addVariable"), IR_VariableAccess(mdDecl), IR_VariableAccess(vmdDecl)))
          )
          new_name_identifier = field.name
        }
      }
    }

    // command metadata
    val cmdDecl = IR_VariableDeclaration(visitHandle, "commandMetadata", visitInvalidHandle)
    ifBody += IR_ForLoop(
      IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < commandNamesDecl.datatype.resolveFlattendSize, IR_PostIncrement(IR_FieldIteratorAccess(0)),
      ListBuffer[IR_Statement](
        cmdDecl,
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_alloc"), IR_AddressOf(IR_VariableAccess(cmdDecl))) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_setName"), IR_VariableAccess(cmdDecl), IR_ArrayAccess(commandNames, IR_FieldIteratorAccess(0))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addGenericCommand"), IR_VariableAccess(mdDecl), IR_VariableAccess(cmdDecl)))))
    )

    fctBody += mdDecl
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mdDecl))) EqEq visitOkay,
      ifBody
    )

    fctBody += IR_Return(IR_VariableAccess(mdDecl))

    IR_PlainFunction(
      name,
      visitHandle,
      IR_FunctionArgument("cbdata", IR_PointerDatatype(IR_UnitDatatype)),
      fctBody
    )
  }

  override def name : String = "SimGetMetaData"
  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
