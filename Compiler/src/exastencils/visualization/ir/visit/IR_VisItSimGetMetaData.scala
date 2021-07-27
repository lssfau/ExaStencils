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

    val md_decl = IR_VariableDeclaration(visitHandle, "metadata", visitInvalidHandle)

    var new_name_identifier = "" // detects new field name

    // simulation metadata(mode, time, cycle)
    val mode_decl = IR_VariableDeclaration(IR_IntegerDatatype, "mode", IR_TernaryCondition(IR_VariableAccess(visit_runMode_decl), IR_Native("VISIT_SIMMODE_RUNNING"), IR_Native("VISIT_SIMMODE_STOPPED")))
    ifBody += mode_decl
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setMode"), IR_VariableAccess(md_decl), IR_VariableAccess(mode_decl))
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setCycleTime"), IR_VariableAccess(md_decl), IR_VariableAccess(sim_cycle_decl), IR_VariableAccess(sim_time_decl))

    if (Knowledge.dimensionality > 1) {
      for (coords_decl <- coords_arrays) {
        val mmd_decl = IR_VariableDeclaration(visitHandle, "meshMetadata_" + coords_decl.name.drop(6), visitInvalidHandle)

        ifBody += mmd_decl
        ifBody += IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mmd_decl))) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), IR_VariableAccess(mmd_decl), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + coords_decl.name.drop(6))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), IR_VariableAccess(mmd_decl), IR_Native("VISIT_MESHTYPE_RECTILINEAR")),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), IR_VariableAccess(mmd_decl), Knowledge.domain_numBlocks),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), IR_VariableAccess(md_decl), IR_VariableAccess(mmd_decl))
          )
        )
      }
    }
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      // display one dimensional variables as a 2d curvilinear mesh consisting of the created coordinate array and the variable values
      // display two dimensional variables as a 3d curvilinear
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != new_name_identifier) {
          val mmd_decl = IR_VariableDeclaration(visitHandle, "meshMetadata_" + field.name, visitInvalidHandle)

          ifBody += mmd_decl
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(IR_VariableAccess(mmd_decl))) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), IR_VariableAccess(mmd_decl), IR_StringConstant("curv" + (Knowledge.dimensionality + 1) + "d_" + field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), IR_VariableAccess(mmd_decl), IR_Native("VISIT_MESHTYPE_CURVILINEAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), IR_VariableAccess(mmd_decl), Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), IR_VariableAccess(mmd_decl), Knowledge.domain_numBlocks),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), IR_VariableAccess(md_decl), IR_VariableAccess(mmd_decl))
            )
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
          val vmd_decl = IR_VariableDeclaration(visitHandle, "varMetadata_" + field.name, visitInvalidHandle)
          val loc_name = field.layout.localization.name
          val varCentering = if (loc_name == "Cell") {
            IR_Native("VISIT_VARCENTERING_ZONE")
          } else {
            IR_Native("VISIT_VARCENTERING_NODE")
          }
          val meshname = if (loc_name == "Node") "Node" else if (loc_name == "Cell") "Zone" else "Face_" + loc_name.charAt(loc_name.length() - 1)
          ifBody += vmd_decl
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_alloc"), IR_AddressOf(IR_VariableAccess(vmd_decl))) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setName"), IR_VariableAccess(vmd_decl), IR_StringConstant(field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setMeshName"), IR_VariableAccess(vmd_decl), IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + meshname)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setType"), IR_VariableAccess(vmd_decl), IR_Native("VISIT_VARTYPE_SCALAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setCentering"), IR_VariableAccess(vmd_decl), varCentering),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addVariable"), IR_VariableAccess(md_decl), IR_VariableAccess(vmd_decl))
            )
          )
          new_name_identifier = field.name
        }
      }
    }

    // command metadata
    val cmd_decl = IR_VariableDeclaration(visitHandle, "commandMetadata", visitInvalidHandle)
    ifBody += IR_ForLoop(
      IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < commandNames_decl.datatype.resolveFlattendSize, IR_PostIncrement(IR_FieldIteratorAccess(0)),
      ListBuffer[IR_Statement](
        cmd_decl,
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_alloc"), IR_AddressOf(IR_VariableAccess(cmd_decl))) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_setName"), IR_VariableAccess(cmd_decl), IR_ArrayAccess(commandNames, IR_FieldIteratorAccess(0))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addGenericCommand"), IR_VariableAccess(md_decl), IR_VariableAccess(cmd_decl))
          )
        )
      )
    )

    fctBody += md_decl
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_alloc"), IR_AddressOf(IR_VariableAccess(md_decl))) EqEq visitOkay,
      ifBody
    )

    fctBody += IR_Return(IR_VariableAccess(md_decl))

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
