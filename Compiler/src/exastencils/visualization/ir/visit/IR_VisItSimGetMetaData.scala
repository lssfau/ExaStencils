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

    val md = IR_VariableAccess("metadata", visitHandle)

    var new_name_identifier = "" // detects new field name

    // simulation metadata(mode, time, cycle)
    val modeDecl = IR_VariableDeclaration(IR_IntegerDatatype, "mode", IR_TernaryCondition(runMode, IR_Native("VISIT_SIMMODE_RUNNING"), IR_Native("VISIT_SIMMODE_STOPPED")))
    ifBody += modeDecl
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setMode"), md, IR_VariableAccess(modeDecl))
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setCycleTime"), md, simCycle, simTime)

    if (Knowledge.dimensionality > 1) {
      for (coords_decl <- coordsArrays) {
        val mmd = IR_VariableAccess("meshMetadata_" + coords_decl.name.drop(6), visitHandle)

        ifBody += IR_VariableDeclaration(mmd, visitInvalidHandle)
        ifBody += IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(mmd)) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), mmd, IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + coords_decl.name.drop(6))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), mmd, IR_Native("VISIT_MESHTYPE_RECTILINEAR")),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), mmd, Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), mmd, Knowledge.dimensionality),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), mmd, Knowledge.domain_numBlocks),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), md, mmd))
        )
      }
    }
    if (Knowledge.dimensionality == 1 || Knowledge.dimensionality == 2) {
      // display one dimensional variables as a 2d curvilinear mesh consisting of the created coordinate array and the variable values
      // display two dimensional variables as a 3d curvilinear
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != new_name_identifier) {
          val mmd = IR_VariableAccess("meshMetadata_" + field.name, visitHandle)

          ifBody += IR_VariableDeclaration(mmd, visitInvalidHandle)
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(mmd)) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), mmd, IR_StringConstant("curv" + (Knowledge.dimensionality + 1) + "d_" + field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), mmd, IR_Native("VISIT_MESHTYPE_CURVILINEAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), mmd, Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), mmd, Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), mmd, Knowledge.domain_numBlocks),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), md, mmd))
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
          val vmd = IR_VariableAccess("varMetadata_" + field.name, visitHandle)
          val locName = field.layout.localization.name
          val varCentering = if (locName == "Cell") IR_Native("VISIT_VARCENTERING_ZONE") else IR_Native("VISIT_VARCENTERING_NODE")
          val meshname = if (locName == "Node") "Node" else if (locName == "Cell") "Zone" else "Face_" + locName.charAt(locName.length() - 1)

          ifBody += IR_VariableDeclaration(vmd, visitInvalidHandle)
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_alloc"), IR_AddressOf(vmd)) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setName"), vmd, IR_StringConstant(field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setMeshName"), vmd, IR_StringConstant("rect" + Knowledge.dimensionality + "d_" + meshname)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setType"), vmd, IR_Native("VISIT_VARTYPE_SCALAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setCentering"), vmd, varCentering),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addVariable"), md, vmd))
          )
          new_name_identifier = field.name
        }
      }
    }

    // command metadata
    val cmd = IR_VariableAccess("commandMetadata", visitHandle)
    ifBody += IR_ForLoop(
      IR_VariableDeclaration(IR_FieldIteratorAccess(0), IR_IntegerConstant(0)), IR_FieldIteratorAccess(0) < commandNamesDecl.datatype.resolveFlattendSize, IR_PostIncrement(IR_FieldIteratorAccess(0)),
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(cmd, visitInvalidHandle),
        IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_alloc"), IR_AddressOf(cmd)) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_CommandMetaData_setName"), cmd, IR_ArrayAccess(commandNames, IR_FieldIteratorAccess(0))),
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addGenericCommand"), md, cmd))))
    )

    fctBody += IR_VariableDeclaration(md, visitInvalidHandle)
    fctBody += IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_alloc"), IR_AddressOf(md)) EqEq visitOkay,
      ifBody
    )

    fctBody += IR_Return(md)

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
