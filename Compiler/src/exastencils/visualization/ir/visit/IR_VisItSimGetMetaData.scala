package exastencils.visualization.ir.visit

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.visualization.ir.visit.IR_VisItGlobals._


/// IR_VisItSimGetMetaData
// metadata handling for VisIt couplings

case class IR_VisItSimGetMetaData() extends IR_VisItFuturePlainFunction {

  import exastencils.visualization.ir.visit.IR_VisItUtil._

  override def generateFct() : IR_PlainFunction =  {
    val fctBody = ListBuffer[IR_Statement]()
    val ifBody = ListBuffer[IR_Statement]()

    val md = IR_VariableAccess("metadata", visitHandle)

    var newNameIdentifier = "" // detects new field name

    /* simulation metadata(mode, time, cycle) */
    val modeDecl = IR_VariableDeclaration(IR_IntegerDatatype, "mode", IR_TernaryCondition(runMode, IR_Native("VISIT_SIMMODE_RUNNING"), IR_Native("VISIT_SIMMODE_STOPPED")))
    ifBody += modeDecl
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setMode"), md, IR_VariableAccess(modeDecl))
    ifBody += IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_setCycleTime"), md, simCycle, simTime)

    /* mesh metadata */
    if (Knowledge.dimensionality > 1) {
      for (coords <- coordsArrays) {
        val mmd = IR_VariableAccess("meshMetadata_" + meshname(coords), visitHandle)

        ifBody += IR_VariableDeclaration(mmd, visitInvalidHandle)
        ifBody += IR_IfCondition(
          IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(mmd)) EqEq visitOkay,
          ListBuffer[IR_Statement](
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), mmd, IR_StringConstant(meshname(coords))),
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
        if (field.name != newNameIdentifier) {
          val mmd = IR_VariableAccess("meshMetadata_" + field.name, visitHandle)

          ifBody += IR_VariableDeclaration(mmd, visitInvalidHandle)
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_alloc"), IR_AddressOf(mmd)) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setName"), mmd, IR_StringConstant(curvname(Knowledge.dimensionality, field.name))),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setMeshType"), mmd, IR_Native("VISIT_MESHTYPE_CURVILINEAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setTopologicalDimension"), mmd, Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setSpatialDimension"), mmd, Knowledge.dimensionality + 1),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_MeshMetaData_setNumDomains"), mmd, Knowledge.domain_numBlocks),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addMesh"), md, mmd))
          )
          newNameIdentifier = field.name
        }
      }
    }

    newNameIdentifier = "" // reset

    /* variable metadata */
    if (Knowledge.dimensionality > 1) {
      for (field <- IR_FieldCollection.sortedObjects) {
        if (field.name != newNameIdentifier) {
          val vmd = IR_VariableAccess("varMetadata_" + field.name, visitHandle)
          val varCentering = field.layout.localization match {
            case IR_AtNode                            => IR_VariableAccess("VISIT_VARCENTERING_NODE", IR_UnknownDatatype)
            case IR_AtCellCenter | IR_AtFaceCenter(_) => IR_VariableAccess("VISIT_VARCENTERING_ZONE", IR_UnknownDatatype) // face vars are interpolated
          }

          ifBody += IR_VariableDeclaration(vmd, visitInvalidHandle)
          ifBody += IR_IfCondition(
            IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_alloc"), IR_AddressOf(vmd)) EqEq visitOkay,
            ListBuffer[IR_Statement](
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setName"), vmd, IR_StringConstant(field.name)),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setMeshName"), vmd, IR_StringConstant(meshname(field))),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setType"), vmd, IR_Native("VISIT_VARTYPE_SCALAR")),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_VariableMetaData_setCentering"), vmd, varCentering),
              IR_FunctionCall(IR_ExternalFunctionReference("VisIt_SimulationMetaData_addVariable"), md, vmd))
          )
          newNameIdentifier = field.name
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
}
