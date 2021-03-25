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

package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.datastructures.Transformation.OutputType
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_VF_CellCenterPerDim
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

/// IR_PrintField

case class IR_PrintField(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioInterface : IR_Expression,
    var includeGhostLayers : Boolean,
    var canonicalFileLayout : Boolean = false,
    var useBinary : Boolean = false,
    var separator : IR_Expression = IR_StringConstant(" "),
    var condition : IR_Expression = true,
    var dataset : IR_Expression = IR_NullExpression,
    var mpiioRepresentation : IR_StringConstant = IR_StringConstant("native")
) extends IR_FieldIO {

  def doWrite = true
  def onlyVals = false

  val arrayIndexRange : Range = 0 until field.gridDatatype.resolveFlattendSize
  val ioInterfaceName : String = ioInterface.asInstanceOf[IR_StringConstant].value

  // writes comma-separated files in ascii mode, raw binaries otherwise (via locking)
  def printCSV() : ListBuffer[IR_Statement] = {
    // get pos in grid depending on localization
    def getPos(dim : Int) : IR_Expression = {
      field.localization match {
        case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
        case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
        case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
        case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
      }
    }
    // print coords for CSV files (Paraview)
    val csvFormat = !useBinary && Knowledge.experimental_generateParaviewFiles
    val altSep = if (csvFormat) IR_StringConstant(",") else separator // use alternative sep if paraview file
    val printPos = ListBuffer(IR_VariableAccess("std::defaultfloat", IR_UnknownDatatype)) ++
      (0 until field.layout.numDimsGrid).view.flatMap { dim => List(getPos(dim), altSep) : List[IR_Expression] }

    // begin file access
    val fileAccess = generateFileAccess(Some(altSep), Some(printPos))
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    statements ++= fileAccess.createOrOpenFile()
    val fileHeader : ListBuffer[IR_Statement] = {
      var ret : ListBuffer[IR_Statement] = ListBuffer()
      var tmp : ListBuffer[IR_Statement] = ListBuffer()
      val openMode = if(Knowledge.mpi_enabled)
        IR_VariableAccess("std::ios::app", IR_UnknownDatatype) // file was already created by root process
      else
        IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype) // create new file

      // write header at the beginning of the file with root
      if (csvFormat) {
        val streamName = IR_FieldIO.getNewStreamName()
        def streamType = IR_SpecialDatatype("std::ofstream")
        def stream = IR_VariableAccess(streamName, streamType)
        tmp += IR_ObjectInstantiation(streamType, streamName, filename, openMode)
        tmp += IR_Print(stream, "\"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\"", IR_Print.endl)
        //tmp += IR_Print(stream, s"""\"${ (0 until field.numDimsGrid).map(d => ('x' + d).toChar.toString).mkString(",") },""" + arrayIndexRange.map(index => s"s$index").mkString(",") + "\"", IR_Print.endl)
        tmp += IR_MemberFunctionCall(stream, "close")
        if (Knowledge.mpi_enabled)
          ret += IR_IfCondition(MPI_IsRootProc(), tmp)
        else
          ret = tmp
      }
      ret
    }
    fileAccess.handleDependencies()
    statements ++= fileHeader
    statements ++= fileAccess.fileAccess(0)
    statements ++= fileAccess.closeFile()

    statements
  }

  def printXdmf() : IR_PrintXdmf = {
    if (condition != IR_BooleanConstant(true))
      Logger.error("Conditions are not applicable in combination with \"IR_PrintXdmf\" since the data extents must be determinable.")

    val binaryFpp = useBinary && ioInterfaceName == "fpp"

    if (Knowledge.parIO_vis_forceMeshlessVisualization) {
      IR_PrintXdmfMeshless(filename, field, slot, ioInterface, includeGhostLayers, dataset, binaryFpp, IR_FieldIO.getNewResolveId())
    } else if (Knowledge.grid_isUniform && Knowledge.grid_isAxisAligned) {
      IR_PrintXdmfUniform(filename, field, slot, ioInterface, includeGhostLayers, dataset, binaryFpp, canonicalFileLayout, IR_FieldIO.getNewResolveId())
    } else if (Knowledge.grid_isAxisAligned && Knowledge.grid_spacingModel != "blockstructured") {
      IR_PrintXdmfNonUniform_AA(filename, field, slot, ioInterface, includeGhostLayers, dataset, binaryFpp, canonicalFileLayout, IR_FieldIO.getNewResolveId())
    } else {
      IR_PrintXdmfNonUniform_NonAA(filename, field, slot, ioInterface, includeGhostLayers, dataset, binaryFpp, canonicalFileLayout, IR_FieldIO.getNewResolveId())
    }
  }

  // use one interface for uniform, non-uniform axis-aligned and non-uniform
  def printNetCDF() : IR_Statement = IR_NullStatement // TODO
  // IR_PrintNetCDF(filename, field, slot, ioInterface, includeGhostLayers, dataset, canonicalFileLayout, IR_FieldIO.getNewResolveId())

  override def expand() : OutputType = {
    ioInterfaceName.toLowerCase match {
      case "lock"                   => printCSV()
      case "fpp" | "mpiio" | "hdf5" => printXdmf()
      case "nc"                     => printNetCDF()
      case "sion"                   =>
        Logger.warn("Sion Files cannot directly be visualized. Defaulting to \"writeField\" implementation.")
        generateFileAccess()
      case _                        =>
        Logger.error("Ignoring call to \"printField\" with unsupported I/O interface: " + ioInterface)
    }
  }
}
