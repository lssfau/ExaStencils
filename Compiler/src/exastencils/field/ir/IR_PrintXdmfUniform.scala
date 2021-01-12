package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_ToInt
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_VectorDatatype
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_DomainFromAABB
import exastencils.domain.ir.IR_IV_FragmentPositionBegin
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_CellWidthPerDim
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess_FPP
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.logger.Logger
import exastencils.util.ir.IR_AABB
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

/// IR_PrintXdmfUniform
// provides visualization of scalar and vector datatypes on an uniform mesh
// usable with following I/O interfaces: file-per-process, hdf5, mpiio
// supports file layouts: canonical and fragment-wise ordering

/*
 NOTE, otherwise the visualization will break:
 - when using VisIt, enable "Knowledge.parIO_generateVisItFiles" flag
 - when using ParaView, disable "Knowledge.parIO_generateVisItFiles" flag
*/

case class IR_PrintXdmfUniform(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var binaryFpp : Boolean,
    var canonicalFileLayout : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) {

  // TODO: test for serial applications

  // validate params
  if (includeGhostLayers) {
    includeGhostLayers = false
    Logger.warn("Ghost layer visualization is currently unsupported for IR_PrintXdmfUniform!")
  }
  if (numDimsGrid < 2) {
    Logger.error("IR_PrintXdmfUniform is only usable for 2D/3D cases.")
  }

  val dataBuffer = IR_DataBuffer(field, slot, includeGhostLayers, None, Some(dataset), canonicalFileLayout)
  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = ListBuffer(dataBuffer)

  val dimsDt : Int = dataBuffer.numDimsData - dataBuffer.numDimsGrid

  override def stmtsForPreparation : ListBuffer[IR_Statement] = IR_IV_FragmentInfo.init(
    dataBuffer.domainIdx,
    // in file-per-process, each rank writes its own domain piece individually -> fragOffset = 0
    calculateFragOffset = ioInterface != "fpp"
  )

  private def globalSize : IR_AABB = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb
  private def fragWidth(dim : Int) : Double = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)
  private def localFragIndex(dim : Int) : IR_Expression = (IR_LoopOverFragments.defIt / (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)

  // only root writes the xdmf file (except for "XML" format) -> fragment position for other processes must be calculated
  private def indexCurRank(dim : Int) : IR_Expression = (curRank / (0 until dim).map(dim => Knowledge.domain_rect_numBlocksAsVec(dim)).product) Mod Knowledge.domain_rect_numBlocksAsVec(dim)
  private def fragPosCurRank(dim : Int) : IR_Expression = (indexCurRank(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) + 0.5 + localFragIndex(dim)) * fragWidth(dim) + globalSize.lower(dim)
  private def fragPosBeginCurRank(dim : Int) : IR_Expression = fragPosCurRank(dim) - 0.5 * fragWidth(dim)
  private def fragIndexCurRank(dim : Int) : IR_Expression = IR_ToInt((fragPosCurRank(dim) - globalSize.lower(dim)) / fragWidth(dim))

  // assumes uniform grids always have "valid" frags
  private def fragIdCurRank(global : Boolean) : IR_Expression = IR_LoopOverFragments.defIt + (if (global) curRank * Knowledge.domain_numFragmentsPerBlock else 0)

  // set start index in global domain and select component of a higher-dim. datatype
  private def globalStartCurRank(componentIndex : Int = 0) : ListBuffer[IR_Expression] = {
    def accessComponent(startIdx : ListBuffer[IR_Expression]) = dataBuffer.datatype match {
      case mat : IR_MatrixDatatype =>
        val r = componentIndex / mat.sizeN
        val c = componentIndex % mat.sizeN
        startIdx :+ IR_IntegerConstant(r) :+ IR_IntegerConstant(c)
      case _ : IR_VectorDatatype =>
        startIdx :+ IR_IntegerConstant(componentIndex)
      case _ : IR_ScalarDatatype   =>
        startIdx
      case _                       =>
        Logger.error("Unsupported higher dimensional datatype used for \"IR_PrintXdmfUniform\".")
    }

    if (canonicalFileLayout) {
      // replace last indices (of the multidim datatype) with the component selection
      val start = dataBuffer.canonicalStartIndexGlobal(dataBuffer.numDimsGridRange.map(fragIndexCurRank))
      accessComponent(start.dropRight(dimsDt)) // e.g. [global x, global y, v0, v1]
    } else {
      // replace indices at the end (except "fragment dimension") with the component selection
      val start = dataBuffer.fragmentwiseStartIndexGlobal(fragIdCurRank(global = true))
      accessComponent(start.dropRight(dimsDt + 1)) :+ start.last // e.g. [x, y, z, v0, v1, v2, fragment]
    }
  }

  // specialization for file-per-process: fields are written fragment-after-fragment (i.e. no canonical layout)
  // -> an Xdmf "Grid" element with origin and spacing must be specified for each fragment to match the layout of the written field data
  override def writeXdmfGrid(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // write grids per fragment of a single block ("XML") or of the whole domain
    val gridName = if (fmt != "XML") {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), Knowledge.domain_numFragmentsPerBlock*curRank + IR_LoopOverFragments.defIt)
    } else {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), IR_LoopOverFragments.defIt)
    }

    // write each subdomain with origins and spacing to the "global" file
    // this kind of loop is only used for "global" files in case of uniform meshes
    // since the handling is fundamentally different from other xdmf writers
    def loopOverRanks(body : IR_Statement*) = IR_ForLoop(
      IR_VariableDeclaration(curRank, 0),
      IR_Lower(curRank, Knowledge.mpi_numThreads),
      IR_PreIncrement(curRank),
      body.to[ListBuffer])

    val printSubdomains = IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
        ListBuffer[IR_Statement](
          printXdmfElement(stream, openGrid(gridName, "Uniform") : _*)) ++
          writeXdmfGeometry(stream, global) ++
          writeXdmfTopology(stream, global) ++
          writeXdmfAttributes(stream, global) :+
          printXdmfElement(stream, closeGrid)))

    if (!binaryFpp) {
      // write grids for each fragment
      statements += printXdmfElement(stream, openGrid(IR_StringConstant("Grid"), "Collection") : _*)
      if (ioInterface != "fpp")
        statements += loopOverRanks(printSubdomains)
      else
        statements += printSubdomains
      statements += printXdmfElement(stream, closeGrid)
    } else {
      statements += printSubdomains
    }

    statements
  }

  // HACK: VisIt needs every subdomain to be referenced explicitly whereas in ParaView it is possible to reference a process's "grid collection"
  override def refPiecesXml(stream : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    if (!Knowledge.parIO_generateVisItFiles) {
      super.refPiecesXml(stream)
    } else {
      (0 until Knowledge.mpi_numThreads).flatMap(r => { // ref each process's grid
        (0 until Knowledge.domain_numFragmentsPerBlock).map(f => { // if a process has a collection of grids -> ref each
          printXdmfElement(stream,
            XInclude(href = buildFilenamePiece(noPath = true, rank = IR_IntegerConstant(r)).toPrint,
              xpath = ListBuffer(IR_StringConstant("/Xdmf/Domain/Grid/Grid["), f : IR_Expression, IR_StringConstant("]")) : _*) : _*)
        })
      }).to[ListBuffer]
    }
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    /* handling for issue Paraview/VisIt:
      - when specifying the origin/spacing for a CoRectMesh, the order of these values depends on the vis. tool
      - https://gitlab.kitware.com/paraview/paraview/-/issues/13274 and https://gitlab.kitware.com/vtk/vtk/-/issues/17886
    */
    def handleOrderCoRectMesh(seq: Seq[IR_Expression]) : ListBuffer[IR_Expression] = (if (Knowledge.parIO_generateVisItFiles) seq.reverse else seq).to[ListBuffer]

    val origin = handleOrderCoRectMesh((0 until numDimsGrid).map(dim =>
      if (fmt != "XML") {
        fragPosBeginCurRank(dim)
      } else {
        IR_IV_FragmentPositionBegin(dim) : IR_Expression
      })
    )
    statements += printXdmfElement(stream, openGeometry("Origin_" + (0 until numDimsGrid).map(d => "D" + ('x'+d).toChar.toString).mkString("")))
    statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dims = ListBuffer(numDimsGrid), name = "origin", altFmt = Some("XML")) : _*)
    statements += IR_Print(stream, indentData +: separateSequenceAndFilter(origin) :+ IR_Print.newline)
    statements += printXdmfElement(stream, closeDataItem)

    val spacing = handleOrderCoRectMesh((0 until numDimsGrid).map(d => IR_VF_CellWidthPerDim.access(level, d, IR_ExpressionIndex(0)) : IR_Expression))
    statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dims = ListBuffer(numDimsGrid), name = "spacing", altFmt = Some("XML")) : _*)
    statements += IR_Print(stream, indentData +: separateSequenceAndFilter(spacing) :+ IR_Print.newline)
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeGeometry)

    statements
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    // in this implementation, a xdmf "grid" is specified for each fragment -> calc. local number of nodes
    val nodeDims = (0 until numDimsGrid).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + 1 : IR_Expression).to[ListBuffer]

    var statements : ListBuffer[IR_Statement] = ListBuffer()
    statements += printXdmfElement(stream, openTopology(if (numDimsGrid == 3) "3DCoRectMesh" else "2DCoRectMesh", nodeDims) : _*)
    statements += printXdmfElement(stream, closeTopology)

    statements
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openAttribute(field.name, attributeType(dataBuffer.datatype), centeringType(dataBuffer.localization)))

    // handle non-scalar datatypes
    val joinDataItems = dimsDt > 0 && ioInterface != "fpp"
    val arrayIndexRange = dataBuffer.datatype.resolveFlattendSize
    val dimsHigherDimDatatype = IR_IntegerConstant(arrayIndexRange) +: dataBuffer.innerDimsLocal.dropRight(dimsDt)
    val localDimsTotal = if (ioInterface == "fpp") {
      // in fpp schemes, all components are written at once in a IR_LoopOverDimensions
      dimsHigherDimDatatype
    } else {
      // otherwise, multidim. datatypes are output as multiple "scalar" components
      dataBuffer.innerDimsLocal
    }
    val dimsComponent = if (joinDataItems) {
      localDimsTotal.dropRight(dimsDt) // select a component of the datatype
    } else {
      localDimsTotal // already correctly formatted
    }

    // introduce "Function DataItem" to combine (join) components of non-scalar datatypes
    if (joinDataItems) {
      val function = "JOIN(" + (0 until arrayIndexRange).map("$"+_).mkString(",") + ")"
      statements += printXdmfElement(stream, openDataItemFunction(dimsHigherDimDatatype, function) : _*)
    }

    val numComponents = if (joinDataItems) arrayIndexRange else 1
    (0 until numComponents).foreach(component => {
      val offsetComponent = component * dimsComponent.reduce(_ * _) * dataBuffer.datatype.resolveBaseDatatype.typicalByteSize // for non-scalar datatypes: skip to current component
      val seekp = offsetComponent + (if (fmt == "Binary") fragIdCurRank(global) * dataBuffer.typicalByteSizeFrag else 0) // field data laid out fragment-wise -> calc. offset to each fragment

      // depending on format and file layout, select the appropriate portion from the "heavy data" file for each fragment
      if ((ioInterface == "hdf5") || (ioInterface == "mpiio" && canonicalFileLayout)) {
        // global file is hdf5 or laid out canonically -> select portion via "hyperslabs"
        val startIndexGlobal = globalStartCurRank(component)
        // conditionally add "fragment dimension" to local dims (for non-canonical layouts)
        val localDimsComponent = dimsComponent ++ ListBuffer.fill(dimsDt)(IR_IntegerConstant(1)) // extract values for one component
        val count = IR_DataBuffer.handleFragmentDimension(dataBuffer, localDimsComponent, 1, orderKJI = false)
        val stride = IR_DataBuffer.handleFragmentDimension(dataBuffer, dataBuffer.stride, 1, orderKJI = false)

        statements += printXdmfElement(stream, openDataItemHyperslab(dimsComponent) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSelection(startIndexGlobal, stride, count) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSource(dataBuffer.datatype.resolveBaseDatatype, dataBuffer.globalDims, printFilename(stream, dataset)) : _*)
      } else {
        // file laid out fragment-wise -> select portion via seek pointers
        statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, dimsComponent, seekp) : _*)
        if (fmt == "XML") {
          val handler = ioHandler(constsIncluded = false, filenamePieceFpp).asInstanceOf[IR_FileAccess_FPP]
          for (bufIdx <- handler.dataBuffers.indices)
            statements ++= handler.printKernel(stream, bufIdx, Some(indentData))
        } else {
          statements += printFilename(stream, dataset)
        }
      }
      statements += printXdmfElement(stream, closeDataItem)
    })

    // close data item with join function
    if (joinDataItems)
      statements += printXdmfElement(stream, closeDataItem)

    statements += printXdmfElement(stream, closeAttribute)

    statements
  }

  // no constants to be reduced -> only write data
  override def writeDataAndSetConstFile() : ListBuffer[IR_Statement] = {
    if (fmt != "XML") {
      writeData(constsIncluded = false)
    } else {
      ListBuffer() // data already incorporated in xdmf file
    }
  }

  override def numDimsGrid : Int = field.layout.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level

  // unused for this implementation
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def dimsPositionsFrag : ListBuffer[IR_IntegerConstant] = ???
  override def numCellsPerFrag : Int = ???
  override def numCells_x : Int = ???
  override def numCells_y : Int = ???
  override def numCells_z : Int = ???
  override def someCellField : IR_Field = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
