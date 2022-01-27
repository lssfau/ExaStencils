package exastencils.applications.swe.ir

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.grid.ir.IR_AtNode
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.xdmf.IR_PrintXdmf

/// IR_PrintXdmfSWE
// 2D only
// for a variable number of fragments per block

case class IR_PrintXdmfSWE(
    var filename : IR_Expression,
    level : Int,
    ioMethod : IR_Expression,
    binaryFpp : Boolean,
    var resolveId : Int,
    var nodalFieldCollection : ListBuffer[IR_Field],
    var discFieldCollection : ListBuffer[ListBuffer[IR_Field]]
) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationSWE with IR_PrintFieldsAsciiSWE {

  // dataset names for hdf5
  def datasetCoords : ListBuffer[IR_StringConstant] = ListBuffer(IR_StringConstant("/constants/X"), IR_StringConstant("/constants/Y"))
  def datasetConnectivity : IR_StringConstant = IR_StringConstant("/constants/Connectivity")
  def datasetFields : ListMap[String, ListBuffer[IR_StringConstant]] = {
    val datasetNodeFields = ListMap(nodalFields.keys.toSeq.map(fieldname =>
      // bath: values don't change -> can be referenced over and over. others: written in each print call
      fieldname -> ListBuffer(IR_StringConstant((if (fieldname == "bath") "/constants/" else "/fieldData/") + fieldname))) : _*)

    val datasetDiscFields = if (Knowledge.swe_nodalReductionPrint) {
      discFieldsReduced.values.map(buf => buf.name -> ListBuffer(IR_StringConstant("/fieldData/" + buf.name)))
    } else if (Knowledge.swe_interleaveDiscComponentsPrint) {
      discFieldBuffers.values.map(buf => buf.name -> ListBuffer(IR_StringConstant("/fieldData/" + buf.name)))
    } else {
      discFields.values.map { discField =>
        // for fields like order (lower0, lower0, lower0, upper0, ...): add suffix to create unique dataset names for each component
        val suffix = (0 until 6).map(i => if (discField.toSet.size != discField.size) s"_$i" else "")
        getBasenameDiscField(discField) -> discField.zipWithIndex.map { case (field, fid) => IR_StringConstant("/fieldData/" + field.name + suffix(fid)) }
      }
    }

    datasetNodeFields ++ datasetDiscFields
  }

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // setup frag info
    stmts ++= IR_IV_FragmentInfo.init(
      domainIndex,
      /*
      - In file-per-process, each rank writes its own domain piece individually -> fragOffset = 0
      - For single-shared file approaches, the offset for the global "valid" fragment needs to be known for each process -> compute frag offset
      */
      calculateFragOffset = fmt != "fpp"
    )

    // setup buffers
    if (fmt != "XML") {
      if (enforceCopiesHdf5 || gridPositionsCopied)
        stmts ++= setupNodePositions(copyNodePositions = true)
      if (!Knowledge.swe_nodalReductionPrint && Knowledge.swe_interleaveDiscComponentsPrint)
        stmts ++= setupNonReducedDiscData
      stmts ++= setupConnectivity(global = ioInterface != "fpp")
    }
    if (Knowledge.swe_nodalReductionPrint) {
      stmts ++= setupReducedData
    } else if (ioInterface == "hdf5") {
      stmts ++= setupNodalFieldsHdf5
    }

    stmts
  }

  /* NOTE: in HDF5 it is possible to implement the access pattern of nodal fields (6 node offsets for each grid cell):
      1. via the "H5Sselect_elements" function for the memory space
      2. via copies
      Despite the fact that the first option is the more elegant, the performance degrades significantly, because it results to many small and non-contiguous accesses to the file
      -> copies are used instead
  */
  val enforceCopiesHdf5 : Boolean = ioInterface == "hdf5" && !Knowledge.swe_nodalReductionPrint
  def nodalFieldBuffersHdf5 : ListMap[String, IR_IV_TemporaryBuffer] = ListMap(nodalFields.toSeq.map { nodeFieldMap =>
    nodeFieldMap._1 -> IR_IV_TemporaryBuffer(nodeFieldMap._2.resolveBaseDatatype, IR_AtNode, nodeFieldMap._1, domainIndex, dimsPositionsFrag)
  } : _*)

  def setupNodalFieldsHdf5 : ListBuffer[IR_Statement] = {
    val stmts : ListBuffer[IR_Statement] = ListBuffer()

    // buffers for nodal fields (e.g. bath)
    nodalFieldBuffersHdf5.map { case (name, tmpBuf) =>
      val indexRangeCells = IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)))

      val numAccessesPerCell = nodeOffsets.length // for non-reduced SWE "6"
      val offset = IR_LoopOverFragments.defIt * dimsPositionsFrag.reduce(_ * _) + numAccessesPerCell * indexRangeCells.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))

      stmts += tmpBuf.allocateMemory
      // bath is constant -> only written once (-> init only needed once)
      stmts += IR_IfCondition(if (name == "bath") IR_ConstantsWrittenToFile().isEmpty else IR_BooleanConstant(true),
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            IR_LoopOverDimensions(numDimsGrid, indexRangeCells,
              (0 until numAccessesPerCell).to[ListBuffer].map(idx => {
                IR_Assignment(
                  tmpBuf.at(offset + idx),
                  IR_FieldAccess(nodalFields(name), IR_IV_ActiveSlot(nodalFields(name)), IR_LoopOverDimensions.defIt(numDimsGrid) + nodeOffsets(idx))) : IR_Statement
              })))))
    }

    stmts
  }

  /* special handling for a variable number of frags */
  // specifies "fragment dimension" (i.e. how many fragments are written to a file)
  override def dimFrags(global : Boolean) : IR_Expression = if (!binaryFpp) {
    super.dimFrags(global)
  } else {
    // binary fpp: only root writes the xdmf file -> requires the number of valid frags for each rank
    IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank)
  }
  // contains expressions that calculate the seek pointer for each DataItem (used for raw binary files)
  override def seekpOffsets(global : Boolean, constsIncluded : Boolean) : ListBuffer[IR_Expression] = if (!binaryFpp) {
    super.seekpOffsets(global, constsIncluded)
  } else {
    // binary fpp: root needs to know the actual number of fragments of each rank to compute the file offsets correctly
    dataBuffers(constsIncluded).map(buf => if (global) buf.typicalByteSizeGlobal else buf.typicalByteSizeFrag * IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank))
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry("X_Y")) // nodePositions are not interleaved
    (0 until numDimsGrid).foreach(d => {
      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dimsPositionsFrag :+ dimFrags(global), getSeekp(global)) : _*)
      statements ++= (if (fmt == "XML") {
        ListBuffer(IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("IB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => nodalLoopEnd + someCellField.layout.idxById("IE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
                IR_Print(stream, nodeOffsets.flatMap(offset =>
                  ListBuffer(indentData, getPos(IR_AtNode, level, d, IR_LoopOverDimensions.defIt(numDimsGrid) + offset), IR_Print.newline)) : _*))),
            IR_Print(stream, IR_Print.flush)))
      } else {
        ListBuffer(printFilename(stream, datasetCoords(d)))
      })
      statements += printXdmfElement(stream, closeDataItem)
    })
    statements += printXdmfElement(stream, closeGeometry)

    writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = "Geometry")
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openTopology("Triangle", ListBuffer(numCellsPerFrag, dimFrags(global))) : _*)
    statements += printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimsConnectivityFrag :+ dimFrags(global), getSeekp(global)) : _*)
    statements += (if (fmt == "XML") {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            (0 until numDimsGrid).map(dim =>
              IR_Print(stream,
                indentData +:
                  connectivityForCell(global = false).slice(3 * dim, 3 * (dim + 1)).flatMap(List(_, separator)).dropRight(1) :+
                  IR_Print.newline) : IR_Statement
            ).to[ListBuffer])),
        IR_Print(stream, IR_Print.flush))
    } else {
      printFilename(stream, datasetConnectivity)
    })
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeTopology)

    writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = "Topology")
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    val nodalDims = dimsPositionsFrag :+ dimFrags(global)
    fieldnames.zipWithIndex.flatMap { case (fname, fid) =>
      var statements : ListBuffer[IR_Statement] = ListBuffer()
      statements += printXdmfElement(stream, openAttribute(name = fname, tpe = "Scalar", ctr = "Node"))
      if (Knowledge.swe_nodalReductionPrint || fields(fname).head.localization == IR_AtNode || fmt == "XML") {
        // reduced fields or nodal fields (e.g. bath)
        statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, nodalDims, getSeekp(global)) : _*)
        statements ++= (if (fmt == "XML") {
          fields(fname).length match {
            case 1 => printNodalField(fields(fname).head, Some(stream), Some(indentData))
            case 6 => printDiscField(fields(fname), Some(stream), Some(indentData))
            case _ => Logger.error("IR_PrintXdmfSWE: Unknown field type; neither nodal nor disc field.")
          }
        } else {
          ListBuffer(printFilename(stream, datasetFields(fname).head))
        })
        statements += printXdmfElement(stream, closeDataItem)
      } else {
        if (!Knowledge.swe_interleaveDiscComponentsPrint) {
          // non-reduced disc fields -> join lower and upper components from file
          val discField = discFields(fname)
          val function = "JOIN(" + discField.indices.map("$" + _).mkString(",") + ")"
          statements += printXdmfElement(stream, openDataItemFunction(nodalDims, function) : _*)
          discField.zipWithIndex.foreach { case (discField, discId) =>
            statements += printXdmfElement(stream, openDataItem(discField.resolveBaseDatatype, nodalDims.drop(1), getSeekp(global)) : _*)
            statements += printFilename(stream, datasetFields(fname)(discId))
            statements += printXdmfElement(stream, closeDataItem)
          }
          statements += printXdmfElement(stream, closeDataItem)
        } else {
          statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, nodalDims, getSeekp(global)) : _*)
          statements += printFilename(stream, datasetFields(fname).head)
          statements += printXdmfElement(stream, closeDataItem)
        }
      }
      statements += printXdmfElement(stream, closeAttribute)

      writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = s"Attribute[${ fid + 1 }]",
        altCondition = Some(IR_ConstantsWrittenToFile().isEmpty OrOr fname != "bath"))
    }
  }

  override def discFieldsToDatabuffers(discField : ListBuffer[IR_Field]) : ListBuffer[IR_DataBuffer] = {
    // source for data buffer is dependent on reduction mode: temp buffer or field
    val fieldname = getBasenameDiscField(discField)
    val dataset = datasetFields(fieldname).head
    if (Knowledge.swe_nodalReductionPrint) {
      ListBuffer(IR_DataBuffer(discFieldsReduced(fieldname), IR_IV_ActiveSlot(someCellField), None, Some(dataset)))
    } else if (Knowledge.swe_interleaveDiscComponentsPrint) {
      ListBuffer(IR_DataBuffer(discFieldBuffers(fieldname), IR_IV_ActiveSlot(someCellField), None, Some(dataset)))
    } else {
      discField.zipWithIndex.map { case (field, fid) =>
        val idxRange = 0 until field.layout.numDimsData
        val slot = IR_IV_ActiveSlot(field)
        new IR_DataBuffer(
          slot = slot,
          datatype = field.gridDatatype,
          localization = field.layout.localization,
          referenceOffset = field.referenceOffset,
          beginIndices = idxRange.map(d => field.layout.defIdxById("IB", d) : IR_Expression).to[ListBuffer],
          endIndices = idxRange.map(d => field.layout.defIdxById("IE", d) : IR_Expression).to[ListBuffer],
          totalDimsLocal = idxRange.map(d => field.layout.defTotal(d) : IR_Expression).to[ListBuffer],
          numDimsGrid = field.layout.numDimsGrid,
          numDimsData = field.layout.numDimsData,
          domainIdx = field.domain.index,
          accessPattern = IR_RegularAccessPattern((idx : IR_Index) => IR_FieldAccess(field, slot, idx.toExpressionIndex)),
          datasetName = datasetFields(getBasenameDiscField(discField))(fid),
          name = field.name,
          canonicalStorageLayout = false,
          accessBlockwise = false,
          isTemporaryBuffer = false
        )
      }
    }
  }

  override def dataBuffersConst : ListBuffer[IR_DataBuffer] = {
    // bath is constant and can be reduced -> move to constants if exists
    val bath = nodalFields.get("bath")
    var constants : ListBuffer[IR_DataBuffer] = ListBuffer()
    if (enforceCopiesHdf5 || gridPositionsCopied) {
      // special case for non-reduced SWE with hdf5 or no associated field for vf -> copy positions to buffer
      constants ++= nodePositionsBuf.zipWithIndex.map { case (tmpBuf, idx) => IR_DataBuffer(tmpBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetCoords(idx))) }
    } else {
      // use vf's associated field directly
      constants ++= nodePosVecAsDataBuffers(accessIndices, Some(datasetCoords.map(s => s : IR_Expression)))
    }
    constants += IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, Some(datasetConnectivity))
    if (bath.isDefined) {
      if (enforceCopiesHdf5) {
        // special case for non-reduced SWE with hdf5
        constants += IR_DataBuffer(nodalFieldBuffersHdf5("bath"), IR_IV_ActiveSlot(bath.get), None, Some(datasetFields("bath").head))
      } else {
        constants += IR_DataBuffer(bath.get, IR_IV_ActiveSlot(bath.get), includeGhosts = false, Some(nodalAccess(bath.get)), Some(datasetFields("bath").head), canonicalOrder = false)
      }
    }

    constants
  }

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    // non-constant fields
    val nonConstFields = fields.filterKeys(_ != "bath").flatMap { fieldMap =>
      val fieldCollection = fieldMap._2
      val name = fieldMap._1

      fieldCollection.length match {
        case 1 =>
          val nodeField = fieldCollection.head
          val buf = if (enforceCopiesHdf5) {
            IR_DataBuffer(nodalFieldBuffersHdf5(name), IR_IV_ActiveSlot(nodeField), None, Some(datasetFields(name).head))
          } else {
            IR_DataBuffer(nodeField, IR_IV_ActiveSlot(nodeField), includeGhosts = false, Some(nodalAccess(nodeField)), Some(datasetFields(name).head), canonicalOrder = false)
          }
          ListBuffer(buf)
        case 6 =>
          discFieldsToDatabuffers(fieldCollection)
        case _ =>
          Logger.error("IR_PrintXdmfSWE: Unknown field type; neither nodal nor disc field.")
      }
    }

    if (constsIncluded) dataBuffersConst ++ nonConstFields else nonConstFields.to[ListBuffer]
  }
}
