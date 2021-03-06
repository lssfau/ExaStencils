package exastencils.visualization.ir.postprocessing.xdmf

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.OutputType
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._
import exastencils.visualization.ir.postprocessing.IR_PrintVisualization

// IR_PrintXdmf: Visualization interface using the eXtensible Data Model and Format (Xdmf)
// to be used in combination with parallel I/O methods: MPI I/O, HDF5, file-per-process
// to be implemented as specific printer in exastencils.application.ir or field.ir

abstract class IR_PrintXdmf(ioMethod : IR_Expression) extends IR_Statement with IR_Expandable with IR_PrintVisualization with IR_XdmfFormat {

  // accesses
  val curRank = IR_VariableAccess("curRank", IR_IntegerDatatype)
  val filenamePieceFpp = IR_VariableAccess(IR_FileAccess.declareVariable("fnPiece"), IR_StringDatatype)

  val supportedInterfaces : ListBuffer[String] = ListBuffer("mpiio", "fpp", "hdf5")

  def ioInterface : String = ioMethod match {
    case s : IR_StringConstant if supportedInterfaces.contains(s.value) =>
      s.value
    case _                                                              =>
      Logger.error("Wrong I/O interface passed to \"printXdmf\". Options are: " + supportedInterfaces.mkString("\"", "\", \"", "\""))
  }

  def dataBuffersConst : ListBuffer[IR_DataBuffer]
  def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] // contains data buffers for node positions, connectivity and field values

  def ioHandler(constsIncluded : Boolean, fn : IR_Expression) : IR_FileAccess = {
    ioInterface match {
      case "mpiio" =>
        IR_FileAccess_MPIIO(fn, dataBuffers(constsIncluded), writeAccess = true, representation = IR_StringConstant("native"), initFragInfo = false)
      case "fpp"   =>
        IR_FileAccess_FPP(fn, dataBuffers(constsIncluded), useBinary = binaryFpp, writeAccess = true, separator, condition = true, optPrintComponents = None)
      case "hdf5"  =>
        IR_FileAccess_HDF5(fn, dataBuffers(constsIncluded), writeAccess = true, initFragInfo = false, zlibCompressionLevel = 0)
      case _       =>
        Logger.error("Wrong I/O interface passed to \"printXdmf\". Options are: " + supportedInterfaces.mkString("\"", "\", \"", "\""))
    }
  }

  // overloaded helper methods to write xdmf elements
  def printXdmfElement(stream : IR_VariableAccess, str : String*) : IR_Statement = IR_Print(stream, str.map(s => IR_StringConstant(s)).to[ListBuffer] :+ IR_Print.newline)
  def printXdmfElement(stream : IR_VariableAccess, expr : IR_Expression*)(implicit d : DummyImplicit) : IR_Statement = IR_Print(stream, expr.to[ListBuffer] :+ IR_Print.newline)

  // helpers to construct filenames
  def buildFilenamePiece(noPath : Boolean, rank : IR_Expression) : IR_BuildString = IR_BuildString(filenamePieceFpp.name,
    ListBuffer(basename(noPath), IR_StringConstant("_rank"), rank, if (binaryFpp) IR_StringConstant(".bin") else ext))
  def buildFilenameData(noPath : Boolean) : IR_Expression = basename(noPath, Some(IR_StringConstant(fmt match {
    case "Binary" => ".bin"
    case "HDF"    => ".h5"
    case "XML"    => ".xmf"
  })))

  // specifies "fragment dimension" (i.e. how many fragments are written to a file)
  def dimFrags(global : Boolean) : IR_Expression = if (global) numFrags else numFragsPerBlock

  // writes the path to the "heavy data" file (incl. dataset for hdf5) to the xdmf file
  def printFilename(stream : IR_VariableAccess, dataset : String) : IR_Print = {
    val refFile = if (binaryFpp) buildFilenamePiece(noPath = true, curRank).toPrint else ListBuffer(buildFilenameData(noPath = true))
    val refDataset = if (fmt == "HDF") IR_StringConstant(":" + dataset) :: Nil else Nil

    IR_Print(stream, ListBuffer(indentData) ++ refFile ++ refDataset :+ IR_Print.newline)
  }

  // overload for dataset names whose names are not a string literal
  def printFilename(stream : IR_VariableAccess, dataset : IR_Expression) : IR_Print = {
    val refFile = if (binaryFpp) buildFilenamePiece(noPath = true, curRank).toPrint else ListBuffer(buildFilenameData(noPath = true))
    val refDataset = if (fmt == "HDF") IR_StringConstant(":") :: dataset :: Nil else Nil

    IR_Print(stream, ListBuffer(indentData) ++ refFile ++ refDataset :+ IR_Print.newline)
  }

  // write constant data once and reference the file containing the constant data afterwards
  def writeXdmfElemOrReferenceConstants(stream : IR_VariableAccess, writeConsts : ListBuffer[IR_Statement], elemToRef : String, altCondition : Option[IR_Expression] = None) : ListBuffer[IR_Statement] = {
    val selectGrid = if (binaryFpp) {
      ListBuffer[IR_Expression](IR_StringConstant("Grid/Grid["), curRank + 1, IR_StringConstant("]")) // collection of grids (one for each subdomain)
    } else {
      ListBuffer[IR_Expression](IR_StringConstant("Grid[1]")) // a single global grid
    }
    ListBuffer(
      new IR_IfCondition(altCondition getOrElse IR_ConstantsWrittenToFile().isEmpty,
        /* true branch */
        writeConsts,
        /* false branch */
        ListBuffer[IR_Statement](
          printXdmfElement(stream, XInclude(href = IR_ConstantsWrittenToFile(), xpath = XPath(selectGrid, elemToRef) : _*) : _*)
        )
      )
    )
  }

  // prints a complete xdmf file
  def writeXdmf : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // open file and write header
    val stream = newStream
    if (ioInterface == "fpp") {
      // build filename for each rank
      val buildStr = IR_VariableAccess(filenamePieceFpp.name, IR_StringDatatype)
      statements += IR_VariableDeclaration(buildStr)
      statements += buildFilenamePiece(noPath = false, rank = MPI_IV_MpiRank)
      statements += IR_ObjectInstantiation(stream, Duplicate(buildStr))
    } else {
      statements += IR_ObjectInstantiation(stream, Duplicate(filename))
    }
    statements += printXdmfElement(stream, xmlHeader)
    statements += printXdmfElement(stream, openXdmf)
    statements += printXdmfElement(stream, openDomain)

    // write global grid element
    statements ++= writeXdmfGrid(stream, global = ioInterface != "fpp")

    // write footer and close file
    statements += printXdmfElement(stream, closeDomain)
    statements += printXdmfElement(stream, closeXdmf)
    statements += IR_MemberFunctionCall(stream, "close")

    if (ioInterface != "fpp")
      ListBuffer(IR_IfCondition(MPI_IsRootProc(), statements))
    else
      statements
  }

  // builds filename for "heavy data" and writes via corresponding I/O interface
  def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    if (binaryFpp) {
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(filenamePieceFpp),
        buildFilenamePiece(noPath = false, MPI_IV_MpiRank),
        ioHandler(constsIncluded, filenamePieceFpp)
      )
    } else {
      val filenameData = IR_VariableAccess(IR_FileAccess.declareVariable("filenameData"), IR_StringDatatype)
      ListBuffer[IR_Statement](
        IR_VariableDeclaration(filenameData, buildFilenameData(noPath = false)),
        ioHandler(constsIncluded, filenameData)
      )
    }
  }

  /* writes a xdmf "grid" element. a "grid" contains topology & geometry data to describe the mesh and also attributes (e.g. fields) that belong to the mesh
    used by writeXdmf and the "main" file for file-per-process
      - global = true : for single-shared file approaches (mpiio, hdf5)
      - global = false: for file-per-process. writes a domain piece

      NOTE: in case of XML we write the data directly into the Xdmf file, otherwise we reference the file with the data
  */
  def writeXdmfGrid(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val gridName = if (binaryFpp) {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), curRank)
    } else {
      IR_StringConstant("Grid")
    }
    statements += printXdmfElement(stream, openGrid(gridName, "Uniform") : _*)
    statements ++= writeXdmfGeometry(stream, global)
    statements ++= writeXdmfTopology(stream, global)
    statements ++= writeXdmfAttributes(stream, global)
    statements += printXdmfElement(stream, closeGrid)

    statements
  }

  // ternary condition (depending on constant data reduction) used print the seek pointer for DataItem with index "idx"
  private var seekpIdx : Int = 0
  def getSeekp(global : Boolean) : IR_TernaryCondition = {
    val idx = seekpIdx
    val idxNoConst = idx - (dataBuffers(constsIncluded = true).length - dataBuffers(constsIncluded = false).length)
    seekpIdx += 1
    IR_TernaryCondition(IR_ConstantsWrittenToFile().isEmpty,
      IR_SimplifyExpression.simplifyIntegralExpr(seekpOffsets(global, constsIncluded = true).take(idx).reduceOption(_ + _).getOrElse(0)),
      IR_SimplifyExpression.simplifyIntegralExpr(seekpOffsets(global, constsIncluded = false).take(idxNoConst).reduceOption(_ + _).getOrElse(0)))
  }

  // contains expressions that calculate the seek pointer for each DataItem (used for raw binary files)
  def seekpOffsets(global : Boolean, constsIncluded : Boolean) : ListBuffer[IR_Expression] = {
    dataBuffers(constsIncluded).map(buf => if (global) buf.typicalByteSizeGlobal else buf.typicalByteSizeBlock)
  }

  // methods to be implemented in application.ir or field.ir
  def stmtsForPreparation : ListBuffer[IR_Statement]
  /* geometry: describes the locations of a mesh's grid nodes */
  def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement]
  /* topology: specifies the connectivity between the mesh's grid nodes*/
  def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement]
  /* attributes: values centered on various locations of the grid (e.g. cell/node/...) */
  def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement]

  // construct xdmf filename for domain pieces in same directory as the global file (e.g. "test_rank0.xmf")
  protected def refPiecesXml(stream : IR_VariableAccess) : ListBuffer[IR_Statement] = (0 until Knowledge.mpi_numThreads).map(r => {
    printXdmfElement(stream,
      XInclude(href = buildFilenamePiece(noPath = true, rank = IR_IntegerConstant(r)).toPrint,
        xpath = IR_StringConstant("/Xdmf/Domain/Grid[1]")) : _*) // assumes the file of process "curRank" only has one grid instance
  }).to[ListBuffer]

  // constant data reduction handling
  def writeDataAndSetConstFile() : ListBuffer[IR_Statement] = {
    // free buffer if only used once, others are used in each print step and free'd later
    val freeTmpBuffersConst : ListBuffer[IR_Statement] = ListBuffer()
    dataBuffersConst.foreach(constBuf => {
      if (Knowledge.parIO_vis_constantDataReduction && constBuf.isTemporaryBuffer) {
        if (constBuf.accessBlockwise) {
          freeTmpBuffersConst += IR_IfCondition(constBuf.name,
            ListBuffer[IR_Statement](
              IR_ArrayFree(constBuf.name),
              IR_Assignment(constBuf.name, 0)))
        } else {
          Logger.error("Unimplemented: temp. buffers are currently only stored block-wise")
        }
      }
    })

    ListBuffer({
      if (fmt != "XML") {
        // write data into a separate, binary file
        IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
          /* true: write constants to file and save filename to reference later */
          writeData(constsIncluded = true)
            ++ freeTmpBuffersConst
            :+ IR_ConstantsWrittenToFile().setFilename(basename(noPath = true), Some(ext)),
          /* false: write field data and reference constants from saved filename */
          writeData(constsIncluded = false))
      } else {
        // data is already incorporated in the xml file
        IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
          IR_Assignment(IR_ConstantsWrittenToFile(),
            IR_MemberFunctionCall(filenamePieceFpp, "substr", lastIdxSubst(filenamePieceFpp, "\"\\\\/\"") + 1))) // constant file in same dir -> remove path
      }
    })
  }

  override def expand() : OutputType = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    if (!Settings.additionalIncludes.contains("string"))
      Settings.additionalIncludes += "string"

    if (Knowledge.parIO_vis_constantDataReduction) {
      filename match {
        case _ : IR_StringConstant => Logger.warn("Constants are reduced but filename is constant; Do not use \"printField\" in a loop with this parameter combination, otherwise the reduction will go wrong.")
        case _                     =>
      }
    }

    if (!IR_GlobalCollection.get.variables.contains(endianness)) {
      if (!IR_GlobalCollection.get.externalDependencies.contains("arpa/inet.h"))
        IR_GlobalCollection.get.externalDependencies += "arpa/inet.h"
      IR_GlobalCollection.get.variables += endianness
    }

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // e.g. set up fragment info
    statements ++= stmtsForPreparation

    // write "main" file for file-per-process with root rank
    if (ioInterface == "fpp") {
      val stream = newStream

      val printGlobalFile : ListBuffer[IR_Statement] = ListBuffer()

      // open and write header
      printGlobalFile += IR_ObjectInstantiation(stream, Duplicate(filename))
      printGlobalFile += printXdmfElement(stream, xmlHeader)
      printGlobalFile += printXdmfElement(stream, openXdmf)
      printGlobalFile += printXdmfElement(stream, openDomain)

      val printPieces = if (binaryFpp) {
        ListBuffer(
          printXdmfElement(stream, openGrid(IR_StringConstant("Grid"), "Collection") : _*),
          IR_ForLoop(
            IR_VariableDeclaration(curRank, 0),
            IR_Lower(curRank, Knowledge.mpi_numThreads),
            IR_PreIncrement(curRank),
            writeXdmfGrid(stream, global = false)),
          printXdmfElement(stream, closeGrid))
      } else {
        ListBuffer(
          printXdmfElement(stream, openGrid(IR_StringConstant("GlobalGrid"), "Collection") : _*)) ++
          refPiecesXml(stream) :+ // when using ascii, the data is directly incorporated in the xdmf file -> reference xdmf file of each piece
          printXdmfElement(stream, closeGrid)
      }

      printGlobalFile ++= printPieces

      // print footer and close
      printGlobalFile += printXdmfElement(stream, closeDomain)
      printGlobalFile += printXdmfElement(stream, closeXdmf)
      printGlobalFile += IR_MemberFunctionCall(stream, "close")

      statements += IR_IfCondition(MPI_IsRootProc(),
        printGlobalFile
      )
    }

    // for binaryFpp: previously written "main" file contains all info -> only writing the binary files is left now
    if (!binaryFpp) {
      statements ++= writeXdmf
    }

    statements ++= writeDataAndSetConstFile()

    statements
  }
}
