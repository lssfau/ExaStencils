package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_CharDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_ObjectInstantiation
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_TernaryCondition
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.OutputType
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess
import exastencils.io.ir.IR_FileAccess_FPP
import exastencils.io.ir.IR_FileAccess_HDF5
import exastencils.io.ir.IR_FileAccess_MPIIO
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.util.ir.IR_BuildString
import exastencils.util.ir.IR_Print

// IR_PrintXdmf: Visualization interface using the eXtensible Data Model and Format (Xdmf)
// to be used in combination with parallel I/O methods: MPI I/O, HDF5, file-per-process
// to be implemented as specific printer in exastencils.application.ir

abstract class IR_PrintXdmf(ioMethod : IR_Expression, binaryFpp : Boolean) extends IR_Statement with IR_Expandable with IR_PrintVisualization {

  // declarations
  val endianness = IR_VariableDeclaration(IR_StringDatatype, "endianness",
    IR_TernaryCondition(IR_FunctionCall(IR_ExternalFunctionReference("htonl"), 47) EqEq 47, "\"Big\"", "\"Little\"")) // determine endianness in target code
  // accesses
  val curRank = IR_VariableAccess("curRank", IR_IntegerDatatype)
  val filenamePieceFpp = IR_VariableAccess(IR_FileAccess.declareVariable("fnPiece"), IR_StringDatatype)

  val supportedInterfaces : ListBuffer[String] = ListBuffer("mpiio", "fpp", "hdf5")

  def ioInterface : String = ioMethod match {
    case s : IR_StringConstant if supportedInterfaces.contains(s.value) => if(s.value != "fpp" && binaryFpp) {
      Logger.error("Parameter \"binFpp\" set with I/O interface=" + s.value + ". Should be \"fpp\"")
    } else {
      s.value
    }
    case _ =>
      Logger.error("Wrong I/O interface passed to \"printXdmf\". Options are: " + supportedInterfaces.mkString("\"", "\", \"", "\""))
  }

  def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] // contains data buffers for node positions, connectivity and field values

  def ioHandler(constsIncluded : Boolean, fn : IR_Expression) : IR_FileAccess = {
    ioInterface match {
      case "mpiio" =>
        IR_FileAccess_MPIIO(fn, dataBuffers(constsIncluded), writeAccess = true)
      case "fpp"   =>
        IR_FileAccess_FPP(fn, dataBuffers(constsIncluded), useBinary = binaryFpp, writeAccess = true, separator, condition = true, optPrintComponents = None)
      case "hdf5"  =>
        IR_FileAccess_HDF5(fn, dataBuffers(constsIncluded), writeAccess = true)
      case _ =>
        Logger.error("Wrong I/O interface passed to \"printXdmf\". Options are: " + supportedInterfaces.mkString("\"", "\", \"", "\""))
    }
  }

  def fmt : String = ioInterface match {
    case "mpiio" => "Binary"
    case "hdf5"  => "HDF"
    case "fpp"   => if(binaryFpp) "Binary" else "XML"
  }

  // overloaded helper methods to write xdmf elements
  def printXdmfElement(stream : IR_VariableAccess, str : String*): IR_Statement = IR_Print(stream, str.map(s => IR_StringConstant(s)).to[ListBuffer] :+ IR_Print.newline)
  def printXdmfElement(stream : IR_VariableAccess, expr : IR_Expression*)(implicit d: DummyImplicit) : IR_Statement = IR_Print(stream, expr.to[ListBuffer] :+ IR_Print.newline)

  // xml elements used in xdmf
  def xmlHeader = "<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\"?>"
  def XInclude(href : IR_Expression, xpath : IR_Expression*) : ListBuffer[IR_Expression] = ListBuffer(
    IR_StringConstant("\t\t\t<xi:include href=\\\""), href, IR_StringConstant("\\\" "), // reference file
      IR_StringConstant("xpointer=\\\"xpointer(")) ++ xpath.to[ListBuffer] :+ IR_StringConstant(")\\\"/>") // reference specific elements in the file
  def XInclude(href : ListBuffer[IR_Expression], xpath : IR_Expression*) : ListBuffer[IR_Expression] =
    (IR_StringConstant("\t\t\t<xi:include href=\\\"") +: href :+ IR_StringConstant("\\\" ")) ++
      (IR_StringConstant("xpointer=\\\"xpointer(") +: xpath.to[ListBuffer] :+ IR_StringConstant(")\\\"/>"))
  def XPath(elem : String) : ListBuffer[IR_Expression] = if (binaryFpp) {
    ListBuffer(IR_StringConstant("/Xdmf/Domain/Grid/Grid["), curRank + 1, IR_StringConstant("]/" + elem)) // collection of grids (one for each subdomain)
  } else {
    ListBuffer[IR_Expression](IR_StringConstant("/Xdmf/Domain/Grid[1]/" + elem)) // a single global grid
  }

  // table from: https://www.xdmf.org/index.php/XDMF_Model_and_Format
  def numberType(dt : IR_Datatype) : String = dt match {
    case IR_FloatDatatype | IR_DoubleDatatype | IR_RealDatatype => "Float"
    case IR_IntegerDatatype                                     => "Int"
    case IR_CharDatatype                                        => "Char"
    case IR_SpecialDatatype("unsigned int")                     => "UInt"
    case IR_SpecialDatatype("unsigned char")                    => "UChar"
  }

  def separator = IR_StringConstant(" ")
  def separateSequenceAndFilter(dims : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = dims
    .reverse // KJI order
    .filter(d => d != IR_IntegerConstant(1) || d != IR_IntegerConstant(0)) // remove unnecessary dim specifications
    .flatMap(d => d :: separator :: Nil)
    .dropRight(1)

  // open xdmf elements
  def openXdmf =
    "<Xdmf xmlns:xi=\\\"http://www.w3.org/2001/XInclude\\\" Version=\\\"3.0\\\">"
  def openDomain =
    "\t<Domain>"
  def openGrid(name : IR_Expression, tpe : String) : ListBuffer[IR_Expression] = ListBuffer(IR_StringConstant(
    "\t\t<Grid Name=\\\""), name, IR_StringConstant(s"""\\\" GridType=\\\"$tpe\\\">"""))
  def openGeometry(tpe : String) =
    s"""\t\t\t<Geometry GeometryType=\\\"$tpe\\\">"""
  def openTopology(tpe : String, dims : ListBuffer[IR_Expression], npe : Option[String] = None) : ListBuffer[IR_Expression] = ListBuffer(IR_StringConstant(
    "\t\t\t<Topology Dimensions=\\\"")) ++ separateSequenceAndFilter(dims) :+ IR_StringConstant(s"""\\\" Type=\\\"$tpe\\\"""") :+
    (if(npe.isDefined) IR_StringConstant(s""" NodesPerElement=\\\"${npe.get}\\\">""") else IR_StringConstant(">"))
  def openAttribute(name : String, tpe : String, ctr : String) =
    s"""\t\t\t<Attribute Center=\\\"$ctr\\\" AttributeType=\\\"$tpe\\\" Name=\\\"$name\\\">"""
  def openDataItem(dt : IR_Datatype, dims : ListBuffer[IR_Expression], seekp : IR_Expression = 0) : ListBuffer[IR_Expression] = ListBuffer(IR_StringConstant(
    s"""\t\t\t\t<DataItem DataType=\\\"${numberType(dt)}\\\" Precision=\\\"${dt.typicalByteSize}\\\" Dimensions=\\\"""")) ++ separateSequenceAndFilter(dims) :+
      IR_StringConstant(s"""\\\" Format=\\\"$fmt\\\" Endian=\\\"""") :+ IR_VariableAccess(endianness) :+
      IR_StringConstant("\\\" Seek=\\\"") :+ seekp :+ IR_StringConstant("\\\">")

  // close xdmf elements
  def closeXdmf      = "</Xdmf>"
  def closeDomain    = "\t</Domain>"
  def closeGrid      = "\t\t</Grid>"
  def closeGeometry  = "\t\t\t</Geometry>"
  def closeTopology  = "\t\t\t</Topology>"
  def closeAttribute = "\t\t\t</Attribute>"
  def closeDataItem  = "\t\t\t\t</DataItem>"

  // helpers to construct filenames
  def buildFilenamePiece(noPath : Boolean, rank : IR_Expression) : IR_BuildString = IR_BuildString(filenamePieceFpp.name,
    ListBuffer(basename(noPath), IR_StringConstant("_rank"), rank, if (binaryFpp) IR_StringConstant(".bin") else ext))
  def buildFilenameData(noPath : Boolean)  : IR_Expression = basename(noPath, Some(IR_StringConstant(fmt match {
    case "Binary" => ".bin"
    case "HDF"    => ".h5"
  })))

  // KJI order: first dimension shows how many fragments were written
  def dimFrags(global : Boolean) : IR_Expression = if (global) numFrags else numFragsPerBlock

  def indentData = IR_StringConstant("\t\t\t\t\t")

  def printFilename(stream : IR_VariableAccess, dataset : String) : IR_Print = {
    val refFile = if (binaryFpp) buildFilenamePiece(noPath = true, curRank).toPrint else ListBuffer(buildFilenameData(noPath = true))
    val refDataset = if (fmt == "HDF") IR_StringConstant(":" + dataset) :: Nil else Nil

    IR_Print(stream, ListBuffer(indentData) ++ refFile ++ refDataset :+ IR_Print.newline)
  }

  // prints a complete xdmf file
  def writeXdmf : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // open file and write header
    val stream = newStream
    if(ioInterface == "fpp") {
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

    statements
  }

  /* writes a xdmf "grid" element. used by writeXdmf and the "main" file for file-per-process
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
  def getSeekp(idx : Int, global : Boolean) : IR_TernaryCondition = {
    val idxNoConst = idx - (dataBuffers(constsIncluded = true).length - dataBuffers(constsIncluded = false).length)
    IR_TernaryCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      IR_SimplifyExpression.simplifyIntegralExpr(seekpOffsets(global, constsIncluded = true).take(idx).reduceOption(_ + _).getOrElse(0)),
      IR_SimplifyExpression.simplifyIntegralExpr(seekpOffsets(global, constsIncluded = false).take(idxNoConst).reduceOption(_ + _).getOrElse(0)))
  }

  // contains expressions that calculate the seek pointer for each DataItem (used for raw binary files)
  def seekpOffsets(global : Boolean, constsIncluded : Boolean) : ListBuffer[IR_Expression] = {
    dataBuffers(constsIncluded).map(_.typicalByteSize(global))
  }

  // methods to be implemented in application.ir
  def stmtsForPreparation : ListBuffer[IR_Statement]
  def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement]
  def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement]
  def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement]
  def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement]

  override def expand() : OutputType = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    if (!Settings.additionalIncludes.contains("string"))
      Settings.additionalIncludes += "string"

    // header for I/O interfaces
    ioHandler(constsIncluded = false, filename).handleDependencies()

    if (!IR_GlobalCollection.get.variables.contains(endianness)) {
      if (!IR_GlobalCollection.get.externalDependencies.contains("arpa/inet.h"))
        IR_GlobalCollection.get.externalDependencies += "arpa/inet.h"
      IR_GlobalCollection.get.variables += endianness
    }

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // set up fragment info
    statements ++= stmtsForPreparation

    // write "main" file for file-per-process with root rank
    if (ioInterface == "fpp") {
      val stream = newStream

      // construct xdmf filename for domain pieces in same directory as the global file
      def refPieces : ListBuffer[IR_Statement] = (0 until Knowledge.mpi_numThreads).map(r => {  // assumes the file of process "curRank" only has one grid instance
        printXdmfElement(stream,
          XInclude(href = buildFilenamePiece(noPath = true, rank = IR_IntegerConstant(r)).toPrint,
            xpath = IR_StringConstant("/Xdmf/Domain/Grid[1]")) : _*)
      }).to[ListBuffer]

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
        ListBuffer(printXdmfElement(stream, openGrid(IR_StringConstant("GlobalGrid"), "Collection") : _*)) ++
          refPieces :+ // when using ascii, the data is directly incorporated in the xdmf file -> reference xdmf file of each piece
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

    if (fmt != "XML") {
      // write data into a separate, binary file
      statements += IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
        /* true: write constants to file and save filename to reference later */
        writeData(constsIncluded = true) :+ IR_Assignment(IR_IV_ConstantsWrittenToFile(), basename(noPath = true) + ext),
        /* false: write field data and reference constants from saved filename */
        writeData(constsIncluded = false))
    } else {
      // data is already incorporated in the xml file
      statements += IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
        IR_Assignment(IR_IV_ConstantsWrittenToFile(),
          IR_MemberFunctionCall(filenamePieceFpp, "substr", lastIdxSubst(filenamePieceFpp, "\"\\\\/\"") + 1))) // constant file in same dir -> remove path
    }

    statements
  }
}
