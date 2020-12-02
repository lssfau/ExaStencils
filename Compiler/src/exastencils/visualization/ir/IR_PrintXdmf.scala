package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_BooleanDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_ObjectInstantiation
import exastencils.base.ir.IR_PreIncrement
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
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IsRootProc
import exastencils.util.ir.IR_Print

abstract class IR_PrintXdmf(ioMethod : IR_Expression, binaryFpp : Boolean) extends IR_Statement with IR_Expandable with IR_PrintVisualization {

  val supportedInterfaces = ListBuffer("mpiio", "fpp", "hdf5")
  def constantsWritten = IR_VariableDeclaration(IR_BooleanDatatype, "constantsWritten", false)

  def ioInterface = ioMethod match {
    case s : IR_StringConstant if(supportedInterfaces.contains(s.value)) => s.value
    case _ =>
      Logger.error("Wrong I/O interface passed to \"printXdmf\"")
  }

  // determine endianness in target code
  val endianness = IR_VariableDeclaration(IR_StringDatatype, "endianness",
    IR_TernaryCondition(IR_FunctionCall(IR_ExternalFunctionReference("htonl"), 47) EqEq 47, "\"Big\"", "\"Little\""))

  val fmt = ioInterface match {
    case "mpiio" => "Binary"
    case "hdf5"  => "HDF"
    case "fpp"   => if(binaryFpp) "Binary" else "XML"
  }

  // implicit to overcome type erasure issues
  def printXdmfElement(stream : IR_VariableAccess, str : String*): IR_Statement = IR_Print(stream, str.map(s => IR_StringConstant(s)).to[ListBuffer] :+ IR_Print.newline)
  def printXdmfElement(stream : IR_VariableAccess, expr : IR_Expression*)(implicit d: DummyImplicit) : IR_Statement = IR_Print(stream, expr.to[ListBuffer] :+ IR_Print.newline)

  def xmlHeader = "<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\"?>"

  // open xdmf elements
  def openXdmf =
    "<Xdmf xmlns:xi=\\\"http://www.w3.org/2001/XInclude\\\" Version=\\\"3.0\\\">"
  def openDomain =
    "\t<Domain>"
  def openGrid(name : String, tpe : String) =
    s"""\t\t<Grid Name=\\\"$name\\\" GridType=\\\"$tpe\\\">"""
  def openGeometry(tpe : String) =
    s"""\t\t\t<Geometry GeometryType=\\\"$tpe\\\">"""
  def openTopology(tpe : String, dims : String, npe : String = "") =
    s"""\t\t\t<Topology Dimensions=\\\"$dims\\\" Type=\\\"$tpe\\\" NodesPerElement=\\\"$npe\\\">"""
  def openAttribute(tpe : String, ctr : String) =
    s"""\t\t\t<Attribute Center=\\\"$ctr\\\" AttributeType=\\\"$tpe\\\">"""
  def openDataItem(dt : IR_Datatype, dims : String, seekp : Int = 0) : IR_Expression =
    s"""\t\t\t\t<DataItem DataType=\\\"${dt.prettyprint}\\\" Precision=\\\"${dt.typicalByteSize}\\\"
       | Dimensions=\\\"$dims\\\" Format=\\\"$fmt\\\" Endian=\\\"" + IR_VariableAccess(endianness) + s"\\\" Seek=\\\"$seekp\\\">""".stripMargin

  // close xdmf elements
  def closeXdmf      = "</Xdmf>"
  def closeDomain    = "\t</Domain>"
  def closeGrid      = "\t\t</Grid>"
  def closeGeometry  = "\t\t\t</Geometry>"
  def closeTopology  = "\t\t\t</Topology>"
  def closeAttribute = "\t\t\t</Attribute>"
  def closeDataItem  = "\t\t\t\t</DataItem>"

  override def expand() : OutputType = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    if(!IR_GlobalCollection.get.variables.contains(constantsWritten)) {
      IR_GlobalCollection.get.variables += constantsWritten
    }
    if(!IR_GlobalCollection.get.variables.contains(endianness)) {
      if(!IR_GlobalCollection.get.externalDependencies.contains("arpa/inet.h"))
        IR_GlobalCollection.get.externalDependencies += "arpa/inet.h"
      IR_GlobalCollection.get.variables += endianness
    }

    // TODO define structure for inheriting classes

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // write global file with root
    // TODO: move?
    if(ioInterface == "fpp") {
      val stream = newStream

      // construct xdmf filename for domain pieces in same directory as the global file
      def fnPiece(rank : Int) = ListBuffer(basename(noPath = true), IR_StringConstant("_rank" + rank), if(binaryFpp) IR_StringConstant(".bin") else ext)
      def refPieces : ListBuffer[IR_Statement] = ListBuffer() ++ (0 until Knowledge.mpi_numThreads).map(rank => {  // assumes the file of process "rank" only has one grid instance
        printXdmfElement(stream,
          ListBuffer(IR_StringConstant("\t\t\t<xi:include href=\\\"")) ++
          fnPiece(rank) :+
          IR_StringConstant("\\\" xpointer=\\\"xpointer(//Xdmf/Domain/Grid[1])\\\"/>") : _*)
      })

      val printGlobalFile : ListBuffer[IR_Statement] = ListBuffer()

      // open and write header
      printGlobalFile += IR_ObjectInstantiation(stream, Duplicate(filename))
      printGlobalFile += printXdmfElement(stream, xmlHeader)
      printGlobalFile += printXdmfElement(stream, openXdmf)
      printGlobalFile += printXdmfElement(stream, openDomain)

      val printPieces = if(binaryFpp) {
        val curRank = IR_VariableAccess("curRank", IR_IntegerDatatype)
        ListBuffer(IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
          IR_Lower("curRank", Knowledge.mpi_numThreads),
          IR_PreIncrement("curRank"),
          ListBuffer[IR_Statement](
            openGrid("Grid" + curRank, "Collection"),
            // TODO add geometry, topology, attributes
            closeGrid)))
      } else {
        ListBuffer(printXdmfElement(stream, openGrid("GlobalGrid", "Collection"))) ++
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

    statements
  }
}
