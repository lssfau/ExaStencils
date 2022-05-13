package exastencils.visualization.ir.postprocessing.xdmf

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

trait IR_XdmfFormat {

  val indentData : IR_StringConstant = IR_StringConstant("\t\t\t\t\t")

  val endianness : IR_VariableDeclaration = IR_VariableDeclaration(IR_StringDatatype, "endianness",
    IR_TernaryCondition(IR_FunctionCall(IR_ExternalFunctionReference("htonl"), 47) EqEq 47, "\"Big\"", "\"Little\"")) // determine endianness in target code

  // to be implemented:
  def ioInterface : String
  def binaryFpp : Boolean

  /* special xml elements for referencing in xdmf */

  def xmlHeader = "<?xml version=\\\"1.0\\\" encoding=\\\"utf-8\\\"?>"
  def XInclude(href : IR_Expression, xpath : IR_Expression*) : ListBuffer[IR_Expression] = {
    ListBuffer(
      IR_StringConstant("\t\t\t<xi:include href=\\\""), href, IR_StringConstant("\\\" "), // reference file
      IR_StringConstant("xpointer=\\\"xpointer(")) ++ xpath.to[ListBuffer] :+ IR_StringConstant(")\\\"/>") // reference specific elements in the file
  }
  def XInclude(href : ListBuffer[IR_Expression], xpath : IR_Expression*) : ListBuffer[IR_Expression] = {
    (IR_StringConstant("\t\t\t<xi:include href=\\\"") +: href :+ IR_StringConstant("\\\" ")) ++
      (IR_StringConstant("xpointer=\\\"xpointer(") +: xpath.to[ListBuffer] :+ IR_StringConstant(")\\\"/>"))
  }
  def XPath(grid : ListBuffer[IR_Expression], elem : String) : ListBuffer[IR_Expression] = {
    IR_StringConstant("/Xdmf/Domain/") +: grid :+ IR_StringConstant("/" + elem)
  }


  /* open xdmf elements */

  def openXdmf : String = {
    "<Xdmf xmlns:xi=\\\"http://www.w3.org/2001/XInclude\\\" Version=\\\"3.0\\\">"
  }
  def openDomain : String = {
    "\t<Domain>"
  }
  def openGrid(name : IR_Expression, tpe : String) : ListBuffer[IR_Expression] = {
    ListBuffer(IR_StringConstant("\t\t<Grid Name=\\\""), name, IR_StringConstant(s"""\\\" GridType=\\\"$tpe\\\">"""))
  }
  def openGeometry(tpe : String) : String = {
    s"""\t\t\t<Geometry GeometryType=\\\"$tpe\\\">"""
  }
  def openTopology(tpe : String, dims : ListBuffer[IR_Expression], npe : Option[String] = None) : ListBuffer[IR_Expression] = {
    ListBuffer(IR_StringConstant(
      "\t\t\t<Topology Dimensions=\\\"")) ++ separateSequenceAndFilter(dims) :+ IR_StringConstant(s"""\\\" Type=\\\"$tpe\\\"""") :+
      (if (npe.isDefined) IR_StringConstant(s""" NodesPerElement=\\\"${ npe.get }\\\">""") else IR_StringConstant(">"))
  }
  def openAttribute(name : String, tpe : String, ctr : String) : String = {
    s"""\t\t\t<Attribute Center=\\\"$ctr\\\" AttributeType=\\\"$tpe\\\" Name=\\\"$name\\\">"""
  }
  def openDataItem(dt : IR_Datatype, dims : ListBuffer[IR_Expression], seekp : IR_Expression = 0, name : String = "", altFmt : Option[String] = None) : ListBuffer[IR_Expression] = {
    ListBuffer(IR_StringConstant(
      s"""\t\t\t\t<DataItem Name=\\\"${ name }\\\" DataType=\\\"${ numberType(dt) }\\\" Precision=\\\"${ dt.typicalByteSize }\\\" Dimensions=\\\"""")) ++ separateSequenceAndFilter(dims) :+
      IR_StringConstant(s"""\\\" Format=\\\"${ altFmt getOrElse fmt }\\\" Endian=\\\"""") :+ IR_VariableAccess(endianness) :+
      IR_StringConstant("\\\" Seek=\\\"") :+ seekp :+ IR_StringConstant("\\\">")
  }
  def openDataItemFunction(dims : ListBuffer[IR_Expression], function : String) : ListBuffer[IR_Expression] =
    ListBuffer(IR_StringConstant(
      s"""\t\t\t\t<DataItem ItemType=\\\"Function\\\" Function=\\\"$function\\\" Dimensions=\\\"""")) ++ separateSequenceAndFilter(dims) :+ IR_StringConstant("\\\">")
  // hyperslabs DataItems consist of two other DataItems: the selection and the source
  def openDataItemHyperslab(dims : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = {
    ListBuffer(IR_StringConstant(
      s"""\t\t\t\t<DataItem ItemType=\\\"HyperSlab\\\" Dimensions=\\\"""")) ++ separateSequenceAndFilter(dims) :+
      IR_StringConstant(s"""\\\" Type=\\\"HyperSlab\\\">""")
  }

  /* close xdmf elements */

  def closeXdmf = "</Xdmf>"
  def closeDomain = "\t</Domain>"
  def closeGrid = "\t\t</Grid>"
  def closeGeometry = "\t\t\t</Geometry>"
  def closeTopology = "\t\t\t</Topology>"
  def closeAttribute = "\t\t\t</Attribute>"
  def closeDataItem = "\t\t\t\t</DataItem>"

  /* helper functions */

  def fmt : String = ioInterface match {
    case "mpiio" => "Binary"
    case "hdf5"  => "HDF"
    case "fpp"   => if (binaryFpp) "Binary" else "XML"
  }

  // separate dim arrays
  def separator = IR_StringConstant(" ")
  def separateSequence(dims : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = dims
    .reverse // KJI order
    .flatMap(List(_, separator))
    .dropRight(1)
  def separateSequenceAndFilter(dims : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = separateSequence(
    dims.filter(d => d != IR_IntegerConstant(1) || d != IR_IntegerConstant(0)) // remove unnecessary dim specifications
  )

  // xdmf type conversion tables
  def attributeType(gridDatatype : IR_Datatype) : String = gridDatatype match {
    case _ : IR_ScalarDatatype                                                => "Scalar"
    case _ : IR_VectorDatatype                                                => "Vector"
    case matrix : IR_MatrixDatatype if matrix.sizeN == 1 || matrix.sizeM == 1 => "Vector"
    case matrix : IR_MatrixDatatype if matrix.sizeN == 1 && matrix.sizeM == 1 => "Scalar"
    case _                                                                    => Logger.error("Unsupported datatype for IR_PrintXdmf: " + gridDatatype.prettyprint)
  }

  def centeringType(localization : IR_Localization) : String = localization match {
    case IR_AtNode       => "Node"
    case IR_AtCellCenter => "Cell"
    // according to the docs ("https://www.xdmf.org/index.php/XDMF_Model_and_Format"), face centered variables are supported but do not map well for vis. systems (e.g. ParaView)
    case IR_AtFaceCenter(_) => "Cell" // values are interpolated
    case _                  => Logger.error("Unsupported localization for IR_PrintXdmf!")
  }

  // table from: https://www.xdmf.org/index.php/XDMF_Model_and_Format
  def numberType(dt : IR_Datatype) : String = dt match {
    case IR_FloatDatatype | IR_DoubleDatatype | IR_RealDatatype => "Float"
    case IR_IntegerDatatype                                     => "Int"
    case IR_CharDatatype                                        => "Char"
    case IR_SpecialDatatype("unsigned int")                     => "UInt"
    case IR_SpecialDatatype("unsigned char")                    => "UChar"
  }

  // hyperslab helpers
  def dataItemHyperslabSelection(start : ListBuffer[IR_Expression], stride : ListBuffer[IR_Expression], count : ListBuffer[IR_Expression]) : ListBuffer[IR_Expression] = {
    var buffer : ListBuffer[IR_Expression] = ListBuffer()
    buffer ++= IR_StringConstant(s"""\t\t\t\t\t<DataItem Dimensions=\\\"3 ${ start.length }\\\" Format=\\\"XML\\\">""") :: IR_Print.newline :: Nil
    buffer ++= IR_StringConstant("\t" + indentData.value) +: separateSequence(start) :+ IR_Print.newline
    buffer ++= IR_StringConstant("\t" + indentData.value) +: separateSequence(stride) :+ IR_Print.newline
    buffer ++= IR_StringConstant("\t" + indentData.value) +: separateSequence(count) :+ IR_Print.newline
    buffer += IR_StringConstant("\t" + closeDataItem)
    buffer
  }
  def dataItemHyperslabSource(dt : IR_Datatype, dims : ListBuffer[IR_Expression], printFilename : IR_Print, seekp : IR_Expression) : ListBuffer[IR_Expression] = {
    var buffer : ListBuffer[IR_Expression] = ListBuffer()
    buffer ++= IR_StringConstant("\t") +: openDataItem(dt, dims, seekp) :+ IR_Print.newline
    buffer ++= IR_StringConstant("\t") +: printFilename.toPrint
    buffer += IR_StringConstant("\t" + closeDataItem)
    buffer
  }
}
