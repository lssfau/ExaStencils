package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes._


object IR_WaLBerlaSweep {
  def defHeader = IR_WaLBerlaFunctions.defBaseName + "_Sweep.h"
  def defSource = IR_WaLBerlaFunctions.defBaseName + "_Sweep.cpp"

  val iblock = IR_VariableAccess("block", WB_IBlock)
  val iblockPtr = IR_FunctionArgument(iblock.name, IR_PointerDatatype(WB_IBlock))

  val blockStorage = IR_VariableAccess("blocks", WB_StructuredBlockStorage)
  val blockStoragePtr = IR_VariableAccess("blocks", IR_PointerDatatype(WB_StructuredBlockStorage))

  def getBlockDataID(acc : IR_FieldAccess) = IR_VariableAccess(acc.name + "_ID", WB_BlockDataID)
}

case class IR_WaLBerlaSweep(
    private var context : IR_WaLBerlaSweepGenerationContext
) extends IR_Node with FilePrettyPrintable {

  import IR_WaLBerlaSweep._

  val namespace = "exastencils"
  val className = "Solver"

  // TODO blockstorage ref member

  val blockDataIDs = context.fieldAccesses.map(acc => acc.name -> IR_FunctionArgument(getBlockDataID(acc))).toMap

  val ctorParams : ListBuffer[IR_FunctionArgument] = blockDataIDs.values.to[ListBuffer] ++ context.parameters
  val members : ListBuffer[IR_VariableAccess] = ctorParams.map(param => IR_VariableAccess(param.name + "_", param.datatype))

  def printHeader() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaSweep.defHeader)
    writerHeader.addExternalDependency(IR_WaLBerlaFunctions.defHeader)

    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace ${namespace} { "
    writerHeader <<< s"class ${className}"
    writerHeader <<< "{\npublic:"

    // TODO ...

    /* ctor */
    writerHeader << s"\t${className} ("
    for ((param, i) <- ctorParams.zipWithIndex) // param list
      writerHeader << param.prettyprint() + (if (i != ctorParams.size-1) "," else "")
    writerHeader << s") ${if (members.nonEmpty) ":" else ""} "
    for ((member, i) <- members.zipWithIndex) // initializer list
      writerHeader << member.prettyprint() + "(" + ctorParams(i).access.prettyprint() + ")" + (if (i != members.size-1) ", " else " ")
    writerHeader <<< "{};"

    /* functor */
    writerHeader <<< "void operator()();"

    /* member */
    writerHeader <<< "private:"
    for (member <- members)
      writerHeader <<< "\t" + IR_VariableDeclaration(member).prettyprint()


    writerHeader <<< "};" // class
    writerHeader <<< "}\n}" // namespaces
  }

  def printSource() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaSweep.defSource)
    writerHeader.addExternalDependency(IR_WaLBerlaSweep.defHeader)
    writerHeader.addExternalDependency(IR_WaLBerlaFunctions.defHeader)

    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace ${namespace} { "

    // TODO ...

    /* functor */
    writerHeader <<< s"void ${className}::operator()() {"
    // ...
    for (stmt <- context.body)
      writerHeader <<< stmt.prettyprint
    writerHeader <<< "}"
    // ...
    writerHeader <<< "}\n}" // namespaces
  }


  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.startNode.isEmpty)
      return

    printHeader()
    printSource()
  }
}
