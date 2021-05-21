package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstReferenceDatatype
import exastencils.base.ir.IR_FunctionArgument
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_SharedPointerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager
import exastencils.waLBerla.ir.IR_WaLBerlaDatatypes._


object IR_WaLBerlaSweep {
  def headerName = "Sweep.h"
  def sourceName = "Sweep.cpp"
  def defHeader : String = IR_WaLBerlaFunctions.defBasePath + "_" + headerName
  def defSource: String = IR_WaLBerlaFunctions.defBasePath + "_" + sourceName

  val iblock = IR_VariableAccess("block", WB_IBlock)
  val iblockPtr = IR_FunctionArgument(iblock.name, IR_SharedPointerDatatype(WB_IBlock))

  val blockStorage = IR_VariableAccess("blocks", WB_StructuredBlockStorage)
  val blockStoragePtr = IR_VariableAccess(blockStorage.name, IR_SharedPointerDatatype(WB_StructuredBlockStorage))

  def getBlockDataID(name : String) = IR_VariableAccess(name + "_ID" + "_", WB_BlockDataID)
  def getBlocks = IR_VariableAccess(blockStoragePtr.name + "_", blockStoragePtr.datatype)
}

case class IR_WaLBerlaSweep(
    private var context : IR_WaLBerlaSweepGenerationContext
) extends IR_Node with FilePrettyPrintable {

  import IR_WaLBerlaSweep._

  val namespace = "exastencils"
  val className = "Solver"

  private def toBlockDataID(name : String) = IR_VariableAccess(name + "_ID", WB_BlockDataID)

  val blockDataIDs : Map[String, IR_FunctionArgument] = context.fields.map(acc => acc.name -> IR_FunctionArgument(toBlockDataID(acc.name))).toMap

  // extract ctor params
  var ctorParams : ListBuffer[IR_FunctionArgument] = ListBuffer()

  // block data IDs and params of waLBerla function
  ctorParams ++= blockDataIDs.values
  ctorParams ++= context.parameters

  // create member for each ctor param
  val members : ListBuffer[IR_VariableAccess] = ctorParams.map(param => IR_VariableAccess(param.name + "_", param.datatype))

  // block storage shared_ptr
  ctorParams += IR_FunctionArgument(IR_VariableAccess(blockStoragePtr.name, IR_ConstReferenceDatatype(blockStoragePtr.datatype)))
  members += IR_VariableAccess(blockStoragePtr.name + "_", blockStoragePtr.datatype)

  def printHeader() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaSweep.defHeader)

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaFunctions.defHeader)
    // headers from waLBerla
    if (Knowledge.cuda_enabled)
      // TODO inner/outer split ?
      writerHeader.addExternalDependency("cuda/GPUField.h")
    else if (Platform.targetHardware == "CPU")
      writerHeader.addExternalDependency("field/GhostLayerField.h")
    writerHeader.addExternalDependency("core/DataTypes.h")
    writerHeader.addExternalDependency("field/SwapableCompare.h")
    writerHeader.addExternalDependency("domain_decomposition/BlockDataID.h")
    writerHeader.addExternalDependency("domain_decomposition/IBlock.h")
    writerHeader.addExternalDependency("domain_decomposition/StructuredBlockStorage.h")
    writerHeader.addExternalDependency("set")

    /* defines and pragmas */
    writerHeader <<< """#ifdef __GNUC__
                       |#define RESTRICT __restrict__
                       |#elif _MSC_VER
                       |#define RESTRICT __restrict
                       |#else
                       |#define RESTRICT
                       |#endif
                       |
                       |#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                       |#   pragma GCC diagnostic push
                       |#   pragma GCC diagnostic ignored "-Wunused-parameter"
                       |#   pragma GCC diagnostic ignored "-Wreorder"
                       |#endif
                       |""".stripMargin

    /* class */
    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace $namespace { "
    writerHeader <<< s"class $className"
    writerHeader <<< "{\npublic:"

    // TODO ...

    /* ctor */
    writerHeader << s"\t$className ("
    for ((param, i) <- ctorParams.zipWithIndex) // param list
      writerHeader << param.prettyprint() + (if (i != ctorParams.size-1) ", " else "")
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

    /* pragma */
    writerHeader <<< """#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                       |#   pragma GCC diagnostic pop
                       |#endif
                       |""".stripMargin
  }

  def printSource() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaSweep.defSource)

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaSweep.defHeader)
    writerHeader.addInternalDependency(IR_WaLBerlaFunctions.defHeader)
    // waLBerla headers
    writerHeader.addExternalDependency("core/DataTypes.h")
    writerHeader.addExternalDependency("core/Macros.h")

    /* defines and macros */
    writerHeader <<< s"""
                       | ${if (Knowledge.cuda_enabled) "#define FUNC_PREFIX __global__" else if (Platform.targetHardware == "CPU") "#define FUNC_PREFIX"}
                       |
                       |#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                       |#   pragma GCC diagnostic push
                       |#   pragma GCC diagnostic ignored "-Wfloat-equal"
                       |#   pragma GCC diagnostic ignored "-Wshadow"
                       |#   pragma GCC diagnostic ignored "-Wconversion"
                       |#   pragma GCC diagnostic ignored "-Wunused-variable"
                       |#endif
                       |
                       |#if ( defined WALBERLA_CXX_COMPILER_IS_INTEL )
                       |#pragma warning push
                       |#pragma warning( disable :  1599 )
                       |#endif
                       |
                       |""".stripMargin

    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace $namespace { "

    // TODO ...

    /* functor */
    writerHeader <<< s"void $className::operator()() {"
    // ...
    for (stmt <- context.body)
      writerHeader <<< stmt.prettyprint
    writerHeader <<< "}"
    // ...
    writerHeader <<< "}\n}" // namespaces

    /* pragmas */
    writerHeader <<< """#if ( defined WALBERLA_CXX_COMPILER_IS_GNU ) || ( defined WALBERLA_CXX_COMPILER_IS_CLANG )
                       |#   pragma GCC diagnostic pop
                       |#endif
                       |
                       |#if ( defined WALBERLA_CXX_COMPILER_IS_INTEL )
                       |#pragma warning pop
                       |#endif
                       |""".stripMargin
  }


  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.startNode.isEmpty)
      return

    printHeader()
    printSource()
  }
}
