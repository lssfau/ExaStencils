package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager


object IR_WaLBerlaSweep {
  def headerName = "Sweep.h"
  def sourceName = "Sweep.cpp"
  def defHeader : String = IR_WaLBerlaFunctions.defBasePath + "_" + headerName
  def defSource: String = IR_WaLBerlaFunctions.defBasePath + "_" + sourceName
}

case class IR_WaLBerlaSweep(
    private var context : IR_WaLBerlaSweepGenerationContext
) extends IR_Node with FilePrettyPrintable {

  val namespace = "exastencils"
  val className = "Solver"

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

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.headerTop

    /* class */
    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace $namespace { "
    writerHeader <<< s"class $className"
    writerHeader <<< "{\npublic:"

    // TODO ...

    /* ctor */
    writerHeader << s"\t$className ("
    for ((param, i) <- context.ctorParams.zipWithIndex) // param list
      writerHeader << param.prettyprint() + (if (i != context.ctorParams.size-1) ", " else "")
    writerHeader << s") ${if (context.members.nonEmpty) ":" else ""} "
    for ((member, i) <- context.members.zipWithIndex) // initializer list
      writerHeader << member.prettyprint() + "(" + context.ctorParams(i).access.prettyprint() + ")" + (if (i != context.members.size-1) ", " else " ")
    writerHeader <<< "{};"

    /* functor */
    writerHeader <<< "void operator()();"

    /* member */
    writerHeader <<< "private:"
    for (member <- context.members)
      writerHeader <<< "\t" + IR_VariableDeclaration(member).prettyprint()


    writerHeader <<< "};" // class
    writerHeader <<< "}\n}" // namespaces

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.headerBottom
  }

  def printSource() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaSweep.defSource)

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaSweep.defHeader)
    writerHeader.addInternalDependency(IR_WaLBerlaFunctions.defHeader)
    // waLBerla headers
    writerHeader.addExternalDependency("core/DataTypes.h")
    writerHeader.addExternalDependency("core/Macros.h")

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceTop

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

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceBottom
  }


  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.startNode.isEmpty)
      return

    printHeader()
    printSource()
  }
}
