package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager


object IR_WaLBerlaFunctor {
  def defHeader(className : String) : String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".h"
  def defSource(className : String): String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".cpp"
}

case class IR_WaLBerlaFunctor(
    private var context : IR_WaLBerlaFunctorGenerationContext
) extends IR_Node with FilePrettyPrintable {

  val namespace = "exastencils"

  def printHeader() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaFunctor.defHeader(context.className))

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaCollection.defHeader)
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
    writerHeader <<< s"class ${context.className}"
    writerHeader <<< "{\npublic:"

    // TODO ...

    /* ctor */
    writerHeader << s"\t${context.className} ("
    for ((param, i) <- context.ctorParams.zipWithIndex) // param list
      writerHeader << param.prettyprint() + (if (i != context.ctorParams.size-1) ", " else "")
    writerHeader << s") ${if (context.members.nonEmpty) ":" else ""} "
    for ((member, i) <- context.members.zipWithIndex) // initializer list
      writerHeader << member.prettyprint() + "(" + context.ctorParams(i).access.prettyprint() + ")" + (if (i != context.members.size-1) ", " else " ")
    writerHeader <<< "{};"

    /* functor */
    writerHeader <<< "\tvoid operator()();"

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
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaFunctor.defSource(context.className))

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaFunctor.defHeader(context.className))
    writerHeader.addInternalDependency(IR_WaLBerlaCollection.defHeader)
    // waLBerla headers
    writerHeader.addExternalDependency("core/DataTypes.h")
    writerHeader.addExternalDependency("core/Macros.h")

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceTop

    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace $namespace { "

    // TODO ...

    /* functor */
    writerHeader <<< s"void ${context.className}::operator()() {"
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
    if (IR_WaLBerlaUtil.functorNodes.isEmpty)
      return

    printHeader()
    printSource()
  }
}
