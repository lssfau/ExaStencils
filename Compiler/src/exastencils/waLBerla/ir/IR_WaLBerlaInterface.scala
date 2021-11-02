package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager

object IR_WaLBerlaInterface {
  def defHeader(className : String) : String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".h"
  def defSource(className : String): String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".cpp"

  def interfaceHeader = defHeader(interfaceName)
  def interfaceName = "ExaInterface"
}

case class IR_WaLBerlaInterface(var functions : ListBuffer[IR_WaLBerlaFunction]) extends IR_Node with FilePrettyPrintable {

  import IR_WaLBerlaInterface._

  val context = IR_WaLBerlaInterfaceGenerationContext(functions)

  def printHeader() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(defHeader(interfaceName))

    /* dependencies */
    writerHeader.addInternalDependency(IR_UserFunctions.defHeader)
    writerHeader.addInternalDependency(IR_WaLBerlaCollection.defHeader)
    // headers from waLBerla
    if (Knowledge.cuda_enabled)
    // TODO inner/outer split ?
      writerHeader.addExternalDependency("cuda/GPUField.h")
    else if (Platform.targetHardware == "CPU")
      writerHeader.addExternalDependency("field/GhostLayerField.h")
    writerHeader.addExternalDependency("core/DataTypes.h")
    writerHeader.addExternalDependency("stencil/all.h")
    writerHeader.addExternalDependency("blockforest/communication/UniformBufferedScheme.h")
    writerHeader.addExternalDependency("field/SwapableCompare.h")
    writerHeader.addExternalDependency("core/cell/Cell.h")
    writerHeader.addExternalDependency("field/communication/PackInfo.h")
    writerHeader.addExternalDependency("domain_decomposition/BlockDataID.h")
    writerHeader.addExternalDependency("domain_decomposition/IBlock.h")
    writerHeader.addExternalDependency("domain_decomposition/StructuredBlockStorage.h")
    writerHeader.addExternalDependency("set")

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.headerTop

    /* class */
    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace exastencils { "
    writerHeader <<< s"class $interfaceName {"
    writerHeader <<< "public:"

    /* member functions */
    functions foreach { f => writerHeader << "\t" + f.prettyprint_decl() }

    /* ctor */
    writerHeader << s"\t$interfaceName"
    writerHeader << s"(${context.ctorParams.map(_.prettyprint()).mkString(", ")})"
    writerHeader << context.ctorInitializerList.prettyprint
    writerHeader <<< " {"
    for (stmt <- context.ctorBody) {
      writerHeader << "\t"
      writerHeader <<< stmt.prettyprint
    }
    writerHeader <<< "\t}"

    /* member */
    writerHeader <<< "private:"
    for (member <- context.members)
      writerHeader <<< "\t" + IR_VariableDeclaration(member).prettyprint()

    writerHeader <<< "};" // end class
    writerHeader <<< "}\n}" // end namespaces

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.headerBottom
  }

  def printSource() : Unit = {
    functions foreach { f =>
      val writerHeader = PrettyprintingManager.getPrinter(defSource(f.name))

      /* dependencies */
      writerHeader.addInternalDependency(defHeader(interfaceName))
      writerHeader.addInternalDependency(IR_WaLBerlaCollection.defHeader)
      // waLBerla headers
      writerHeader.addExternalDependency("core/DataTypes.h")
      writerHeader.addExternalDependency("core/Macros.h")

      /* preprocessor directives */
      writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceTop

      writerHeader <<< s"namespace walberla {"
      writerHeader <<< s"namespace exastencils { "

      writerHeader << f.prettyprint()

      writerHeader <<< "}\n}" // end namespaces

      /* preprocessor directives */
      writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceBottom
    }
  }

  override def printToFile() : Unit = {
    printHeader()
    printSource()
  }
}

object IR_WaLBerlaCreateInterface extends DefaultStrategy("Find functions and create targets for them") {
  this += Transformation("Transform WaLBerlaCollection functions to waLBerla interface functions", {
    case collection : IR_WaLBerlaCollection =>
      // transform collected wb functions into the wb <-> exa interface class
      val wbFunctions = collection.functions.collect { case f : IR_WaLBerlaFunction if f.isInterfaceFunction => f }
      if (wbFunctions.nonEmpty) {
        collection.interfaceInstance = Some(IR_WaLBerlaInterface(Duplicate(wbFunctions)))
        collection.functions = collection.functions diff wbFunctions
      }

      collection
  })
}
