package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
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

  val namespace = "exastencils"

  // TODO init function for lower-level & exa-internal fields (and members for each encapsulated in context)

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
    writerHeader <<< s"class $interfaceName {"
    writerHeader <<< "public:"

    /* member functions */
    functions foreach { f => writerHeader << "\t" + f.prettyprint_decl() }

    /* ctor */
    writerHeader << s"\t$interfaceName("
    for ((param, i) <- context.ctorParams.zipWithIndex) // param list
      writerHeader << param.prettyprint() + (if (i != context.ctorParams.size-1) ", " else "")
    writerHeader << s") ${if (context.members.nonEmpty) ":" else ""} "
    for ((member, i) <- context.members.zipWithIndex) // initializer list
      writerHeader << member.prettyprint() + "(" + context.ctorParams(i).access.prettyprint() + ")" + (if (i != context.members.size-1) ", " else " ")
    writerHeader <<< "{"
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
      writerHeader <<< s"namespace $namespace { "

      writerHeader << f.prettyprint()

      writerHeader <<< "}\n}" // end namespaces

      /* preprocessor directives */
      writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceBottom
    }
  }

  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.waLBerlafunctionNodes.isEmpty)
      return

    printHeader()
    printSource()
  }
}

object IR_WaLBerlaCreateInterface extends DefaultStrategy("Find functions and create targets for them") {
  var plainFunctions : ListBuffer[IR_WaLBerlaPlainFunction] = ListBuffer()
  var leveledFunctions : mutable.HashMap[String, ListBuffer[IR_WaLBerlaLeveledFunction]] = mutable.HashMap()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    plainFunctions = ListBuffer()
    leveledFunctions = mutable.HashMap()

    super.apply(applyAtNode)

    ExaRootNode.ir_root.nodes += IR_WaLBerlaInterface(plainFunctions ++ leveledFunctions.values.flatten)
  }

  this += Transformation("Collect and consume", {
    case f : IR_WaLBerlaLeveledFunction =>
      leveledFunctions.update(f.name, leveledFunctions.getOrElseUpdate(f.name, ListBuffer()) :+ f)
      None
    case f : IR_WaLBerlaPlainFunction   =>
      plainFunctions += f
      None
  })
}
