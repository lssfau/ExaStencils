package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_Node
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager

object IR_WaLBerlaFunctionTargets {
  def defHeader(className : String) : String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".h"
  def defSource(className : String): String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".cpp"
}

// TODO rename

case class IR_WaLBerlaFunctionTargets(
    var functions : ListBuffer[IR_WaLBerlaFunction]
) extends IR_Node with FilePrettyPrintable {

  val namespace = "exastencils"

  if (functions.map(_.name).toSet.size != 1)
    Logger.error("IR_WaLBerlaFuncttionTarget: Function names should be identical.")

  def targetName = functions.head.name

  def printHeader() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaFunctionTargets.defHeader(targetName))

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

    /* function */
    functions foreach { f => writerHeader << f.prettyprint_decl() }

    writerHeader <<< "}\n}" // namespaces

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.headerBottom
  }

  def printSource() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaFunctionTargets.defSource(targetName))

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaFunctionTargets.defHeader(targetName))
    writerHeader.addInternalDependency(IR_WaLBerlaCollection.defHeader)
    // waLBerla headers
    writerHeader.addExternalDependency("core/DataTypes.h")
    writerHeader.addExternalDependency("core/Macros.h")

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceTop

    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace $namespace { "

    /* function */
    functions foreach { f => writerHeader << f.prettyprint() }

    writerHeader <<< "}\n}" // namespaces

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.sourceBottom
  }

  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.waLBerlafunctionNodes.isEmpty)
      return

    printHeader()
    printSource()
  }
}

object IR_WaLBerlaCreateFunctionTargets extends DefaultStrategy("Find functions and create targets for them") {
  var plainFunctions : ListBuffer[IR_WaLBerlaPlainFunction] = ListBuffer()
  var leveledFunctions : mutable.HashMap[String, ListBuffer[IR_WaLBerlaFunction]] = mutable.HashMap()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    plainFunctions = ListBuffer()
    leveledFunctions = mutable.HashMap()

    super.apply(applyAtNode)

    val plainFuncClasses = plainFunctions.map(f => IR_WaLBerlaFunctionTargets(ListBuffer(f)))
    val levFuncClasses = leveledFunctions.values.map(f => IR_WaLBerlaFunctionTargets(f))

    ExaRootNode.ir_root.nodes ++= plainFuncClasses
    ExaRootNode.ir_root.nodes ++= levFuncClasses
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
