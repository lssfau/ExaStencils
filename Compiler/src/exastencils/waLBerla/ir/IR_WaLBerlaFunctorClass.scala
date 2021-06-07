package exastencils.waLBerla.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.logger.Logger
import exastencils.prettyprinting.FilePrettyPrintable
import exastencils.prettyprinting.PrettyprintingManager

object IR_WaLBerlaFunctorClass {
  def defHeader(className : String) : String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".h"
  def defSource(className : String): String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".cpp"
}

case class IR_WaLBerlaFunctorClass(
    var functors : ListBuffer[IR_WaLBerlaFunctor]
) extends IR_Node with FilePrettyPrintable {

  val namespace = "exastencils"

  val context = IR_WaLBerlaFunctorClassGenerationContext(this)

  if (functors.map(_.name).toSet.size != 1)
    Logger.error("IR_WaLBerlaFunctorClass: Functor names should be identical.")

  def className = functors.head.name

  def printHeader() : Unit = {
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaFunctorClass.defHeader(className))

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
    writerHeader <<< "\tvoid operator()();"
    functors foreach {
      case f : IR_WaLBerlaLeveledFunctor if f.level != f.maxLevel => writerHeader <<< s"\tvoid ${f.funcName}();"
      case _ =>
    }

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
    val writerHeader = PrettyprintingManager.getPrinter(IR_WaLBerlaFunctorClass.defSource(className))

    /* dependencies */
    writerHeader.addInternalDependency(IR_WaLBerlaFunctorClass.defHeader(className))
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
    for (f <- functors)
      writerHeader <<< f.prettyprint()
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

object IR_WaLBerlaCreateFunctorClasses extends DefaultStrategy("Find functors and create classes for them") {
  var plainFunctors : ListBuffer[IR_WaLBerlaPlainFunctor] = ListBuffer()
  var leveledFunctors : mutable.HashMap[String, ListBuffer[IR_WaLBerlaFunctor]] = mutable.HashMap()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    plainFunctors = ListBuffer()
    leveledFunctors = mutable.HashMap()

    super.apply(applyAtNode)

    val plainFuncClasses = plainFunctors.map(f => IR_WaLBerlaFunctorClass(ListBuffer(f)))
    val levFuncClasses = leveledFunctors.values.map(f => IR_WaLBerlaFunctorClass(f))

    ExaRootNode.ir_root.nodes ++= plainFuncClasses
    ExaRootNode.ir_root.nodes ++= levFuncClasses
  }

  this += Transformation("Collect and consume", {
    case f : IR_WaLBerlaLeveledFunctor =>
      leveledFunctors.update(f.name, leveledFunctors.getOrElseUpdate(f.name, ListBuffer()) :+ f)
      None
    case f : IR_WaLBerlaPlainFunctor =>
      plainFunctors += f
      None
  })
}
