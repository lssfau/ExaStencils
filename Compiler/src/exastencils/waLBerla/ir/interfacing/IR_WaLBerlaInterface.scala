package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._
import exastencils.waLBerla.ir.util.IR_WaLBerlaPreprocessorDirectives

object IR_WaLBerlaInterface {
  def defHeader(className : String) : String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".h"
  def defSource(className : String): String = IR_WaLBerlaCollection.defBasePath + "_" + className + ".impl.h"

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

    /* preprocessor directives */
    writerHeader <<< IR_WaLBerlaPreprocessorDirectives.headerTop

    /* class */
    writerHeader <<< s"namespace walberla {"
    writerHeader <<< s"namespace exastencils { "
    writerHeader <<< s"template< typename $WB_StencilTemplate >"
    writerHeader <<< s"class $interfaceName {"
    writerHeader <<< "public:"

    /* member functions */
    functions foreach { f => writerHeader << "\t" + f.prettyprint_decl() }

    /* ctors */
    for (ctor <- context.ifaceConstructors)
      writerHeader << ctor.prettyprint()

    /* dtors */
    for (dtor <- context.ifaceDestructors)
      writerHeader << dtor.prettyprint()

    /* members */
    writerHeader <<< "public:"
    for (pubDecl <- context.publicMemberDeclarationMap.values)
      writerHeader <<< "\t" + pubDecl.prettyprint()
    writerHeader <<< "private:"
    for (privDecl <- context.privateMemberDeclarationMap.values)
      writerHeader <<< "\t" + privDecl.prettyprint()

    writerHeader <<< "};" // end class
    writerHeader <<< "}\n}" // end namespaces

    /* include template function impls */
    functions foreach { f => writerHeader << "#include \"" + defSource(f.name) + "\"\n" }

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
      val waLBerlaUsed = wbFunctions.exists(_.isUserFunction) || IR_WaLBerlaFieldCollection.objects.nonEmpty

      // check if knowledge flag is set when waLBerla data structures are used
      if (waLBerlaUsed && !Knowledge.waLBerla_generateInterface)
        Logger.error("Knowledge flag 'waLBerla_generateInterface' must be enabled when using waLBerla data structures")

      // create interface object
      if (Knowledge.waLBerla_generateInterface) {
        collection.interfaceInstance = Some(IR_WaLBerlaInterface(Duplicate(wbFunctions)))
        collection.functions = collection.functions diff wbFunctions
      }

      collection
  })
}
