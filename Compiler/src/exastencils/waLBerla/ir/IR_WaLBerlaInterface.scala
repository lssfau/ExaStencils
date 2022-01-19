package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UserFunctions
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.prettyprinting._

trait IR_WaLBerlaInterfaceParameter extends IR_Access {
  def name : String
  def datatype : IR_Datatype

  def resolveAccess() : IR_Access

  def initializerListEntry : (IR_Access, IR_Expression) = (member, ctorParameter.access)
  def ctorParameter : IR_FunctionArgument
  def member : IR_VariableAccess

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()
}

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
    writerHeader << IR_Constructor(interfaceName, context.ctorParams, context.ctorInitializerList, context.ctorBody).prettyprint()

    /* dtor */
    writerHeader << IR_Destructor(interfaceName, context.dtorBody).prettyprint()

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
      if (wbFunctions.exists(_.isUserFunction)) {
        collection.interfaceInstance = Some(IR_WaLBerlaInterface(Duplicate(wbFunctions)))
        collection.functions = collection.functions diff wbFunctions
      }

      collection
  })
}
