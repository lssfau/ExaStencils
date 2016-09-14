package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Node
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge
import exastencils.prettyprinting._

class IR_FunctionCollection(var baseName : String,
    var externalDependencies : ListBuffer[String],
    var internalDependencies : ListBuffer[String],
    var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer()) extends IR_Node with FilePrettyPrintable {

  def printHeader() = {
    val writer = PrettyprintingManager.getPrinter(s"$baseName.h")
    for (inc <- internalDependencies)
      writer.addInternalDependency(inc)
    for (inc <- externalDependencies)
      writer.addExternalDependency(inc)

    val externC = Knowledge.generateFortranInterface || Knowledge.generateCInterface

    if (externC)
      writer <<< "extern \"C\" {"

    for (func <- functions)
      if (!func.isHeaderOnly && !func.hasAnnotation("deviceOnly"))
        writer << func.asInstanceOf[FunctionStatement].prettyprint_decl

    if (externC)
      writer <<< "}"
  }

  def printSources() = {
    // will be overwritten for kernel functions
    for (func <- functions)
      if (!func.isHeaderOnly) {
        val writer = PrettyprintingManager.getPrinter(s"${ baseName }_${ func.name }.cpp")
        writer.addInternalDependency(s"$baseName.h")

        writer <<< func.prettyprint
      }
  }

  override def printToFile() = {
    functions = functions.sortBy({
      case fs : FunctionStatement => fs.name
      case _                      => ""
    })

    printHeader()
    printSources()
  }
}
