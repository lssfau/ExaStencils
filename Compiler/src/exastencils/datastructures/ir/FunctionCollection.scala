package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.prettyprinting._

class FunctionCollection(var baseName : String,
    var externalDependencies : ListBuffer[String],
    var internalDependencies : ListBuffer[String],
    var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer()) extends Node with FilePrettyPrintable {

  def printHeader = {
    val writer = PrettyprintingManager.getPrinter(s"${baseName}.h")
    for (inc <- internalDependencies) writer.addInternalDependency(inc)
    for (inc <- externalDependencies) writer.addExternalDependency(inc)

    writer <<< "// template functions"
    for (func <- functions)
      if (func.asInstanceOf[FunctionStatement].hasAnnotation("isTemplate"))
        writer <<< func.prettyprint + ";"

    writer <<< "// ordinary functions"
    if (Knowledge.generateFortranInterface)
      writer <<< "extern \"C\" {"

    for (func <- functions)
      if (!func.asInstanceOf[FunctionStatement].hasAnnotation("isTemplate"))
        writer << func.asInstanceOf[FunctionStatement].prettyprint_decl

    if (Knowledge.generateFortranInterface)
      writer <<< "}"
  }

  def printSources = {
    for (f <- functions) {
      if (!f.asInstanceOf[FunctionStatement].hasAnnotation("isTemplate")) {
        val writer = PrettyprintingManager.getPrinter(s"${baseName}_${f.asInstanceOf[FunctionStatement].name}.cpp")
        writer.addInternalDependency(s"${baseName}.h")

        writer <<< f.prettyprint
      }
    }
  }

  override def printToFile = {
    functions = functions.sortBy(f => f.asInstanceOf[FunctionStatement].name)

    printHeader
    printSources
  }
}
