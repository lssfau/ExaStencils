package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.prettyprinting._

class FunctionCollection(var baseName : String,
    var includes : ListBuffer[String],
    var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer()) extends Node with FilePrettyPrintable {

  def printHeader = {
    val writer = PrettyprintingManager.getPrinter(s"${baseName}.h")

    for (inc <- includes) writer <<< inc
    for (func <- functions) writer << func.asInstanceOf[FunctionStatement].cpp_decl
  }

  def printSources = {
    for (f <- functions) {
      val writer = PrettyprintingManager.getPrinter(s"${baseName}_${f.asInstanceOf[FunctionStatement].name}.cpp")

      writer <<< "#include \"" + baseName + ".h\""
      writer <<< f.cpp
    }
  }

  override def printToFile = {
    functions = functions.sortBy(f => f.asInstanceOf[FunctionStatement].name)

    printHeader
    printSources
  }
}
