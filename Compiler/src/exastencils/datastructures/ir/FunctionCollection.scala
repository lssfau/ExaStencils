package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.prettyprinting._

class FunctionCollection(var baseName : String,
    var includes : ListBuffer[String],
    var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer()) extends Node with FilePrettyPrintable {

  override def printToFile = {
    {
      val writer = PrettyprintingManager.getPrinter(s"${baseName}.h")

      for (inc <- includes)
        writer <<< inc

      for (func <- functions) {
        val function = func.asInstanceOf[FunctionStatement]
        writer << s"${function.returntype.cpp} ${function.name.cpp}(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
      }
    }

    var i = 0
    for (f <- functions) {
      val writer = PrettyprintingManager.getPrinter(s"${baseName}_$i.cpp")

      writer <<< "#include \"" + baseName + ".h\""
      writer <<< f.cpp

      i += 1
    }
  }
}
