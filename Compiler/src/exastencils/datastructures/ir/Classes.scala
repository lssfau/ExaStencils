package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
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

abstract class Class(var className : String = "CLASS_NAME") extends Statement {
  var declarations : ListBuffer[Statement] = ListBuffer()
  // FIXME: use specialized c'tor and d'tor nodes
  var cTorArgs : ListBuffer[Expression] = ListBuffer()
  var cTorInitList : ListBuffer[Expression] = ListBuffer()
  var cTorBody : ListBuffer[Statement] = ListBuffer()
  var dTorBody : ListBuffer[Statement] = ListBuffer()
  var functions : ListBuffer[AbstractFunctionStatement] = ListBuffer()

  def cpp : String = {
    var s : String = ""

    s += s"class $className\n{\n"

    s += s"public:\n"

    for (decl <- declarations)
      s += s"${decl.cpp}\n"

    s += s"$className (${cTorArgs.map(stat => stat.cpp).mkString(", ")})\n:\n"
    s += cTorInitList.map(stat => stat.cpp).mkString(",\n")
    s += s"{\n"
    for (stat <- cTorBody)
      s += s"${stat.cpp}\n"
    s += s"}\n"

    s += s"~$className ()\n"
    s += s"{\n"
    for (stat <- dTorBody)
      s += s"${stat.cpp}\n"
    s += s"}\n"

    for (func <- functions) {
      val function = func.asInstanceOf[FunctionStatement]
      s += s"${function.returntype.cpp} ${function.name.cpp.split("::")(1)}(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
    }

    s += s"};\n"

    return s
  }
}
