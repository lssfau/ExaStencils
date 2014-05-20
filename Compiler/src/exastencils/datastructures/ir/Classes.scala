package exastencils.datastructures.ir

import scala.collection.mutable.ListBuffer

abstract class Class extends Statement {
  var className : String = "CLASS_NAME"
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
      s += s"${function.returntype.cpp} ${function.name./*FIXME: COOKIE*/cpp.split("::")(1) /*FIXME: handle with reason*/ }(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
    }

    s += s"};\n"

    return s
  }
}
