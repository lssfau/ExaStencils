package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl.ParameterInfo

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.prettyprinting._

case class ImplClass(var cname : String, var templ : String, var memlist : ListBuffer[ParameterInfo], var memfunc : ListBuffer[ImplFunction]) extends Node with FilePrettyPrintable {
  var name : String = cname
  var memberfunc = memfunc
  var membervar = memlist

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"${name}.h")

    writer << (
      s"#ifndef ${name.toUpperCase()}_H\n"
      + s"#define ${name.toUpperCase()}_H\n"
      + "#include \"Globals/Globals.h\"\n"
      + "#include \"Container/Container.h\"\n")

    // FIXME: HACK
    if ("MyStencil" == name) {
      writer <<< "#include \"MyArray.h\""
      writer <<< "#include <vector>"
    }

    if (!templ.equals("")) writer <<< "template<class " + templ + ">"

    writer << s"class $cname\n{\n"
    writer <<< "public:"
    writer <<< memlist.map(mem => mem.toString_cpp + ";\n").mkString("")
    writer <<< memfunc.map(fun => fun.toString_cpp + "\n").mkString("")
    writer <<< "};"

    writer <<< "#endif"
  }

  def costs(para : String) : Map[Int, String] = {
    var m : Map[Int, String] = Map()
    for (b <- memfunc) {
      m ++= b.costs(para)
      // println(b.costs)      
    }
    return m
  }
}
