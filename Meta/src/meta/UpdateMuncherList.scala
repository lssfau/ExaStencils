//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package meta

import scala.collection.mutable._

object UpdateMuncherList {
  def recursivelyListFiles(f : java.io.File) : Array[java.io.File] = {
    val (files, folders) = f.listFiles.partition(_.isFile)
    files.filter(_.getName.endsWith(".scala")) ++ folders.flatMap(recursivelyListFiles)
  }

  def collectApplicableFiles() = {
    var files = recursivelyListFiles(new java.io.File("./Compiler/src/")).map(_.getPath.replace('\\', '/'))

    // filter everything that is not in the exastencils package
    files = files.filter(_.startsWith("./Compiler/src/exastencils/"))

    // filer layer-independent files
    files = files.filter(f => Layer.all.map(l => s"/${ l.lc }/").map(f.contains).reduce(_ || _))

    files
  }

  def update() = {
    var body = ""

    val files = collectApplicableFiles()
    files.transform(_.drop("./Compiler/src/exastencils/".length))

    val inPackages = files.groupBy(_.split("/").dropRight(2).mkString("."))

    inPackages.foreach(p => {
      var s = ""

      val pack = p._1
      val entries = Map[String, ListBuffer[Layer]]()
      p._2.foreach(e => {
        val entryName = e.split("/").last.dropRight(".scala".length).drop(3)
        if (!entries.contains(entryName)) entries += entryName -> ListBuffer()
        entries(entryName) += Layer.matchLc(e.split("/").dropRight(1).last)
      })

      s += s"  if (true) { // in exastencils.${ pack }\n"
      s += s"""    curPackage = "exastencils.${ pack }"\n"""
      s += s"\n"

      entries.foreach(e => s += s"""    entries += Entry(curPackage, "${ e._1 }", ${ Layer.matchLayerList(e._2) })\n""")

      s += s"  }\n"

      //println(pack + " -> " + "\n" + s) //entries.mkString(", "))
      //println(s)
      body += s
    })

    //inPackages.foreach(p => println(p._1 + " -> " + p._2.mkString(", ")))

    //files.foreach(println)
    writeToFile(body)
  }

  def writeToFile(body : String) = {
    val printer = new Printer

    printer <<< "package meta"
    printer <<< ""

    printer <<< "import scala.collection.mutable.ListBuffer"
    printer <<< ""

    printer <<< "/// MuncherList"
    printer <<< ""

    printer <<< "object MuncherList {"
    printer <<< ""

    printer <<< "  import Layer._"
    printer <<< ""

    printer <<< "  object Entry {def apply(filePackage : String, className : String, layer : Layer) = new Entry(filePackage, className, ListBuffer(layer)) }"
    printer <<< ""

    printer <<< "  case class Entry(var filePackage : String, var className : String, var layers : ListBuffer[Layer])"
    printer <<< ""

    printer <<< "  val entries = ListBuffer[Entry]()"
    printer <<< ""

    printer <<< "  var curPackage = \"\""
    printer <<< ""

    printer << body

    printer <<< "}"

    Generatable.printToFile(s"./Meta/src/meta/MuncherList.scala", printer.toString)
  }
}