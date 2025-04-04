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

package exastencils.prettyprinting

import scala.collection.mutable.{ HashMap, ListBuffer, Stack, TreeSet }

import exastencils.config._
import exastencils.logger._

object PrettyprintingManager {
  protected var printers = new HashMap[String, Prettyprinter]
  protected var printerStack = new Stack[Prettyprinter]

  def getFiles = printers.keys
  def getPrettyprinters = printers.values

  def getCurrentPrinter = printerStack.head
  def pushPrinter(printer : Prettyprinter) = printerStack.push(printer)
  def popPrinter() = printerStack.pop

  def getPrinter(filename : String) : Prettyprinter = {
    printers.getOrElseUpdate(filename, new Prettyprinter(filename, new java.io.File(Settings.getOutputPath + filename).getAbsolutePath))
  }

  def finish() = {
    printers.values.foreach(f => f.finish())
    BuildfileGenerator.parseGenerators(Settings.buildfileGenerators).foreach(gen => gen.write())
    JobScriptGenerator.write()

    printers.clear()
    printerStack.clear()
  }

  protected class Prettyprinter(val filename : String, val path : String) extends java.io.StringWriter {

    import Prettyprinter._

    protected var internalDependencies_ = new TreeSet[Prettyprinter]()(Ordering.by(_.filename))
    def addInternalDependency(prettyprinter : Prettyprinter) = { internalDependencies_ += prettyprinter }
    def addInternalDependency(filename : String) = { internalDependencies_ += getPrinter(filename) }
    def removeInternalDependency(prettyprinter : Prettyprinter) = { internalDependencies_ -= prettyprinter }
    def removeInternalDependency(filename : String) = { internalDependencies_ -= getPrinter(filename) }
    def internalDependencies = internalDependencies_.toList

    protected var externalDependencies_ = new ListBuffer[String]()
    // preserve input ordering!
    def addExternalDependency(filename : String) : Unit = { externalDependencies_ += filename }
    def removeExternalDependency(filename : String) : Unit = { externalDependencies_ = externalDependencies_.distinct -= filename }
    def externalDependencies() : Seq[String] = {
      externalDependencies_ = externalDependencies_.distinct
      externalDependencies_
    }

    def <<(s : String) = write(s)
    def <<<(s : String) = write(s + "\n")

    def writeToFile() = {
      val outFile = new java.io.FileWriter(path)
      outFile.write(Indenter.addIndentations(this.toString))
      outFile.close()
    }

    def finish() = {
      // post-process code files
      val isCodeFile = (filename.endsWith(".h") || filename.endsWith(".hpp") || filename.endsWith(".hxx")
        || filename.endsWith(".c") || filename.endsWith(".cpp") || filename.endsWith(".cxx")
        || filename.endsWith(".cuh") || filename.endsWith(".cu"))

      if (isCodeFile) {
        // temporary storage
        val extendedContent = new java.io.StringWriter

        // add header guard
        val addHeaderGuard = filename.endsWith(".h") || filename.endsWith(".hpp") || filename.endsWith(".hxx") || filename.endsWith(".cuh")
        if (addHeaderGuard) {
          val guard = "EXASTENCILS_" + filename.replace("/", "_").replace("""\""", "_").replace(".", "_").toUpperCase()
          extendedContent.write(s"#ifndef $guard\n")
          extendedContent.write(s"#define $guard\n\n")
        }

        if (Knowledge.library_CImg) {
          extendedContent.write("#define cimg_use_jpeg\n")
          extendedContent.write("#define cimg_use_png\n")
        }

        // add includes for external dependencies
        for (dep <- externalDependencies())
          extendedContent.write(generateInclude(dep))
        if (externalDependencies().nonEmpty)
          extendedContent.write('\n')

        // add includes from Settings.additionalIncludes
        for (dep <- Settings.additionalIncludes)
          extendedContent.write(generateInclude(dep))
        if (Settings.additionalIncludes.nonEmpty)
          extendedContent.write('\n')

        // add includes from Settings.additionalNamespaces
        for (dep <- Settings.additionalNamespaces)
          extendedContent.write("using namespace " + dep + ";\n")
        if (Settings.additionalNamespaces.nonEmpty)
          extendedContent.write('\n')



        // add includes for internal dependencies
        for (dep <- internalDependencies)
          extendedContent.write(generateInclude(dep.filename))
        if (internalDependencies.nonEmpty)
          extendedContent.write('\n')

        // add main content
        extendedContent.write(this.toString)

        // add end of header guard
        if (addHeaderGuard)
          extendedContent.write("\n#endif\n")

        // empty and swap buffer
        this.getBuffer.setLength(0)
        this.write(extendedContent.toString)
      }

      // check if the file already exists
      if (!new java.io.File(path).exists) {
        Logger.debug("Creating file: " + path)
        var file = new java.io.File(path)
        if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

        writeToFile()
      } else if (Indenter.addIndentations(toString) != scala.io.Source.fromFile(path).mkString) {
        Logger.debug("Updating file: " + path)
        writeToFile()
      }
    }
  }

  object Prettyprinter {
    def generateInclude(toInclude : String) : String = {
      val prepend = toInclude match {
        case "mpi.h"         =>
          s"""#ifdef _MSC_VER
          |    #pragma warning(disable : 4800)
          |#endif""".stripMargin
        case "windows.h"     => "#define NOMINMAX\n"
        case "vecmathlib.h"  => "#define VML_NODEBUG\n"
        case "vectorclass.h" => "#define VCL_NAMESPACE vectorclass\n"
        case _               => ""
      }
      prepend + "#include \"" + toInclude + "\"\n"
    }

    def gatherDependencies(target : Prettyprinter, foundDependencies : TreeSet[String] = TreeSet()) : TreeSet[String] = {
      for (dep <- target.internalDependencies)
        if (!foundDependencies.contains(dep.filename)) {
          foundDependencies += dep.filename
          foundDependencies ++= gatherDependencies(dep, foundDependencies)
        }
      foundDependencies
    }
  }

}

