package exastencils.prettyprinting

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.TreeSet

import exastencils.core._
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
    printers.getOrElse(filename, {
      var printer = new Prettyprinter(filename, (new java.io.File(Settings.getOutputPath + filename)).getAbsolutePath())
      printers += ((filename, printer))
      printer
    })
  }

  def finish = {
    printers.values.foreach(f => f.finish)
    Settings.buildfileGenerator.write
    JobScriptGenerator.write

    printers.clear
    printerStack.clear
  }

  protected class Prettyprinter(val filename : String, val path : String) extends java.io.StringWriter {
    import Prettyprinter._

    protected var internalDependencies_ = new TreeSet[Prettyprinter]()(Ordering.by(_.filename))
    def addInternalDependency(prettyprinter : Prettyprinter) = { internalDependencies_ += prettyprinter }
    def addInternalDependency(filename : String) = { internalDependencies_ += getPrinter(filename) }
    def removeInternalDependency(prettyprinter : Prettyprinter) = { internalDependencies_ -= prettyprinter }
    def removeInternalDependency(filename : String) = { internalDependencies_ -= getPrinter(filename) }
    def internalDependencies = internalDependencies_.toList

    protected var externalDependencies_ = new TreeSet[String]
    def addExternalDependency(filename : String) = { externalDependencies_ += filename }
    def removeExternalDependency(filename : String) = { externalDependencies_ -= filename }
    def externalDependencies = externalDependencies_.toList

    def <<(s : String) = write(s)
    def <<<(s : String) = write(s + "\n")

    def writeToFile = {
      val outFile = new java.io.FileWriter(path)
      outFile.write(Indenter.addIndentations(this.toString))
      outFile.close
    }

    def finish = {
      // post-process code files
      val isCodeFile = (filename.endsWith(".h") || filename.endsWith(".hpp") || filename.endsWith(".hxx")
        || filename.endsWith(".c") || filename.endsWith(".cpp") || filename.endsWith(".cxx")
        || filename.endsWith(".cuh") || filename.endsWith(".cu"))

      if (isCodeFile) {
        // temporary storage
        val extendedContent = new java.io.StringWriter

        // add header guard
        val addHeaderGuard = (filename.endsWith(".h") || filename.endsWith(".hpp") || filename.endsWith(".hxx") || filename.endsWith(".cuh"))
        if (addHeaderGuard) {
          val guard = "EXASTENCILS_" + filename.replace("/", "_").replace("""\""", "_").replace(".", "_").toUpperCase()
          extendedContent.write(s"#ifndef $guard\n")
          extendedContent.write(s"#define $guard\n\n")
        }

        // add includes for external dependencies
        for (dep <- externalDependencies)
          extendedContent.write(generateInclude(dep))
        if (externalDependencies.size > 0)
          extendedContent.write('\n')

        // add includes from Settings.additionalIncludes
        for (dep <- Settings.additionalIncludes)
          extendedContent.write(generateInclude(dep))
        if (Settings.additionalIncludes.size > 0)
          extendedContent.write('\n')

        // add includes for internal dependencies
        for (dep <- internalDependencies)
          extendedContent.write(generateInclude(dep.filename))
        if (internalDependencies.size > 0)
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
      if (!(new java.io.File(path)).exists) {
        Logger.debug("Creating file: " + path)
        var file = new java.io.File(path)
        if (!file.getParentFile().exists()) file.getParentFile().mkdirs()

        writeToFile
      } else if (Indenter.addIndentations(toString) != scala.io.Source.fromFile(path).mkString) {
        Logger.debug("Updating file: " + path)
        writeToFile
      }
    }
  }

  object Prettyprinter {
    def generateInclude(toInclude : String) = {
      val prepend = toInclude match {
        case "mpi.h"     => "#pragma warning(disable : 4800)\n"
        case "windows.h" => "#define NOMINMAX\n"
        case _           => ""
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

