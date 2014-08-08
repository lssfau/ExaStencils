package exastencils.prettyprinting

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import exastencils.core.Settings

object PrettyprintingManager {
  protected var printers = new HashMap[String, Prettyprinter]
  protected var printerStack = new Stack[Prettyprinter]

  def getFiles = printers.keys
  def getPrettyprinters = printers.values

  def getCurrentPrinter = printerStack.head
  def pushPrinter(printer : Prettyprinter) = printerStack.push(printer)
  def popPrinter() = printerStack.pop

  protected class Prettyprinter(val filename : String, val path : String) extends java.io.PrintWriter(path) {
    def <<(s : String) = write(s)
    def <<<(s : String) = write(s + "\n")

    def finish = close

    protected var dependencies_ = new HashSet[Prettyprinter]
    def addDependency(prettyprinter : Prettyprinter) = dependencies_ += prettyprinter
    def addDependency(filename : String) = dependencies_ += getPrinter(filename)
    def removeDependency(prettyprinter : Prettyprinter) = dependencies_ -= prettyprinter
    def removeDependency(filename : String) = dependencies_ -= getPrinter(filename)
    def dependencies = dependencies_.toList
  }

  protected class HeaderPrettyprinter(filename : String, path : String) extends Prettyprinter(filename, path) {
    protected var guard = "EXASTENCILS_" + filename.replace("/", "_").replace("""\""", "_").replace(".", "_").toUpperCase()

    <<<(s"#ifndef $guard")
    <<<(s"#define $guard")

    override def finish = {
      <<<("\n#endif")
      super.finish
    }
  }

  def getPrinter(filename : String) : Prettyprinter = {
    printers.getOrElse(filename, {
      var file = new java.io.File(Settings.outputPath + java.io.File.separator + filename)
      if (!file.getParentFile().exists()) file.getParentFile().mkdirs()

      exastencils.core.Logger.debug("new file: " + file.getAbsolutePath)

      if (filename.endsWith(".h") || filename.endsWith(".hpp") || filename.endsWith(".hxx")) {
        var printer = new HeaderPrettyprinter(filename, file.getAbsolutePath())
        printers += ((filename, printer))
        printer
      } else {
        var printer = new Prettyprinter(filename, file.getAbsolutePath())
        printers += ((filename, printer))
        printer
      }

    })
  }

  def finish = {
    printers.values.foreach(f => f.finish)
    Settings.buildfileGenerator.write

    printers.clear
    printerStack.clear
  }
}

