package exastencils.prettyprinting

import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import exastencils.core.Settings

object PrettyPrintManager {
  protected var printers = new HashMap[String, PrettyPrinter]
  protected var printerStack = new Stack[PrettyPrinter]
  
  def getFiles = printers.keys
  
  def getCurrentPrinter = printerStack.head
  def pushPrinter(printer : PrettyPrinter) = printerStack.push(printer)
  def popPrinter() = printerStack.pop

  protected class PrettyPrinter(filename : String) extends java.io.PrintWriter(filename) {
    def <<(s : String) = write(s)
    def <<<(s : String) = write(s + "\n")
  }

  def getPrinter(filename : String) : PrettyPrinter = {
    printers.getOrElse(filename, {
      var file = new java.io.File(Settings.outputPath + java.io.File.separator + filename)
      if (!file.getParentFile().exists()) file.getParentFile().mkdirs()
      
      exastencils.core.DBG("new file: " + file.getAbsolutePath)

      var printer = new PrettyPrinter(file.getAbsolutePath())
      printers += ((filename, printer))
      printer
    })
  }
  
  def finish = {
    printers.values.foreach(f => f.close())
    Settings.buildfileGenerator.write
  }
}



