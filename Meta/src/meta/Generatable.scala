package meta

import scala.collection.mutable.ListBuffer

/// Generatable

object Generatable {
  def printToFile(filename : String, content : String) = {
    var withLineEndings = content
    if (Config.windows)
      withLineEndings = withLineEndings.replaceAll("\n", "\r\n")

    if (!new java.io.File(filename).exists) {
      println("Creating " + filename)

      val file = new java.io.File(filename)
      if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

      val outFile = new java.io.FileWriter(filename)
      outFile.write(withLineEndings)
      outFile.close()
    } else if (withLineEndings != scala.io.Source.fromFile(filename).mkString) {
      println("Updating " + filename)

      val outFile = new java.io.FileWriter(filename)
      outFile.write(withLineEndings)
      outFile.close()
    } else {
      println("Skipping " + filename)
    }
  }
}

trait Generatable {
  def validLayers() : ListBuffer[Layer]

  def filenameForLayer(layer : Layer) : String
  def generateForLayer(layer : Layer) : String

  def duplicateFromTo(copyFrom : Layer, layer : ListBuffer[Layer]) : Unit = {
    layer.foreach(duplicateFromTo(copyFrom, _))
  }

  def duplicateFromTo(copyFrom : Layer, layer : Layer) = {
    var filename = filenameForLayer(layer)
    filename = Printer.process(filename, layer)

    var content = generateForLayer(copyFrom)
    // ensure newline at end of file
    if (!content.endsWith("\n"))
      content += "\n"
    content = Printer.process(content, layer)

    //println(s"Handling $filename")
    Generatable.printToFile(filename, content)
  }

  def generate() = {
    for (layer <- validLayers()) {
      var filename = filenameForLayer(layer)
      filename = Printer.process(filename, layer)

      var content = generateForLayer(layer)
      // ensure newline at end of file
      if (!content.endsWith("\n"))
        content += "\n"
      content = Printer.process(content, layer)

      //println(s"Handling $filename")
      Generatable.printToFile(filename, content)
    }
  }
}
