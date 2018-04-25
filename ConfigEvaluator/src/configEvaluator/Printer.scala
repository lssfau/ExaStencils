package configEvaluator

class Printer extends java.io.StringWriter {
  def <<(s : String) = { write(s); this }
  def <<<(s : String) = { write(s + "\n"); this }

  def printToFile(filename : String) = {
    val content = this.toString

    if (!new java.io.File(filename).exists) {
      println("Creating " + filename)

      val file = new java.io.File(filename)
      if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

      val outFile = new java.io.FileWriter(filename)
      outFile.write(content)
      outFile.close()
    } else if (content != scala.io.Source.fromFile(filename).mkString) {
      println("Updating " + filename)

      val outFile = new java.io.FileWriter(filename)
      outFile.write(content)
      outFile.close()
    } else {
      println("Skipping " + filename)
    }
  }
}
