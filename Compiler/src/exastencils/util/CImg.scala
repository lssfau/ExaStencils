package exastencils.util

import scala.io.Source
import exastencils.core.Settings
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.prettyprinting._
import exastencils.knowledge.Knowledge

case class CImg() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    if (!Knowledge.library_CImg) {
      return
    }

    Knowledge.targetOS match {
      case "Windows" => if (!Settings.additionalLibs.contains("gdi32")) Settings.additionalLibs += "gdi32"
      case "Linux" | "OSX" => {
        if (!Settings.additionalLibs.contains("m")) Settings.additionalLibs += "m"
        if (!Settings.additionalLibs.contains("pthread")) Settings.additionalLibs += "pthread"
        if (!Settings.additionalLibs.contains("X11")) Settings.additionalLibs += "X11"
      }
    }

    val writer = PrettyprintingManager.getPrinter(s"Util/CImg.h");

    val url = getClass.getResource("/res/CImg.h")
    for (line <- Source.fromURL(url, "utf8").getLines())
      writer << (line + '\n')

  }
}
