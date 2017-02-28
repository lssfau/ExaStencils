package exastencils.util

import scala.io.Source

import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.datastructures._
import exastencils.prettyprinting._

object CImg {
  def resolveLdLibs ():String= {
    var flags = ""

    if (Knowledge.library_CImg)
      Platform.targetOS match {
        case "Windows"       => flags += " -lgdi32 "
        case "Linux" | "OSX" => flags += " -lm -lpthread -lX11"
      }

    flags
  }
}

case class CImg() extends Node with FilePrettyPrintable {
  override def printToFile() : Unit = {
    if (!Knowledge.library_CImg) {
      return
    }

    val writer = PrettyprintingManager.getPrinter(s"Util/CImg.h")

    val url = getClass.getResource("/res/CImg.h")
    for (line <- Source.fromURL(url, "utf8").getLines())
      writer << (line + '\n')

  }
}
