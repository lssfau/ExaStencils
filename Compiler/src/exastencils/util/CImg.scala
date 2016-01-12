package exastencils.util

import scala.io.Source
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.prettyprinting._
import exastencils.knowledge.Knowledge

case class CImg() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    if (!Knowledge.library_CImg) {
      return
    }

    val writer = PrettyprintingManager.getPrinter(s"Util/CImg.h");

    val url = getClass.getResource("/res/CImg.h")
    for (line <- Source.fromURL(url, "utf8").getLines())
      writer << (line + '\n')

  }
}
