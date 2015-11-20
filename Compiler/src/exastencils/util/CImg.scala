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

    // FIXME embed CImg.h
    // CImg.h cannot be embedded easily because it's too large, breaking scalac
    for (line <- Source.fromFile(Knowledge.path_library_CImg).getLines())
      writer << (line + '\n')

  }
}
