package exastencils.datastructures.l3

import exastencils.knowledge._

object Layouts {
  def addLayouts(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 => {
        printer.println("Layout BasicComm {")
        printer.println("\tghostLayers = [ 1, 1 ] with communication")
        printer.println("\tduplicateLayers = [ 1, 1 ] with communication")
        printer.println("}")

        printer.println(s"Layout CommFullTempBlockable {")
        if (Knowledge.testTempBlocking)
          printer.println(s"\tghostLayers = [ ${Knowledge.numPre}, ${Knowledge.numPre} ] with communication")
        else
          printer.println(s"\tghostLayers = [ 1, 1 ] with communication")
        printer.println(s"\tduplicateLayers = [ 1, 1 ] with communication")
        printer.println(s"}")

        if (Knowledge.testTempBlocking) {
          printer.println(s"Layout CommPartTempBlockable {")
          if (Knowledge.numPre > 1)
            printer.println(s"\tghostLayers = [ ${Knowledge.numPre - 1}, ${Knowledge.numPre - 1} ] with communication")
          else
            printer.println("\tghostLayers = [ 0, 0 ]")
          printer.println(s"\tduplicateLayers = [ 1, 1 ] with communication")
          printer.println(s"}")
        }

        printer.println("Layout NoComm {")
        printer.println("\tghostLayers = [ 0, 0 ]")
        printer.println("\tduplicateLayers = [ 1, 1 ]")
        printer.println("}")

        if (Knowledge.testExtFields) {
          printer.println("Layout ExtSolLayout {")
          printer.println("\tghostLayers = [ 0, 0 ]")
          printer.println("\tduplicateLayers = [ 0, 0 ]")
          printer.println(s"\tinnerPoints = [ ${(Knowledge.domain_fragLengthPerDim(0) * (1 << Knowledge.maxLevel)) + 1}, ${(Knowledge.domain_fragLengthPerDim(1) * (1 << Knowledge.maxLevel)) + 1} ]")
          printer.println("}")
        }
      }

      case 3 => {
        printer.println("Layout BasicComm {")
        printer.println("\tghostLayers = [ 1, 1, 1 ] with communication")
        printer.println("\tduplicateLayers = [ 1, 1, 1 ] with communication")
        printer.println("}")

        printer.println(s"Layout CommFullTempBlockable {")
        if (Knowledge.testTempBlocking)
          printer.println(s"\tghostLayers = [ ${Knowledge.numPre}, ${Knowledge.numPre}, ${Knowledge.numPre} ] with communication")
        else
          printer.println(s"\tghostLayers = [ 1, 1, 1 ] with communication")
        printer.println(s"\tduplicateLayers = [ 1, 1, 1 ] with communication")
        printer.println(s"}")

        if (Knowledge.testTempBlocking) {
          printer.println(s"Layout CommPartTempBlockable {")
          if (Knowledge.numPre > 1)
            printer.println(s"\tghostLayers = [ ${Knowledge.numPre - 1}, ${Knowledge.numPre - 1}, ${Knowledge.numPre - 1} ] with communication")
          else
            printer.println("\tghostLayers = [ 0, 0, 0 ]")
          printer.println(s"\tduplicateLayers = [ 1, 1, 1 ] with communication")
          printer.println(s"}")
        }

        printer.println("Layout NoComm {")
        printer.println("\tghostLayers = [ 0, 0, 0 ]")
        printer.println("\tduplicateLayers = [ 1, 1, 1 ]")
        printer.println("}")

        if (Knowledge.testExtFields) {
          printer.println("Layout ExtSolLayout {")
          printer.println("\tghostLayers = [ 0, 0, 0 ]")
          printer.println("\tduplicateLayers = [ 0, 0, 0 ]")
          printer.println(s"\tinnerPoints = [ ${(Knowledge.domain_fragLengthPerDim(0) * (1 << Knowledge.maxLevel)) + 1}, ${(Knowledge.domain_fragLengthPerDim(1) * (1 << Knowledge.maxLevel)) + 1}, ${(Knowledge.domain_fragLengthPerDim(2) * (1 << Knowledge.maxLevel)) + 1} ]")
          printer.println("}")
        }
      }
    }
    printer.println
  }
}