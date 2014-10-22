package exastencils.datastructures.l3

import exastencils.knowledge._

object Layouts {
  def genSet(mapping : (Int => Int)) = { (0 until Knowledge.dimensionality).toArray.map(mapping).mkString(", ") }

  def addLayouts(printer : java.io.PrintWriter) = {
    var fieldDatatype = (if (Knowledge.l3tmp_genVectorFields) s"Array[Real][${Knowledge.l3tmp_numVecDims}]" else "Real")

    printer.println(s"Layout NoComm< $fieldDatatype >@all {")
    printer.println(s"\tghostLayers = [ ${genSet(_ => 0)} ]")
    printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ]")
    printer.println(s"}")

    if (Knowledge.l3tmp_genVectorFields) {
      printer.println(s"Layout NoCommScalar< Real >@coarsest {")
      printer.println(s"\tghostLayers = [ ${genSet(_ => 0)} ]")
      printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ]")
      printer.println(s"}")
    }

    printer.println(s"Layout CommPartTempBlockable< $fieldDatatype >@all {")
    if (Knowledge.l3tmp_genTemporalBlocking) {
      printer.println(s"\tghostLayers = [ ${genSet(_ => Knowledge.l3tmp_numPre - 1)} ] with communication")
      printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ] with communication")
    } else {
      printer.println(s"\tghostLayers = [ ${genSet(_ => 0)} ]")
      printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ]")
    }
    printer.println(s"}")

    printer.println(s"Layout BasicComm< $fieldDatatype >@all {")
    printer.println(s"\tghostLayers = [ ${genSet(_ => 1)} ] with communication")
    printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ] with communication")
    printer.println(s"}")

    if (Knowledge.l3tmp_genVectorFields) {
      printer.println(s"Layout BasicCommScalar< Real >@coarsest {")
      printer.println(s"\tghostLayers = [ ${genSet(_ => 1)} ] with communication")
      printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ] with communication")
      printer.println(s"}")
    }

    printer.println(s"Layout CommFullTempBlockable< $fieldDatatype >@all {")
    if (Knowledge.l3tmp_genTemporalBlocking)
      printer.println(s"\tghostLayers = [ ${genSet(_ => Knowledge.l3tmp_numPre)} ] with communication")
    else
      printer.println(s"\tghostLayers = [ ${genSet(_ => 1)} ] with communication")
    printer.println(s"\tduplicateLayers = [ ${genSet(_ => 1)} ] with communication")
    printer.println(s"}")

    if (Knowledge.l3tmp_genExtFields) {
      printer.println(s"Layout ExtSolLayout< $fieldDatatype >@finest {")
      printer.println(s"\tghostLayers = [ ${genSet(_ => 0)} ]")
      printer.println(s"\tduplicateLayers = [ ${genSet(_ => 0)} ]")
      printer.println(s"\tinnerPoints = [ ${genSet(i => (Knowledge.domain_fragLengthPerDim(i) * (1 << Knowledge.maxLevel)) + 1)} ]")
      printer.println(s"}")
    }

    printer.println
  }
}