package exastencils.deprecated.l3Generate

import exastencils.config._

object Layouts {
  def genSet(mapping : (Int => Int)) = { Knowledge.dimensions.map(mapping).mkString(", ") }

  def addLayouts(printer : java.io.PrintWriter) = {
    var fieldDatatype = (if (Knowledge.l3tmp_genVectorFields) s"Array<Real><${ Knowledge.l3tmp_numVecDims }>" else "Real") +
      (if (Knowledge.l3tmp_genCellBasedDiscr) ", Cell" else ", Node")
    var scalarDatatype = "Real, " + (if (Knowledge.l3tmp_genCellBasedDiscr) "Cell" else "Node")

    var defDup = if (Knowledge.l3tmp_genCellBasedDiscr) 0 else 1

    printer.println(s"Layout NoComm< $fieldDatatype >@all {")
    printer.println(s"\tghostLayers = [ ${ genSet(_ => 0) } ]")
    printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ]")
    printer.println(s"}")

    if (Knowledge.l3tmp_genVectorFields) {
      printer.println(s"Layout NoCommScalar< $scalarDatatype >@coarsest {")
      printer.println(s"\tghostLayers = [ ${ genSet(_ => 0) } ]")
      printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ]")
      printer.println(s"}")
    }

    printer.println(s"Layout CommPartTempBlockable< $fieldDatatype >@all {")
    if (Knowledge.l3tmp_genTemporalBlocking) {
      printer.println(s"\tghostLayers = [ ${ genSet(_ => Knowledge.l3tmp_numPre - 1) } ] with communication")
      printer.println(s"\tduplicateLayers = [ ${ genSet(_ => 1) } ] with communication")
    } else {
      printer.println(s"\tghostLayers = [ ${ genSet(_ => 0) } ]")
      printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ]")
    }
    printer.println(s"}")

    printer.println(s"Layout BasicComm< $fieldDatatype >@all {")
    printer.println(s"\tghostLayers = [ ${ genSet(_ => 1) } ] with communication")
    printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ] with communication")
    printer.println(s"}")

    if (Knowledge.l3tmp_genVectorFields) {
      printer.println(s"Layout BasicCommScalar< $scalarDatatype >@coarsest {")
      printer.println(s"\tghostLayers = [ ${ genSet(_ => 1) } ] with communication")
      printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ] with communication")
      printer.println(s"}")
    }

    printer.println(s"Layout CommFullTempBlockable< $fieldDatatype >@all {")
    if (Knowledge.l3tmp_genTemporalBlocking)
      printer.println(s"\tghostLayers = [ ${ genSet(_ => Knowledge.l3tmp_numPre) } ] with communication")
    else
      printer.println(s"\tghostLayers = [ ${ genSet(_ => 1) } ] with communication")
    printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ] with communication")
    printer.println(s"}")

    if (Knowledge.l3tmp_genExtFields) {
      printer.println(s"Layout ExtSolLayout< $fieldDatatype >@finest {")
      printer.println(s"\tghostLayers = [ ${ genSet(_ => 0) } ]")
      printer.println(s"\tduplicateLayers = [ ${ genSet(_ => 0) } ]")
      printer.println(s"\tinnerPoints = [ ${ genSet(i => (Knowledge.domain_fragmentLengthAsVec(i) * (1 << Knowledge.maxLevel)) + 1) } ]")
      printer.println(s"}")
    }

    if (Knowledge.l3tmp_genStencilFields) {
      if (Knowledge.l3tmp_genStencilStencilConv) {
        printer.println(s"Layout CommFullTempBlockableSF< Array<Real><${ var res = 1; for (i <- 0 until Knowledge.dimensionality) res *= 3; res }>, ${ if (Knowledge.l3tmp_genCellBasedDiscr) "Cell" else "Node" } >@all {")
        if (Knowledge.l3tmp_genTemporalBlocking)
          printer.println(s"\tghostLayers = [ ${ genSet(_ => Knowledge.l3tmp_numPre) } ] with communication")
        else
          printer.println(s"\tghostLayers = [ ${ genSet(_ => 1) } ] with communication")
        printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ] with communication")
        printer.println(s"}")
      } else {
        if (Knowledge.l3tmp_genTemporalBlocking) {
          printer.println(s"Layout CommPartTempBlockableSF< Array<Real><${ 2 * Knowledge.dimensionality + 1 }>, ${ if (Knowledge.l3tmp_genCellBasedDiscr) "Cell" else "Node" } >@all {")
          printer.println(s"\tghostLayers = [ ${ genSet(_ => Knowledge.l3tmp_numPre - 1) } ] with communication")
          printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ] with communication")
          printer.println(s"}")
        }
        printer.println(s"Layout NoCommSF< Array<Real><${ 2 * Knowledge.dimensionality + 1 }>, ${ if (Knowledge.l3tmp_genCellBasedDiscr) "Cell" else "Node" } >@all {")
        printer.println(s"\tghostLayers = [ ${ genSet(_ => 0) } ]")
        printer.println(s"\tduplicateLayers = [ ${ genSet(_ => defDup) } ]")
        printer.println(s"}")
      }
    }

    printer.println()
  }
}
