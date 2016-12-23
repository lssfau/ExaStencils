package exastencils.deprecated.l3Generate

import exastencils.config._

object Domains {
  def getGlobalWidths : Array[Double] = {
    Knowledge.dimensionality match {
      case 1 => Array(1.0)
      case 2 =>
        if (Knowledge.l3tmp_kelvin) {
          Array(
            (1.0 + Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags))
              - (0.0 - Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags)),
            (1.0 + Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_y - 2 * Knowledge.l3tmp_kelvin_numHaloFrags))
              - (0.0 - Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_y - 2 * Knowledge.l3tmp_kelvin_numHaloFrags)))
        } else {
          Array(1.0, 1.0)
        }
      case 3 => Array(1.0, 1.0, 1.0)
    }
  }

  def addDomains(printer : java.io.PrintWriter) = {
    Knowledge.dimensionality match {
      case 2 =>
        if (!Knowledge.domain_rect_generate) {
          if (Knowledge.domain_readFromFile) {
            printer.println("Domain fromFile(\"DomainDefinition.cfg\")")

          } else {
            Knowledge.domain_useCase match {
              case "L-Shape"   =>
                printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
                //printer.println("""Domain LShaped< [ 0, 0 ] to [ 0.5, 0.5 ], [ 0.5, 0 ] to [ 1, 0.5 ], [ 0, 0.5 ] to [ 0.5, 1 ] >""")
                printer.println("""Domain LShaped< [ 0, 0 ] to [ 0.5, 0.5 ], [ 0, 0.5 ] to [ 0.5, 1 ], [ 0.5, 0.5 ] to [ 1, 1 ] >""")
              case "2-L-Shape" =>
                printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
                //printer.println("""Domain LShaped< [ 0, 0 ] to [ 0.5, 0.5 ], [ 0.5, 0 ] to [ 1, 0.5 ], [ 0, 0.5 ] to [ 0.5, 1 ] >""")
                printer.println("""Domain LShapedLower< [ 0.5, 0 ] to [ 0.75, 0.25 ], [  0.75, 0 ] to [  1, 0.25 ], [ 0.75, 0.25 ] to [  1, 0.5 ] >""")
                printer.println("""Domain LShapedUpper< [ 0, 0.5 ] to [ 0.25, 0.75 ], [ 0, 0.75 ] to [ 0.25, 1 ], [ 0.25, 0.75 ] to [ 0.5, 1 ] >""")
              case "X-Shape"   =>
                printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
                //printer.println("""Domain LShaped< [ 0, 0 ] to [ 0.5, 0.5 ], [ 0.5, 0 ] to [ 1, 0.5 ], [ 0, 0.5 ] to [ 0.5, 1 ] >""")
                printer.println("""Domain XShaped< [ 0, 0 ] to [ 0.25, 0.25 ], [ 0, 0.75 ] to [ 0.25, 1 ], [ 0.75, 0.75 ] to [ 1, 1 ], [ 0.75, 0 ] to [ 1, 0.25 ], [ 0.25, 0.25] to [0.75,0.75] >""")
              case _           => printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
            }
          }

        } else if (Knowledge.l3tmp_kelvin) {
          printer.println(s"Domain global< [ ${ 0.0 - Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) }, ${ 0.0 - Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_y - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) } ] " +
            s"to [ ${ 1.0 + Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_x - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) }, ${ 1.0 + Knowledge.l3tmp_kelvin_numHaloFrags * (1.0 - 0.0) / (Knowledge.domain_rect_numFragsTotal_y - 2 * Knowledge.l3tmp_kelvin_numHaloFrags) } ] >")
          printer.println("Domain innerDom< [ 0, 0 ] to [ 1, 1 ] >")
        } else {
          printer.println("Domain global< [ 0, 0 ] to [ 1, 1 ] >")
          if (Knowledge.l3tmp_genEmbeddedDomain)
            printer.println(s"Domain innerDom< [ ${ 0.0 + 1.0 / Knowledge.domain_rect_numFragsTotal_x }, ${ 0.0 + 1.0 / Knowledge.domain_rect_numFragsTotal_y } ] " +
              s"to [ ${ 1.0 - 1.0 / Knowledge.domain_rect_numFragsTotal_x }, ${ 1.0 - 1.0 / Knowledge.domain_rect_numFragsTotal_y } ] >")
        }
      case 3 =>
        printer.println("Domain global< [ 0, 0, 0 ] to [ 1, 1, 1 ] >")
        if (Knowledge.l3tmp_genEmbeddedDomain)
          printer.println(s"Domain innerDom< [ ${ 0.0 + 1.0 / Knowledge.domain_rect_numFragsTotal_x }, ${ 0.0 + 1.0 / Knowledge.domain_rect_numFragsTotal_y }, ${ 0.0 + 1.0 / Knowledge.domain_rect_numFragsTotal_z } ] " +
            s"to [ ${ 1.0 - 1.0 / Knowledge.domain_rect_numFragsTotal_x }, ${ 1.0 - 1.0 / Knowledge.domain_rect_numFragsTotal_y }, ${ 1.0 - 1.0 / Knowledge.domain_rect_numFragsTotal_z } ] >")
    }
    printer.println()
  }
}
