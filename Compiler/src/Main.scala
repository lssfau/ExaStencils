import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers.l4
import exastencils.parsers.l4.ParserL4

import exastencils.knowledge._

import exastencils.application.GenerateCode_Application
import exastencils.domain.GenerateCode_Domain
import exastencils.multiGrid.GenerateCode_MultiGrid
import exastencils.primitives.GenerateCode_Primitives
import exastencils.util.GenerateCode_Util

object Main {
  def main(args : Array[String]) : Unit = {
    Globals.printPath = s"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ExaStencils_DSL/Generated/";

    GenerateCode_Primitives();
    GenerateCode_MultiGrid();
    GenerateCode_Application();
    GenerateCode_Domain();
    GenerateCode_Util();
  }
}
