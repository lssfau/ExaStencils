//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.util

import scala.io.Source

import exastencils.base.ir.IR_Node
import exastencils.config._
import exastencils.prettyprinting._

object CImg {
  def resolveLdLibs() : String = {
    var flags = ""

    if (Knowledge.library_CImg)
      Platform.targetOS match {
        case "Windows"         => flags += " -lgdi32 "
        case "Linux" | "macOS" => flags += " -lm -lpthread -lX11 -ljpeg -lpng"
      }

    flags
  }
}

case class CImg() extends IR_Node with FilePrettyPrintable {
  override def printToFile() : Unit = {
    if (!Knowledge.library_CImg) {
      return
    }

    val writer = PrettyprintingManager.getPrinter(s"Util/CImg.h")

    val url = getClass.getResource("/CImg.h")
    for (line <- Source.fromURL(url, "utf8").getLines())
      writer << (line + '\n')
  }
}
