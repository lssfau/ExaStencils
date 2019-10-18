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

import exastencils.base.ExaRootNode
import exastencils.base.l4._
import exastencils.config._
import exastencils.parsers.config._
import exastencils.parsers.l4._

object ValidateL4 {
  def main(args : Array[String]) : Unit = {
    // check from where to read input
    val settingsParser = new Settings_Parser(Settings)
    val knowledgeParser = new Settings_Parser(Knowledge)
    if (args.length >= 1) {
      settingsParser.parseFile(args(0))
    }
    if (args.length >= 2) {
      knowledgeParser.parseFile(args(1))
    }
    Knowledge.update()

    ExaRootNode.l4_root = L4_Root(Settings.getL4file.map(L4_Parser.parseFile(_) : L4_Node))
    ExaRootNode.l4_root.flatten()
    L4_Validation.apply()
  }
}
