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

package exastencils.prettyprinting

import scala.collection.mutable.StringBuilder

object Indenter {
  def addIndentations(toIndent : String) : String = {
    var indent = 0
    var output : StringBuilder = new StringBuilder

    for (c <- toIndent) {
      c match {
        case '{'  =>
          indent += 1
          output.append(c)
        case '}'  =>
          indent -= 1
          if ('\t' == output.charAt(output.length - 1))
            output.deleteCharAt(output.length - 1) // remove last tab
          output.append(c)
        case '\n' =>
          output.append(c)
          for (_ <- 0 until indent)
            output.append('\t')
        case _    =>
          output.append(c)
      }
    }

    output.toString
  }
}