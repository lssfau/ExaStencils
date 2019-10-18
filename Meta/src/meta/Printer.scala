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

package meta

/// Printer

object Printer {
  def process(toProcess : String, layer : Layer) = {
    var string = toProcess

    if (string.contains("|LAYER_UC|"))
      string = string.replaceAllLiterally("|LAYER_UC|", layer.uc)
    if (string.contains("|LAYER_LC|"))
      string = string.replaceAllLiterally("|LAYER_LC|", layer.lc)

    if (string.contains("|NEXT_UC|"))
      string = string.replaceAllLiterally("|NEXT_UC|", layer.next.uc)
    if (string.contains("|NEXT_LC|"))
      string = string.replaceAllLiterally("|NEXT_LC|", layer.next.lc)

    string = string.replaceAll("\r\n", "\n")

    string
  }
}

class Printer extends java.io.StringWriter {
  def <<(s : String) = write(s)
  def <<<(s : String) = write(s + "\n")
}
