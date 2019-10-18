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

import meta._

object Meta {
  def main(args : Array[String]) : Unit = {
    println(s"Performing operation '${ args(0) }'")

    args(0) match {
      case "update"    => update()
      case "collect"   => collect()
      case "generate"  => generate()
      case "duplicate" => duplicate()
      case "license"   => AddLicenseInfo()
      case other       => println(s"Unknown operation $other")
    }
  }

  def update() = {
    UpdateMuncherList.update()
  }

  def collect() = {
    for (entry <- MuncherList.entries)
      CodeMuncher.process(entry)
    CodeMuncher.generateGeneratorList()
    CodeMuncher.checkForUnhandledFiles()
  }

  def generate() = {
    for (entry <- GeneratorList.entries)
      entry.generate()
  }

  def duplicate() = {
    import Layer._

    all /* dummy */

//    for (entry <- GeneratorList.entries)
//      entry.duplicateFromTo(L2, L3)
//
//    ME_XXX.duplicateFromTo(IR, L2)
  }
}
