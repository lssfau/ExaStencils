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

package configEvaluator

class Printer extends java.io.StringWriter {
  def <<(s : String) = { write(s); this }
  def <<<(s : String) = { write(s + "\n"); this }

  def printToFile(filename : String) = {
    val content = this.toString

    if (!new java.io.File(filename).exists) {
      println("Creating " + filename)

      val file = new java.io.File(filename)
      if (!file.getParentFile.exists()) file.getParentFile.mkdirs()

      val outFile = new java.io.FileWriter(filename)
      outFile.write(content)
      outFile.close()
    } else if (content != scala.io.Source.fromFile(filename).mkString) {
      println("Updating " + filename)

      val outFile = new java.io.FileWriter(filename)
      outFile.write(content)
      outFile.close()
    } else {
      println("Skipping " + filename)
    }
  }
}
