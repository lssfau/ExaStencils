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

import exastencils.datastructures._
import exastencils.prettyprinting.PrintEnvironment.PrintEnvironment

/// FilePrettyPrintable

trait FilePrettyPrintable {
  def printToFile() : Unit
}

/// PrintToFile

object PrintToFile extends DefaultStrategy("Prettyprint all file-prettyprintable nodes") {
  this += new Transformation("Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile()
      printable
  })
}

/// PrintEnvironment

object PrintEnvironment extends Enumeration {
  type PrintEnvironment = Value
  val L1 = Value
  val L2 = Value
  val L3 = Value
  val L4 = Value
  val CPP = Value
  val CUDA = Value
}

/// PrettyPrintable

trait PrettyPrintable {
  def prettyprint(out : PpStream) : Unit

  final def prettyprint : String = prettyprint()
  // FIXME: HACK: allow usage of prettyprint without braces... -.-
  final def prettyprint(env : PrintEnvironment = PrintEnvironment.CPP) : String = {
    val out = new PpStream(env)
    prettyprint(out)
    out.toString()
  }
}
