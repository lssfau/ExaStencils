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

package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// L4_FunctionTemplate

object L4_FunctionTemplate {
  def apply(name : String, datatype : L4_Datatype, templateArgs : List[String], functionArgs : List[L4_Function.Argument], statements : List[L4_Statement]) =
    new L4_FunctionTemplate(name, datatype, templateArgs.to[ListBuffer], functionArgs.to[ListBuffer], statements.to[ListBuffer])
}

case class L4_FunctionTemplate(
    var name : String,
    var datatype : L4_Datatype,
    var templateArgs : ListBuffer[String],
    var functionArgs : ListBuffer[L4_Function.Argument],
    var statements : ListBuffer[L4_Statement]) extends L4_Statement {

  override def prettyprint(out : PpStream) = {
    out << "FunctionTemplate " << name << " < " << templateArgs.mkString(", ") << " > ( "
    out <<< (functionArgs, ", ") << " )" << " : " << datatype << " {\n"
    out <<< (statements, "\n")
    out << "\n}"
  }

  override def progress = Logger.error("Trying to progress L4_FunctionTemplate; unsupported")
}
