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

package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_FunctionTemplate
import exastencils.prettyprinting._

/// L3_FunctionTemplate

object L3_FunctionTemplate {
  def apply(name : String, datatype : Option[L3_Datatype], templateArgs : List[String],
      functionArgs : Option[List[L3_Function.Argument]], statements : List[L3_Statement]) : L3_FunctionTemplate =
    L3_FunctionTemplate(name, datatype.getOrElse(L3_UnitDatatype), templateArgs.to[ListBuffer],
      functionArgs.getOrElse(List()).to[ListBuffer], statements.to[ListBuffer])
}

case class L3_FunctionTemplate(
    var name : String,
    var datatype : L3_Datatype,
    var templateArgs : ListBuffer[String],
    var functionArgs : ListBuffer[L3_Function.Argument],
    var statements : ListBuffer[L3_Statement]) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "FunctionTemplate " << name << " < " << templateArgs.mkString(", ") << " > ( "
    out <<< (functionArgs, ", ") << " )" << " : " << datatype << " {\n"
    out <<< (statements, "\n")
    out << "\n}"
  }

  override def progress = ProgressLocation(L4_FunctionTemplate(name, datatype.progress, templateArgs, functionArgs.map(_.progress), statements.map(_.progress)))
}
