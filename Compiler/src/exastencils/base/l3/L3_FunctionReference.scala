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

package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_FunctionReference

trait L3_FunctionReference extends L3_Node with L3_Progressable with PrettyPrintable {
  def name : String
  def returnType : L3_Datatype
  override def progress : L4_FunctionReference
}

/// L3_PlainFunctionReference

trait L3_PlainFunctionReference extends L3_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// L3_LeveledFunctionReference

trait L3_LeveledFunctionReference extends L3_FunctionReference {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}

/// L3_UnresolvedFunctionReference

case class L3_UnresolvedFunctionReference(
    var name : String,
    var level : Option[L3_LevelSpecification],
    var offset : Option[L3_ConstIndex]) extends L3_FunctionReference {

  override def returnType = L3_UnknownDatatype

  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation(L4_UnresolvedFunctionReference(name, L3_ProgressOption(level)(_.progress), L3_ProgressOption(offset)(_.progress)))
}
