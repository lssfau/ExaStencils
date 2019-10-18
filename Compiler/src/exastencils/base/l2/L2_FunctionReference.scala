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

package exastencils.base.l2

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.prettyprinting._

/// L2_FunctionReference

trait L2_FunctionReference extends L2_Node with L2_Progressable with PrettyPrintable {
  def name : String
  def returnType : L2_Datatype
  override def progress : L3_FunctionReference
}

/// L2_PlainFunctionReference

trait L2_PlainFunctionReference extends L2_FunctionReference {
  override def prettyprint(out : PpStream) = out << name
}

/// L2_LeveledFunctionReference

trait L2_LeveledFunctionReference extends L2_FunctionReference {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}

/// L2_UnresolvedFunctionReference

case class L2_UnresolvedFunctionReference(
    var name : String,
    var level : Option[L2_LevelSpecification],
    var offset : Option[L2_ConstIndex]) extends L2_FunctionReference {

  override def returnType = L2_UnknownDatatype

  override def prettyprint(out : PpStream) = {
    out << name
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation(L3_UnresolvedFunctionReference(name, L2_ProgressOption(level)(_.progress), L2_ProgressOption(offset)(_.progress)))
}
