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

package exastencils.runner

import exastencils.core.UniversalSetter
import exastencils.util._

/// DerivedValue

abstract class DerivedParameter {
  def print() : String
  def apply() : Unit
}

/// DerivedValueWithAssign

case class DerivedParameterWithAssign(var name : String, var value : Any) extends DerivedParameter {
  def print() : String = name + " = " + value
  override def apply() : Unit = UniversalSetter(ResolveConfigCollection(name), name, resolveValue())

  def resolveValue() = {
    value match {
      case code : CodeWrapper => code.eval[Any]()
      case other              => other
    }
  }
}

/// DerivedValueWithAppend

case class DerivedParameterWithAppend(var name : String, var value : Any) extends DerivedParameter {
  def print() : String = name + " += " + value
  override def apply() : Unit = UniversalSetter(ResolveConfigCollection(name), name, resolveValue())

  def resolveValue() = {
    value match {
      case code : CodeWrapper => code.eval[Any]()
      case other              => other
    }
  }
}
