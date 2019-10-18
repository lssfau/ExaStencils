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

import scala.collection.mutable.ListBuffer

/// Variability

abstract class Variability {
  def print() : String
}

/// VariabilitiesFromList

object VariabilitiesFromList {
  def apply(name : String, values : List[Any]) = new VariabilitiesFromList(name, values.to[ListBuffer])
}

case class VariabilitiesFromList(var name : String, var values : ListBuffer[Any]) extends Variability {
  override def print() : String = name + " => { " + values.mkString(", ") + " }"
}

/// VariabilitiesFromLambda

case class VariabilitiesFromLambda(var name : String, var expression : CodeWrapper) extends Variability {
  override def print() : String = name + " => " + expression.print()
}
