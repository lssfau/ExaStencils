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

package exastencils.util.l4

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L4_ReplaceExpressions

object L4_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : L4_Expression = L4_NullExpression
  var replacement : L4_Expression = L4_NullExpression

  this += new Transformation("Search and replace", {
    case e : L4_Expression if e == toReplace => Duplicate(replacement)
    //case e : L4_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : L4_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// L4_ReplaceIntWithReal

object L4_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L4_IntegerConstant(c) => L4_RealConstant(c)
  })
}

/// L4_ReplaceRealWithInt

object L4_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L4_RealConstant(c) => L4_IntegerConstant(c.toInt)
  })
}
