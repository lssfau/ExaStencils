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

package exastencils.util.l3

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L3_ReplaceExpressions

object L3_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : L3_Expression = L3_NullExpression
  var replacement : L3_Expression = L3_NullExpression

  this += new Transformation("Search and replace", {
    case e : L3_Expression if e == toReplace => Duplicate(replacement)
    //case e : L3_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : L3_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// L3_ReplaceIntWithReal

object L3_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L3_IntegerConstant(c) => L3_RealConstant(c)
  })
}

/// L3_ReplaceRealWithInt

object L3_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L3_RealConstant(c) => L3_IntegerConstant(c.toInt)
  })
}
