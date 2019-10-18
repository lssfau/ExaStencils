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

package exastencils.util.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger

/// L2_ReplaceExpressions

object L2_ReplaceExpressions extends QuietDefaultStrategy("Replace something with something else") {
  var toReplace : L2_Expression = L2_NullExpression
  var replacement : L2_Expression = L2_NullExpression

  this += new Transformation("Search and replace", {
    case e : L2_Expression if e == toReplace => Duplicate(replacement)
    //case e : L2_Expression if e.toString == toReplace.toString           => Duplicate(replacement)
    case e : L2_Expression if e.prettyprint() == toReplace.prettyprint() =>
      Logger.warn(s"$e doesn't seem to be $toReplace")
      e
  }, false)
}

/// L2_ReplaceIntWithReal

object L2_ReplaceIntWithReal extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L2_IntegerConstant(c) => L2_RealConstant(c)
  })
}

/// L2_ReplaceRealWithInt

object L2_ReplaceRealWithInt extends QuietDefaultStrategy("Replace something with something else") {
  this += new Transformation("Search and replace", {
    case L2_RealConstant(c) => L2_IntegerConstant(c.toInt)
  })
}
