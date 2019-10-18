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

import exastencils.util.Evaluator

/// CodeWrapper

case class CodeWrapper(var expression : String) {
  val defImports = "import exastencils.config.Knowledge._\n" + "import exastencils.config.Settings._\n" + "import exastencils.config.Platform._\n"

  def print() : String = "CodeInjection ( " + expression + ")"
  def eval[T]() : T = Evaluator[T](defImports + expression)
}
