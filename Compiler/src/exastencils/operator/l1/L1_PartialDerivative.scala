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

package exastencils.operator.l1

import exastencils.base.l1.L1_Expression
import exastencils.grid.l1.L1_Localization
import exastencils.logger.Logger
import exastencils.parsers.l1.L1_ReservedSigns
import exastencils.prettyprinting.PpStream

/// L1_PartialDerivative

object L1_PartialDerivative {
  def apply(stringRep : String) = {
    val cases = stringRep.toCharArray.distinct.sorted
    if (cases.length > 1) Logger.error("Mixed partial derivatives are currently not supported")

    val dimAsChar = cases.head
    new L1_PartialDerivative(stringRep.count(_ == dimAsChar), L1_Localization.stringToDim(dimAsChar))
  }
}

case class L1_PartialDerivative(order : Int, dim : Int) extends L1_Expression {
  override def prettyprint(out : PpStream) = out << L1_ReservedSigns.partial._2 << "_{" << L1_Localization.dimToString(dim).toString * order << "}"
  override def progress = Logger.error("Trying to progress L1 partial derivative; unsupported")
}

/// L1_Laplace

case object L1_Laplace extends L1_Expression {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) = out << L1_ReservedSigns.capitalDelta._2
  override def progress = Logger.error("Trying to progress L1 Laplace; unsupported")
}
