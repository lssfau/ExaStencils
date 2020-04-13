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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_TensorExpression2
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.prettyprinting.PrettyPrintable


case class L4_TensorEntry(var index : List[Int], var coefficient : L4_Expression) extends L4_Node with L4_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << "[" << index.foreach(out << _ << ",") << "]" << " => " << coefficient.prettyprint(out)
  override def progress = null

  def convertConstants(dt : L4_Datatype) : Unit = (coefficient, dt) match {
      case (c : L4_IntegerConstant, L4_RealDatatype | L4_FloatDatatype | L4_DoubleDatatype) => L4_RealConstant(c.v)
      case (c : L4_RealConstant, L4_IntegerDatatype)                                        => L4_IntegerConstant(c.v.toInt)
      case (_,_)                                                                            => coefficient
    }
}
/// L4_TensorExpression

case class L4_TensorExpression2(
    var datatype : Option[L4_Datatype],
    var expressions : List[L4_TensorEntry]) extends L4_Expression {

  def prettyprint(out : PpStream) = {
    out << "{ "
    expressions.foreach(out << "{ " << (_, ", ") << " }, ")
    out.removeLast(", ".length)
    out << " }"
  }

  override def progress = ProgressLocation(IR_TensorExpression2(L4_ProgressOption(datatype)(_.progress), progressEntrys(expressions)))

  def progressEntrys(input: List[L4_TensorEntry]) : Array[IR_Expression] = {
    val flattenIn = input.toArray
    if (flattenIn.length > 9) {
      Logger.error("To much tensor entries!")
    }
    var test_entry_dim = true
    for (i <- 0 until 9) {
      if (flattenIn(0).index.length != 2) test_entry_dim = false
    }
    if (!test_entry_dim) {
      Logger.error("Tensor entry has wrong dimension")
    }
    val eval : Array[Boolean] = Array.fill(flattenIn.length){false}
    val exp  = new Array[IR_Expression](9)
    for (i <- 0 until 9){
      if ((flattenIn(i).index(0) + flattenIn(i).index(1) * 3) > exp.length){
        if (eval(flattenIn(i).index(0) + flattenIn(i).index(1) * 3) == false){
          eval(flattenIn(i).index(0) + flattenIn(i).index(1) * 3) = true
          exp(flattenIn(i).index(0) + flattenIn(i).index(1) * input.length) = flattenIn(i).coefficient.progress
        } else {
          Logger.error("Tensor index was double set")
        }
      } else {
        Logger.error("Tensor index is not available")
      }
    }
    for (i <- 0 until 9) {
      if (eval(i) == false) {
        exp(i) = L4_RealConstant(0.0).progress
      }
    }
    exp
  }

  def dim = 2
  def isConstant = expressions.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions.foreach(_.convertConstants(dt))
  }
}
