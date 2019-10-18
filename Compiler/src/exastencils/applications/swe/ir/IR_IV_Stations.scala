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

package exastencils.applications.swe.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.prettyprinting.PpStream

/// IR_IV_Stations

case class IR_IV_Stations(var i : IR_Expression, var j : IR_Expression) extends IR_InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(i, j)
  override def resolveName() = s"sweStations" + resolvePostfix("", "", "", "", "")
  override def resolveDatatype() = IR_ArrayDatatype(IR_DoubleDatatype, nStationsMax * dims)
  override def resolveDefValue() = Some(-99999) //TODO reasonable default value

  val nStationsMax = Knowledge.swe_stationsMax
  val dims = 2

  override def getCtor() : Option[IR_Statement] = {

    val i = IR_VariableAccess("i", IR_IntegerDatatype)
    val j = IR_VariableAccess("j", IR_IntegerDatatype)

    if (resolveDefValue().isDefined)
      Some(IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, nStationsMax), IR_PreIncrement(i),
        IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, dims), IR_PreIncrement(j),
          IR_Assignment(resolveAccess(i, j), resolveDefValue().get)
        )
      ))
    else
      None
  }

  def resolveAccess(i : IR_Expression, j : IR_Expression) = {
    IR_ArrayAccess(resolveName(), dims * i + j)
  }
}

/// IR_IV_StationsFragment

case class IR_IV_StationsFragment(var i : IR_Expression) extends IR_InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(i)
  override def resolveName() = s"sweStationsFragment" + resolvePostfix("", "", "", "", "")
  override def resolveDatatype() = IR_ArrayDatatype(IR_IntegerDatatype, nStationsMax)
  override def resolveDefValue() = Some(-1)

  val nStationsMax = Knowledge.swe_stationsMax

  override def getCtor() : Option[IR_Statement] = {

    val i = IR_VariableAccess("i", IR_IntegerDatatype)

    if (resolveDefValue().isDefined)
      Some(IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, nStationsMax), IR_PreIncrement(i),
        IR_Assignment(resolveAccess(i), resolveDefValue().get)
      ))
    else
      None
  }

  def resolveAccess(i : IR_Expression) = {
    IR_ArrayAccess(resolveName(), i)
  }
}

/// IR_IV_StationsId

// store station id according to IR_LoopOverDimensions.defIt
case class IR_IV_StationsId(var i : IR_Expression, var j : IR_Expression) extends IR_InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(i, j)
  override def resolveName() = s"sweStationsId" + resolvePostfix("", "", "", "", "")
  override def resolveDatatype() = IR_ArrayDatatype(IR_IntegerDatatype, nStationsMax * dims)
  override def resolveDefValue() = Some(-1)

  val nStationsMax = Knowledge.swe_stationsMax
  val dims = 2

  override def getCtor() : Option[IR_Statement] = {

    val i = IR_VariableAccess("i", IR_IntegerDatatype)
    val j = IR_VariableAccess("j", IR_IntegerDatatype)

    if (resolveDefValue().isDefined)
      Some(IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, nStationsMax), IR_PreIncrement(i),
        IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, dims), IR_PreIncrement(j),
          IR_Assignment(resolveAccess(i, j), resolveDefValue().get)
        )
      ))
    else
      None
  }

  def resolveAccess(i : IR_Expression, j : IR_Expression) = {
    IR_ArrayAccess(resolveName(), dims * i + j)
  }
}

/// IR_IV_StationsIsLower

case class IR_IV_StationsIsLower(var i : IR_Expression) extends IR_InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(i)
  override def resolveName() = s"sweStationsIsLower" + resolvePostfix("", "", "", "", "")
  override def resolveDatatype() = IR_ArrayDatatype(IR_BooleanDatatype, nStationsMax)
  override def resolveDefValue() = Some(true)

  val nStationsMax = Knowledge.swe_stationsMax

  override def getCtor() : Option[IR_Statement] = {

    val i = IR_VariableAccess("i", IR_IntegerDatatype)

    if (resolveDefValue().isDefined)
      Some(IR_ForLoop(IR_VariableDeclaration(i, 0), IR_Lower(i, nStationsMax), IR_PreIncrement(i),
        IR_Assignment(resolveAccess(i), resolveDefValue().get)
      ))
    else
      None
  }

  def resolveAccess(i : IR_Expression) = {
    IR_ArrayAccess(resolveName(), i)
  }
}
