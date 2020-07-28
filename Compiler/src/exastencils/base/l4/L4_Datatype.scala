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

package exastencils.base.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir._
import exastencils.prettyprinting._

/// L4_Datatype

trait L4_Datatype extends L4_Node with PrettyPrintable with L4_Progressable {
  override def progress : IR_Datatype

  def dimensionality : Int
  def getSizeArray : Array[Int]
  def resolveBaseDatatype : L4_Datatype
  def resolveDeclType : L4_Datatype
  def resolveFlattendSize : Int
  def typicalByteSize : Int
}

/// L4_UnknownDatatype

case object L4_UnknownDatatype extends L4_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "???"
  override def progress = ProgressLocation(IR_UnknownDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L4_Datatype = this
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// special data types

case class L4_SpecialDatatype(typeName : String) extends L4_Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def progress = ProgressLocation(IR_SpecialDatatype(typeName))

  // unknown
  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : L4_Datatype = this
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case object L4_UnitDatatype extends L4_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Unit"
  override def progress = ProgressLocation(IR_UnitDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L4_Datatype = this
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// scalar data types

trait L4_ScalarDatatype extends L4_Datatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L4_Datatype = this
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = 1
}

case object L4_BooleanDatatype extends L4_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Boolean"
  override def progress = ProgressLocation(IR_BooleanDatatype)

  override def typicalByteSize = 1
}

case object L4_IntegerDatatype extends L4_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Integer"
  override def progress = ProgressLocation(IR_IntegerDatatype)

  override def typicalByteSize = 4
}

case object L4_RealDatatype extends L4_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Real"
  override def progress = ProgressLocation(IR_RealDatatype) // TODO: print warning here?

  override def typicalByteSize = ???
}

case object L4_FloatDatatype extends L4_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "Float"
  override def progress = ProgressLocation(IR_FloatDatatype)

  override def typicalByteSize = 4
}

case object L4_DoubleDatatype extends L4_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "Double"
  override def progress = ProgressLocation(IR_DoubleDatatype)

  override def typicalByteSize = 8
}

case object L4_CharDatatype extends L4_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def progress = ProgressLocation(IR_CharDatatype)

  override def typicalByteSize = 1
}

/// standard data types

case object L4_StringDatatype extends L4_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "String"
  override def progress = ProgressLocation(IR_StringDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L4_Datatype = L4_CharDatatype
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class L4_ComplexDatatype(datatype : L4_Datatype) extends L4_Datatype {
  override def prettyprint(out : PpStream) = { out << "Complex<" << datatype << '>' }
  override def progress = ProgressLocation(IR_ComplexDatatype(datatype.progress))

  // TODO: treat like a vec2 or like a struct?

  override def dimensionality : Int = 0 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array() ++ datatype.getSizeArray
  override def resolveBaseDatatype : L4_Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : L4_Datatype = this
  override def resolveFlattendSize : Int = 1 * datatype.resolveFlattendSize
  override def typicalByteSize = 2 * datatype.typicalByteSize
}
