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

package exastencils.base.l3

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.prettyprinting._

/// L3_Datatype

trait L3_Datatype extends L3_Node with PrettyPrintable with L3_Progressable {
  override def progress : L4_Datatype

  def dimensionality : Int
  def getSizeArray : Array[Int]
  def resolveBaseDatatype : L3_Datatype
  def resolveDeclType : L3_Datatype
  def resolveFlattendSize : Int
  def typicalByteSize : Int
}

/// L3_UnknownDatatype

case object L3_UnknownDatatype extends L3_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "???"
  override def progress = ProgressLocation(L4_UnknownDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L3_Datatype = this
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// special data types

case class L3_SpecialDatatype(typeName : String) extends L3_Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def progress = ProgressLocation(L4_SpecialDatatype(typeName))

  // unknown
  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : L3_Datatype = this
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case object L3_UnitDatatype extends L3_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Unit"
  override def progress = ProgressLocation(L4_UnitDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L3_Datatype = this
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// scalar data types

trait L3_ScalarDatatype extends L3_Datatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L3_Datatype = this
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = 1
}

case object L3_BooleanDatatype extends L3_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Boolean"
  override def progress = ProgressLocation(L4_BooleanDatatype)

  override def typicalByteSize = 1
}

case object L3_IntegerDatatype extends L3_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Integer"
  override def progress = ProgressLocation(L4_IntegerDatatype)

  override def typicalByteSize = 3
}

case object L3_RealDatatype extends L3_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Real"
  override def progress = ProgressLocation(L4_RealDatatype) // TODO: print warning here?

  override def typicalByteSize = ???
}

case object L3_FloatDatatype extends L3_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "Float"
  override def progress = ProgressLocation(L4_FloatDatatype)

  override def typicalByteSize = 3
}

case object L3_DoubleDatatype extends L3_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "Double"
  override def progress = ProgressLocation(L4_DoubleDatatype)

  override def typicalByteSize = 8
}

case object L3_CharDatatype extends L3_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def progress = ProgressLocation(L4_CharDatatype)

  override def typicalByteSize = 1
}

/// standard data types

case object L3_StringDatatype extends L3_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "String"
  override def progress = ProgressLocation(L4_StringDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L3_Datatype = L3_CharDatatype
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class L3_ComplexDatatype(datatype : L3_Datatype) extends L3_Datatype {
  override def prettyprint(out : PpStream) = { out << "Complex<" << datatype << '>' }
  override def progress = ProgressLocation(L4_ComplexDatatype(datatype.progress))

  // TODO: treat like a vec2 or like a struct?

  override def dimensionality : Int = 0 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array() ++ datatype.getSizeArray
  override def resolveBaseDatatype : L3_Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : L3_Datatype = this
  override def resolveFlattendSize : Int = 1 * datatype.resolveFlattendSize
  override def typicalByteSize = 2 * datatype.typicalByteSize
}
