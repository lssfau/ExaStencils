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

package exastencils.base.l1

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.prettyprinting._

/// L1_Datatype

trait L1_Datatype extends L1_Node with PrettyPrintable with L1_Progressable {
  override def progress : L2_Datatype

  def dimensionality : Int
  def getSizeArray : Array[Int]
  def resolveBaseDatatype : L1_Datatype
  def resolveDeclType : L1_Datatype
  def resolveFlattendSize : Int
  def typicalByteSize : Int
}

/// L1_UnknownDatatype

case object L1_UnknownDatatype extends L1_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "???"
  override def progress = ProgressLocation(L2_UnknownDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L1_Datatype = this
  override def resolveDeclType : L1_Datatype = this
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// special data types

case class L1_SpecialDatatype(typeName : String) extends L1_Datatype {
  override def prettyprint(out : PpStream) : Unit = out << typeName
  override def progress = ProgressLocation(L2_SpecialDatatype(typeName))

  // unknown
  override def dimensionality : Int = ???
  override def getSizeArray : Array[Int] = ???
  override def resolveBaseDatatype : L1_Datatype = this
  override def resolveDeclType : L1_Datatype = this
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case object L1_UnitDatatype extends L1_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Unit"
  override def progress = ProgressLocation(L2_UnitDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L1_Datatype = this
  override def resolveDeclType : L1_Datatype = this
  override def resolveFlattendSize : Int = 0
  override def typicalByteSize = 0
}

/// scalar data types

trait L1_ScalarDatatype extends L1_Datatype {
  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L1_Datatype = this
  override def resolveDeclType : L1_Datatype = this
  override def resolveFlattendSize : Int = 1
}

case object L1_BooleanDatatype extends L1_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Boolean"
  override def progress = ProgressLocation(L2_BooleanDatatype)

  override def typicalByteSize = 1
}

case object L1_IntegerDatatype extends L1_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Integer"
  override def progress = ProgressLocation(L2_IntegerDatatype)

  override def typicalByteSize = 2
}

case object L1_RealDatatype extends L1_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "Real"
  override def progress = ProgressLocation(L2_RealDatatype) // TODO: print warning here?

  override def typicalByteSize = ???
}

case object L1_FloatDatatype extends L1_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "Float"
  override def progress = ProgressLocation(L2_FloatDatatype)

  override def typicalByteSize = 2
}

case object L1_DoubleDatatype extends L1_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "Double"
  override def progress = ProgressLocation(L2_DoubleDatatype)

  override def typicalByteSize = 8
}

case object L1_CharDatatype extends L1_ScalarDatatype {
  exastencils.core.Duplicate.registerConstant(this)

  override def prettyprint(out : PpStream) : Unit = out << "char"
  override def progress = ProgressLocation(L2_CharDatatype)

  override def typicalByteSize = 1
}

/// standard data types

case object L1_StringDatatype extends L1_Datatype {
  exastencils.core.Duplicate.registerConstant(this)

  // TODO: ensure parser support
  override def prettyprint(out : PpStream) : Unit = out << "String"
  override def progress = ProgressLocation(L2_StringDatatype)

  override def dimensionality : Int = 0
  override def getSizeArray : Array[Int] = Array()
  override def resolveBaseDatatype : L1_Datatype = L1_CharDatatype
  override def resolveDeclType : L1_Datatype = this
  override def resolveFlattendSize : Int = ???
  override def typicalByteSize = ???
}

case class L1_ComplexDatatype(datatype : L1_Datatype) extends L1_Datatype {
  override def prettyprint(out : PpStream) = { out << "Complex<" << datatype << '>' }
  override def progress = ProgressLocation(L2_ComplexDatatype(datatype.progress))

  // TODO: treat like a vec2 or like a struct?

  override def dimensionality : Int = 0 + datatype.dimensionality
  override def getSizeArray : Array[Int] = Array() ++ datatype.getSizeArray
  override def resolveBaseDatatype : L1_Datatype = datatype.resolveBaseDatatype
  override def resolveDeclType : L1_Datatype = this
  override def resolveFlattendSize : Int = 1 * datatype.resolveFlattendSize
  override def typicalByteSize = 2 * datatype.typicalByteSize
}
