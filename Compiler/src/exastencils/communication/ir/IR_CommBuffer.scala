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

package exastencils.communication.ir

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_Field
import exastencils.prettyprinting.PpStream

/// IR_IV_AbstractCommBuffer

abstract class IR_IV_AbstractCommBuffer extends IR_IV_CommVariable {
  var field : IR_Field
  var direction : String
  var size : IR_Expression
  var neighIdx : IR_Expression
  var fragmentIdx : IR_Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveDatatype() = {
    // IR_ComplexDatatype should be a base datatype on one level with IR_DoubleDatatype
    if(field.layout.datatype.isInstanceOf[IR_ComplexDatatype]) IR_PointerDatatype(field.layout.datatype)
    else IR_PointerDatatype(field.resolveBaseDatatype)
  }
  override def resolveDefValue() = Some(0)

  override def getDtor() : Option[IR_Statement] = {
    val ptrExpr = resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)
    Some(wrapInLoops(
      IR_IfCondition(ptrExpr,
        ListBuffer[IR_Statement](
          IR_ArrayFree(ptrExpr),
          IR_Assignment(ptrExpr, 0)))))
  }
}

/// IR_IV_CommBufferBasePtr

case class IR_IV_CommBufferBasePtr(override var field : IR_Field, override var direction : String, override var size : IR_Expression, override var neighIdx : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractCommBuffer {
  override def resolveName() = s"buffer_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint) + "_base"

}

/// IR_IV_CommBuffer

case class IR_IV_CommBuffer(override var field : IR_Field, override var direction : String, override var size : IR_Expression, override var neighIdx : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractCommBuffer {
  def basePtr = IR_IV_CommBufferBasePtr(field, direction, size, neighIdx, fragmentIdx)

  override def resolveName() = s"buffer_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)


  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.data_alignTmpBufferPointers) {
      val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
      Some(wrapInLoops(IR_Assignment(access, 0)))
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, IR_VariableDeclaration], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignTmpBufferPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}

/// IR_IV_CommBufferIterator

case class IR_IV_CommBufferIterator(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"tmpBufferIndex_$direction" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(0)
}
