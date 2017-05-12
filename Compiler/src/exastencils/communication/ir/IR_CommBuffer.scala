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

  override def resolveDatatype() = IR_PointerDatatype(field.resolveBaseDatatype)
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
