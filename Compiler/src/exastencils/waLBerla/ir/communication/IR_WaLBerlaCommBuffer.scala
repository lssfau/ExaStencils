package exastencils.waLBerla.ir.communication

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_HasMessageDirection
import exastencils.communication.ir.IR_IV_AbstractCommBufferLike
import exastencils.communication.ir.IR_IV_CommBufferLike
import exastencils.config.Knowledge
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

/// IR_WaLBerlaAbstractCommBuffer

abstract class IR_WaLBerlaAbstractCommBuffer extends IR_WaLBerlaCommVariable with IR_IV_AbstractCommBufferLike {
  var field : IR_FieldLike
  var send : Boolean
  var size : IR_Expression
  var neighIdx : IR_Expression
  var concurrencyId : Int
  var indexOfRefinedNeighbor : Option[IR_Expression]
  var fragmentIdx : IR_Expression

  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  // IR_ComplexDatatype should be a base datatype on one level with IR_DoubleDatatype
  override def baseDatatype : IR_Datatype = if (field.layout.datatype.isInstanceOf[IR_ComplexDatatype])
    IR_PointerDatatype(field.layout.datatype)
  else
    IR_PointerDatatype(field.resolveBaseDatatype)

  override def resolveDefValue() = Some(0)

  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.refinement_enabled) {
      Some(wrapInLoops(
        IR_Scope((0 until Knowledge.refinement_maxFineNeighborsForCommAxis).map(i =>
          IR_IfCondition(resolveAccessOverWrappedLoops(i),
            ListBuffer[IR_Statement](
              IR_ArrayFree(resolveAccessOverWrappedLoops(i)),
              IR_Assignment(resolveAccessOverWrappedLoops(i), 0)))) : _*)))
    } else {
      val ptrExpr = super.resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_LoopOverNeighbors.defIt)

      Some(wrapInLoops(
        IR_IfCondition(ptrExpr,
          ListBuffer[IR_Statement](
            IR_ArrayFree(ptrExpr),
            IR_Assignment(ptrExpr, 0)))))
    }
  }
}

/// IR_WaLBerlaCommBufferBasePtr

case class IR_WaLBerlaCommBufferBasePtr(
    override var field : IR_FieldLike,
    override var send : Boolean,
    override var size : IR_Expression,
    override var neighIdx : IR_Expression,
    override var concurrencyId : Int,
    override var indexOfRefinedNeighbor : Option[IR_Expression],
    override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaAbstractCommBuffer {

  override def name = s"wbBuffer_${ direction }_${ concurrencyId }" + "_base"
}

/// IR_WaLBerlaCommBuffer

case class IR_WaLBerlaCommBuffer(
    override var field : IR_FieldLike,
    override var send : Boolean,
    override var size : IR_Expression,
    override var neighIdx : IR_Expression,
    override var concurrencyId : Int,
    override var indexOfRefinedNeighbor : Option[IR_Expression],
    override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaAbstractCommBuffer with IR_IV_CommBufferLike {

  def basePtr = IR_WaLBerlaCommBufferBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

  override def name = s"wbBuffer_${ direction }_${ concurrencyId }"

  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.data_alignTmpBufferPointers) {
      val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
      Some(wrapInLoops(IR_Assignment(access, 0)))
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : mutable.AbstractMap[String, IR_VariableDeclaration], ctors : mutable.AbstractMap[String, IR_Statement], dtors : mutable.AbstractMap[String, IR_Statement]) = {
    super.registerIV(declarations, ctors, dtors)

    if (Knowledge.data_alignTmpBufferPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}

/// IR_WaLBerlaCommBufferIterator

case class IR_WaLBerlaCommBufferIterator(
    var field : IR_FieldLike,
    var send : Boolean,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaCommVariable {

  override def isPrivate : Boolean = true

  override def name = s"wbTmpBufferIndex_${ direction }_${ concurrencyId }"

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def baseDatatype : IR_Datatype = IR_IntegerDatatype

  override def resolveDefValue() = Some(0)
}
