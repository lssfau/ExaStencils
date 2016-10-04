package exastencils.datastructures.ir.iv

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.field.ir.IR_Field
import exastencils.knowledge._
import exastencils.prettyprinting._

abstract class CommVariable extends IR_InternalVariable(Knowledge.comm_sepDataByFragment, false, Knowledge.comm_useFieldArrays, Knowledge.comm_useLevelArrays, Knowledge.comm_useNeighborArrays) {
  override def usesFragmentArrays : Boolean = Knowledge.comm_useFragmentArrays
  override def usesDomainArrays : Boolean = Knowledge.comm_useDomainArrays
  override def usesFieldArrays : Boolean = Knowledge.comm_useFieldArrays
  override def usesLevelArrays : Boolean = Knowledge.comm_useLevelArrays
  override def usesNeighborArrays : Boolean = Knowledge.comm_useNeighborArrays
}

case class RemoteReqOutstanding(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"remoteReqOutstanding_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class LocalReqOutstanding(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localReqOutstanding_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class MpiRequest(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"mpiRequest_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = "MPI_Request"
}

case class LocalCommReady(var field : IR_Field, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommReady" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_VolatileDatatype(IR_BooleanDatatype)
  override def resolveDefValue = Some(false)
}

case class LocalCommDone(var field : IR_Field, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommDone" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_VolatileDatatype(IR_BooleanDatatype)
  override def resolveDefValue = Some(false)
}

/// communication buffer handling

abstract class AbstractTmpBuffer extends CommVariable {
  var field : IR_Field
  var direction : String
  var size : IR_Expression
  var neighIdx : IR_Expression
  var fragmentIdx : IR_Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveDatatype = new IR_PointerDatatype(field.resolveBaseDatatype)
  override def resolveDefValue = Some(0)

  override def getDtor() : Option[IR_Statement] = {
    val ptrExpr = resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)
    Some(wrapInLoops(
      IR_IfCondition(ptrExpr,
        ListBuffer[IR_Statement](
          IR_ArrayFree(ptrExpr),
          new IR_Assignment(ptrExpr, 0)))))
  }
}

case class TmpBufferBasePtr(override var field : IR_Field, override var direction : String, override var size : IR_Expression, override var neighIdx : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends AbstractTmpBuffer {
  override def resolveName = s"buffer_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint) + "_base"

}

case class TmpBuffer(override var field : IR_Field, override var direction : String, override var size : IR_Expression, override var neighIdx : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends AbstractTmpBuffer {
  def basePtr = TmpBufferBasePtr(field, direction, size, neighIdx, fragmentIdx)

  override def resolveName = s"buffer_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)

  override def getDtor() : Option[IR_Statement] = {
    if (Knowledge.data_alignTmpBufferPointers) {
      var access = resolveAccess(resolveName, IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
      Some(wrapInLoops(new IR_Assignment(access, 0)))
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

case class TmpBufferIterator(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"tmpBufferIndex_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some(0)
}
