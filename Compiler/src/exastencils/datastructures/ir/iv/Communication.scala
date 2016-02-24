package exastencils.datastructures.ir.iv

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

abstract class CommVariable extends InternalVariable(Knowledge.comm_sepDataByFragment, false, Knowledge.comm_useFieldArrays, Knowledge.comm_useLevelArrays, Knowledge.comm_useNeighborArrays) {
  override def usesFragmentArrays : Boolean = Knowledge.comm_useFragmentArrays
  override def usesDomainArrays : Boolean = Knowledge.comm_useDomainArrays
  override def usesFieldArrays : Boolean = Knowledge.comm_useFieldArrays
  override def usesLevelArrays : Boolean = Knowledge.comm_useLevelArrays
  override def usesNeighborArrays : Boolean = Knowledge.comm_useNeighborArrays
}

case class RemoteReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"remoteReqOutstanding_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class LocalReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localReqOutstanding_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class MpiRequest(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = "MPI_Request"
}

case class LocalCommReady(var field : Field, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommReady" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = VolatileDatatype(BooleanDatatype)
  override def resolveDefValue = Some(false)
}

case class LocalCommDone(var field : Field, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommDone" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = VolatileDatatype(BooleanDatatype)
  override def resolveDefValue = Some(false)
}

/// communication buffer handling

abstract class AbstractTmpBuffer extends CommVariable {
  var field : Field
  var direction : String
  var size : Expression
  var neighIdx : Expression
  var fragmentIdx : Expression

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveDataType = new PointerDatatype(field.resolveBaseDatatype)
  override def resolveDefValue = Some(0)

  override def getDtor() : Option[Statement] = {
    val ptrExpr = resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)
    Some(wrapInLoops(
      new ConditionStatement(ptrExpr,
        ListBuffer[Statement](
          FreeStatement(ptrExpr),
          new AssignmentStatement(ptrExpr, 0)))))
  }
}

case class TmpBufferBasePtr(override var field : Field, override var direction : String, override var size : Expression, override var neighIdx : Expression, override var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractTmpBuffer {
  override def resolveName = s"buffer_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint) + "_base"

}

case class TmpBuffer(override var field : Field, override var direction : String, override var size : Expression, override var neighIdx : Expression, override var fragmentIdx : Expression = LoopOverFragments.defIt) extends AbstractTmpBuffer {
  def basePtr = TmpBufferBasePtr(field, direction, size, neighIdx, fragmentIdx)

  override def resolveName = s"buffer_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)

  override def getDtor() : Option[Statement] = {
    if (Knowledge.data_alignTmpBufferPointers) {
      var access = resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt)
      Some(wrapInLoops(new AssignmentStatement(access, 0)))
    } else {
      super.getDtor()
    }
  }

  override def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, Statement], dtors : HashMap[String, Statement]) = {
    declarations += (resolveName -> getDeclaration)
    ctors += (resolveName -> getCtor().get)
    dtors += (resolveName -> getDtor().get)

    if (Knowledge.data_alignTmpBufferPointers)
      basePtr.registerIV(declarations, ctors, dtors)
  }
}

case class TmpBufferIterator(var field : Field, var direction : String, var neighIdx : Expression, var fragmentIdx : Expression = LoopOverFragments.defIt) extends CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"tmpBufferIndex_${direction}" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDataType = IntegerDatatype
  override def resolveDefValue = Some(0)
}