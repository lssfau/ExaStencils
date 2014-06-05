package exastencils.primitives

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.util._

abstract class FragCommMember() extends Expression {
  override def cpp : String = "curFragment." + resolveName

  def resolveName : String
  def resolveDataType : Datatype
  def resolveDefValue : Option[Expression] = None

  def getDeclaration(numNeighbors : Int) : VariableDeclarationStatement = {
    new VariableDeclarationStatement(ArrayDatatype(resolveDataType, numNeighbors), resolveName)
  }

  def getCtor(neighIdx : Expression) : Option[Statement] = {
    if (resolveDefValue.isDefined)
      Some(AssignmentStatement(new ArrayAccess(resolveName, neighIdx), resolveDefValue.get))
    else
      None
  }

  def getDtor(neighIdx : Expression) : Option[Statement] = None
}

abstract class FragCommMemberWithPostfix extends FragCommMember {
  var field : Field
  def resolvePostfix = {
    if (Knowledge.comm_sepCommStructsPerField)
      s"_${field.identifier}"
    else
      ""
  }
}

case class FragMember_ReqOutstanding(var field : Field, var direction : String, var neighIdx : Expression) extends FragCommMemberWithPostfix {
  override def cpp : String = super.cpp + s"[${neighIdx.cpp}]"

  override def resolveName = s"reqOutstanding_${direction}" + resolvePostfix
  override def resolveDataType = new BooleanDatatype
  override def resolveDefValue = Some(false)
}

case class FragMember_MpiRequest(var field : Field, var direction : String, var neighIdx : Expression) extends FragCommMemberWithPostfix {
  override def cpp : String = super.cpp + s"[${neighIdx.cpp}]"

  override def resolveName = s"mpiRequest_${direction}" + resolvePostfix
  override def resolveDataType = "MPI_Request"
}

object FragMember_TmpBuffer {
  var sizes : Map[(String, Int), Int] = Map()
  def update(name : String, size : Int, neighIdx : Int) = {
    sizes += ((name, neighIdx) -> (size max sizes.getOrElse((name, neighIdx), 0)))
  }
}

case class FragMember_TmpBuffer(var field : Field, var direction : String, var size : Expression, var neighIdx : Expression) extends FragCommMemberWithPostfix {
  import FragMember_TmpBuffer._
  update(resolveName, SimplifyExpression.evalIntegral(size).toInt, SimplifyExpression.evalIntegral(neighIdx).toInt)

  override def cpp : String = super.cpp + s"[${neighIdx.cpp}]"

  override def resolveName = s"buffer_${direction}" + resolvePostfix
  override def resolveDataType = new PointerDatatype(field.dataType)
  override def resolveDefValue = Some(0)

  override def getDtor(neighIdx : Expression) : Option[Statement] = {
    Some(new ConditionStatement(new ArrayAccess(resolveName, neighIdx),
      ListBuffer[Statement](
        "delete []" ~~ new ArrayAccess(resolveName, neighIdx),
        new AssignmentStatement(new ArrayAccess(resolveName, neighIdx), 0))))
  }
}
