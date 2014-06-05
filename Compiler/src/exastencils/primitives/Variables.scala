package exastencils.primitives

import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class FragCommMember(var name : String, var field : Field, var direction : String) extends Expression {
  override def cpp : String = "curFragment." + resolveName

  def getDeclaration(numNeighbors : Int) : VariableDeclarationStatement = {
    new VariableDeclarationStatement(ArrayDatatype(resolveDataType, numNeighbors), resolveName)
  }

  def getCtor(neighIdx : Expression) : Option[Statement] = {
    if (resolveDefValue.isDefined)
      Some(AssignmentStatement(new ArrayAccess(resolveName, neighIdx), resolveDefValue.get))
    else
      None
  }

  def resolveName : String = {
    var ret = name match {
      case "reqOutstanding" => s"reqOutstanding_${direction}"
      case "mpiRequest"     => s"mpiRequest_${direction}"
      case _                => s"UNRECOGNIZED VARIABLE name"
    }

    if (Knowledge.comm_sepCommStructsPerField)
      ret += s"_${field.identifier}"

    ret
  }

  def resolveDataType : Datatype = {
    name match {
      case "reqOutstanding" => new BooleanDatatype
      case "mpiRequest"     => "MPI_Request"
      case _                => new UnitDatatype
    }
  }

  def resolveDefValue : Option[Expression] = {
    name match {
      case "reqOutstanding" => Some(false)
      case "mpiRequest"     => None
      case _                => None
    }
  }
}
