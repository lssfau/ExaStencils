package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class CommunicationFunctions() extends FunctionCollection("CommFunctions/CommFunctions",
  ListBuffer("cmath"),
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "MultiGrid/MultiGrid.h")) {

  if (Knowledge.useMPI)
    externalDependencies += "mpi.h"
  if (Knowledge.opt_vectorize)
    externalDependencies += "immintrin.h"
  if (Knowledge.opt_vectorize || Knowledge.poly_optLevel_fine > 0)
    externalDependencies += "algorithm"
}

case class SetIterationOffset(var location : Expression, var domain : Expression, var fragment : Expression) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = SetIterationOffset\n"

  override def expand : Output[SwitchStatement] = {
    var cases : ListBuffer[CaseStatement] = ListBuffer()

    for (neigh <- Fragment.neighbors) {
      // neighbor directions are always 3D vectors; invalid directions are not part of the given collection
      neigh.dir match {
        case Array(-1, 0, 0) => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 0), 0))
        case Array(1, 0, 0)  => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 0), 0))
        case Array(0, -1, 0) => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 1), 0))
        case Array(0, 1, 0)  => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 1), 0))
        case Array(0, 0, -1) => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 2), 0))
        case Array(0, 0, 1)  => cases += new CaseStatement(neigh.index, AssignmentStatement(ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 2), 0))
        case _               =>
      }
    }

    SwitchStatement(location, cases)
  }
}

case class ConnectLocalElement() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ConnectLocalElement\n"
  override def prettyprint_decl : String = prettyprint

  override def expand : Output[FunctionStatement] = {
    FunctionStatement(new UnitDatatype(), s"connectLocalElement",
      ListBuffer(
        VariableAccess("localFragId", Some("size_t")),
        VariableAccess("localNeighId", Some("size_t")),
        VariableAccess("location", Some(IntegerDatatype())),
        VariableAccess("domain", Some(IntegerDatatype()))),
      ListBuffer[Statement](
        AssignmentStatement(iv.NeighborIsValid("domain", "location", "localFragId"), true),
        AssignmentStatement(iv.NeighborIsRemote("domain", "location", "localFragId"), false),
        AssignmentStatement(iv.NeighborFragLocalId("domain", "location", "localFragId"), "localNeighId"),
        SetIterationOffset("location", "domain", "localFragId")))
  }
}

case class ConnectRemoteElement() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = ConnectRemoteElement\n"
  override def prettyprint_decl : String = prettyprint

  override def expand : Output[FunctionStatement] = {
    FunctionStatement(new UnitDatatype(), s"connectRemoteElement",
      ListBuffer(
        VariableAccess("localFragId", Some("size_t")),
        VariableAccess("localNeighId", Some("size_t")),
        VariableAccess("remoteRank", Some(IntegerDatatype())),
        VariableAccess("location", Some(IntegerDatatype())),
        VariableAccess("domain", Some(IntegerDatatype()))),
      ListBuffer[Statement](
        AssignmentStatement(iv.NeighborIsValid("domain", "location", "localFragId"), true),
        AssignmentStatement(iv.NeighborIsRemote("domain", "location", "localFragId"), true),
        AssignmentStatement(iv.NeighborFragLocalId("domain", "location", "localFragId"), "localNeighId"),
        AssignmentStatement(iv.NeighborRemoteRank("domain", "location", "localFragId"), "remoteRank"),
        SetIterationOffset("location", "domain", "localFragId")))
  }
}
