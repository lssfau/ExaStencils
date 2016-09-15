package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class CommunicationFunctions() extends IR_FunctionCollection("CommFunctions/CommFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Matrix.h", "MultiGrid/MultiGrid.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled)
    internalDependencies += "KernelFunctions/KernelFunctions.h"
  if (Knowledge.opt_vectorize) {
    val header = Platform.simd_header
    if (header != null) externalDependencies += header
  }
}

case class SetIterationOffset(var location : IR_Expression, var domain : IR_Expression, var fragment : IR_Expression) extends IR_Statement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand : Output[IR_Switch] = {
    var cases : ListBuffer[IR_Case] = ListBuffer()

    for (neigh <- Fragment.neighbors) {
      // neighbor directions are always 3D vectors; invalid directions are not part of the given collection
      neigh.dir match {
        case Array(-1, 0, 0) => cases += IR_Case(neigh.index, IR_Assignment(IR_ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 0), 0))
        case Array(1, 0, 0)  => cases += IR_Case(neigh.index, IR_Assignment(IR_ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 0), 0))
        case Array(0, -1, 0) => cases += IR_Case(neigh.index, IR_Assignment(IR_ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 1), 0))
        case Array(0, 1, 0)  => cases += IR_Case(neigh.index, IR_Assignment(IR_ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 1), 0))
        case Array(0, 0, -1) => cases += IR_Case(neigh.index, IR_Assignment(IR_ArrayAccess(iv.IterationOffsetBegin(domain, fragment), 2), 0))
        case Array(0, 0, 1)  => cases += IR_Case(neigh.index, IR_Assignment(IR_ArrayAccess(iv.IterationOffsetEnd(domain, fragment), 2), 0))
        case _               =>
      }
    }

    IR_Switch(location, cases)
  }
}

case class ConnectLocalElement() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "connectLocalElement"

  override def expand : Output[IR_Function] = {
    IR_Function(IR_UnitDatatype, name,
      ListBuffer(
        IR_FunctionArgument("localFragId", IR_IntegerDatatype),
        IR_FunctionArgument("localNeighId", IR_IntegerDatatype),
        IR_FunctionArgument("location", IR_IntegerDatatype),
        IR_FunctionArgument("domain", IR_IntegerDatatype)),
      ListBuffer[IR_Statement](
        IR_Assignment(iv.NeighborIsValid("domain", "location", "localFragId"), true),
        IR_Assignment(iv.NeighborIsRemote("domain", "location", "localFragId"), false),
        IR_Assignment(iv.NeighborFragLocalId("domain", "location", "localFragId"), "localNeighId"),
        SetIterationOffset("location", "domain", "localFragId")))
  }
}

case class ConnectRemoteElement() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "connectRemoteElement"

  override def expand : Output[IR_Function] = {
    IR_Function(IR_UnitDatatype, name,
      ListBuffer(
        IR_FunctionArgument("localFragId", IR_IntegerDatatype),
        IR_FunctionArgument("localNeighId", IR_IntegerDatatype),
        IR_FunctionArgument("remoteRank", IR_IntegerDatatype),
        IR_FunctionArgument("location", IR_IntegerDatatype),
        IR_FunctionArgument("domain", IR_IntegerDatatype)),
      ListBuffer[IR_Statement](
        IR_Assignment(iv.NeighborIsValid("domain", "location", "localFragId"), true),
        IR_Assignment(iv.NeighborIsRemote("domain", "location", "localFragId"), true),
        IR_Assignment(iv.NeighborFragLocalId("domain", "location", "localFragId"), "localNeighId"),
        IR_Assignment(iv.NeighborRemoteRank("domain", "location", "localFragId"), "remoteRank"),
        SetIterationOffset("location", "domain", "localFragId")))
  }
}
