package exastencils.solver.l2

import exastencils.base.l2._
import exastencils.field.l2.L2_FieldCollection
import exastencils.prettyprinting._
import exastencils.solver.l3.L3_SolverForEqEntry

/// L2_SolverForEqEntry

case class L2_SolverForEqEntry(unknownName : String, eqName : String) extends L2_Node with L2_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << unknownName << " in " << eqName

  def getSolField(level : Int) = L2_FieldCollection.getByIdentifier(unknownName, level).get
  def getEq(level : Int) = L2_EquationCollection.getByIdentifier(eqName, level).get

  override def progress = L3_SolverForEqEntry(unknownName, eqName)
}
