package exastencils.solver.l1

import exastencils.base.ProgressLocation
import exastencils.base.l1._
import exastencils.field.l1.L1_FieldCollection
import exastencils.prettyprinting._
import exastencils.solver.l2.L2_SolverForEqEntry

/// L1_SolverForEqEntry

case class L1_SolverForEqEntry(unknownName : String, eqName : String) extends L1_Node with L1_Progressable with PrettyPrintable {
  override def prettyprint(out : PpStream) = out << unknownName << " in " << eqName

  def getSolField(level : Int) = L1_FieldCollection.getByIdentifier(unknownName, level).get
  def getEq(level : Int) = L1_EquationCollection.getByIdentifier(eqName, level).get

  override def progress = ProgressLocation(L2_SolverForEqEntry(unknownName, eqName))
}
