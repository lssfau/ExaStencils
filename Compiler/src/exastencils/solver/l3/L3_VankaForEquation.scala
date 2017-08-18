package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_ForLoop
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.l3.L3_FieldAccess
import exastencils.grid.l3._
import exastencils.logger.Logger

/// L3_VankaForEquation

object L3_VankaForEquation {
  def omega : L3_Expression = 0.8

  def generateLocalSolveForScalar(entry : L3_SolverForEqEntry, level : Int) = {
    // only one unknown => generate point-smoother

    val unknowns = ListBuffer[L3_Expression](L3_FieldAccess(entry.getSolField(level)))
    val equations = ListBuffer[L3_Equation](Duplicate(entry.getEq(level)))

    L3_LocalSolve(unknowns, equations, false, Some(omega), L3_FieldAccess(entry.getSolField(level)))
  }

  def generateLocalSolveForAligned(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // all unknowns share the same localization => simply use first as loop field

    val unknowns = ListBuffer[L3_Expression]()
    val equations = ListBuffer[L3_Equation]()

    for (entry <- entries) {
      unknowns += L3_FieldAccess(entry.getSolField(level))
      equations += Duplicate(entry.getEq(level))
    }

    L3_LocalSolve(unknowns, equations, false, Some(omega), L3_FieldAccess(entries.head.getSolField(level)))
  }

  def generateLocalSolveForMixed(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // multiple unknowns with varying localizations => look for cells

    val entryAtCell = entries.find(_.getSolField(level).localization == L3_AtCellCenter)
    if (entryAtCell.isEmpty) Logger.warn("Mixed localization system solvers are currently only possible if at least one unknown is cell-centered")

    val unknowns = ListBuffer[L3_Expression]()
    val equations = ListBuffer[L3_Equation]()

    for (entry <- entries) {
      def numDims = /* FIXME */ Knowledge.dimensionality
      def defOffset = L3_ConstIndex(Array.fill(numDims)(0))
      val offsets = entry.getSolField(level).localization match {
        case L3_AtCellCenter     => ListBuffer(defOffset)
        case L3_AtFaceCenter(fd) => ListBuffer(defOffset, L3_GridUtil.offsetIndex(defOffset, 1, fd))
        case other               => Logger.error(s"Unsupported localization in local solver: $other")

      }

      for (offset <- offsets) {
        val unknown = L3_FieldAccess(entry.getSolField(level))
        unknown.offsetWith(Duplicate(offset))
        unknowns += unknown

        val equation = Duplicate(entry.getEq(level))
        L3_OffsetAllApplicable.offset = offset
        L3_OffsetAllApplicable.applyStandalone(equation)
        equations += equation
      }
    }

    L3_LocalSolve(unknowns, equations, false, Some(omega), L3_FieldAccess(entryAtCell.get.getSolField(level)))
  }

  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    val stmts = ListBuffer[L3_Statement]()

    val localSolve = {
      if (1 == entries.length)
        generateLocalSolveForScalar(entries.head, level)
      else if (entries.map(_.getSolField(level).localization.name).distinct.length == 1)
        generateLocalSolveForAligned(entries, level)
      else
        generateLocalSolveForMixed(entries, level)
    }

    stmts += L3_ForLoop(4, None, ListBuffer[L3_Statement](localSolve))

    stmts
  }
}