//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.solver.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.l3._
import exastencils.grid.l3._
import exastencils.logger.Logger

/// L3_VankaForEquation

object L3_VankaForEquation {
  def omega : L3_Expression = Knowledge.solver_smoother_damping

  def generateLocalSolveForScalar(entry : L3_SolverForEqEntry, level : Int) = {
    // only one unknown => generate point-smoother

    val unknowns = ListBuffer[L3_Expression](L3_FieldAccess(entry.getSolField(level)))
    val equations = ListBuffer[L3_Equation](Duplicate(entry.getEq(level)))

    L3_LocalSolve(unknowns, equations, Knowledge.solver_smoother_jacobiType, Some(omega), L3_FieldAccess(entry.getSolField(level)))
  }

  def generateLocalSolveForAligned(entries : ListBuffer[L3_SolverForEqEntry], level : Int) = {
    // all unknowns share the same localization => simply use first as loop field

    val unknowns = ListBuffer[L3_Expression]()
    val equations = ListBuffer[L3_Equation]()

    for (entry <- entries) {
      unknowns += L3_FieldAccess(entry.getSolField(level))
      equations += Duplicate(entry.getEq(level))
    }

    L3_LocalSolve(unknowns, equations, Knowledge.solver_smoother_jacobiType, Some(omega), L3_FieldAccess(entries.head.getSolField(level)))
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

        case other => Logger.error(s"Unsupported localization in local solver: $other")

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

    L3_LocalSolve(unknowns, equations, Knowledge.solver_smoother_jacobiType, Some(omega), L3_FieldAccess(entryAtCell.get.getSolField(level)))
  }

  def generateLocalSolverFromHints(entries : ListBuffer[L3_SolverForEqEntry], smootherHint : L3_GenerateSmootherHint, level : Int) = {
    val loopField = entries.find(_.solName == smootherHint.loopBase.get.name)

    val unknowns = ListBuffer[L3_Expression]()
    val equations = ListBuffer[L3_Equation]()

    for (entry <- entries) {
      def numDims = /* FIXME */ Knowledge.dimensionality

      def defOffset = L3_ConstIndex(Array.fill(numDims)(0))

      val offsets = smootherHint.solveFor.filter(_.name == entry.solName).map(_.asInstanceOf[L3_FieldAccess].offset.getOrElse(defOffset))

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

    L3_LocalSolve(unknowns, equations, Knowledge.solver_smoother_jacobiType, Some(omega), L3_FieldAccess(loopField.get.getSolField(level)))
  }

  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int, numSteps : Int, smootherHints : ListBuffer[L3_GenerateSmootherHint]) = {
    val stmts = ListBuffer[L3_Statement]()

    /// generate local solve statement

    val localSolves : ListBuffer[L3_Statement] = {
      if (smootherHints.isEmpty) {
        if (1 == entries.length)
          ListBuffer(generateLocalSolveForScalar(entries.head, level))
        else if (entries.map(_.getSolField(level).localization.name).distinct.length == 1)
          ListBuffer(generateLocalSolveForAligned(entries, level))
        else
          ListBuffer(generateLocalSolveForMixed(entries, level))
      } else {
        smootherHints.map(generateLocalSolverFromHints(entries, _, level))
      }
    }

    /// add coloring

    val numDims = /* FIXME */ Knowledge.dimensionality

    def generateColors(numColorsPerDim : Int) = {
      val colorExps = ListBuffer[L3_Modulo]()
      for (dim <- 0 until numDims)
        colorExps += L3_Modulo(L3_FieldIteratorAccess(dim), numColorsPerDim)
      colorExps
    }

    Knowledge.solver_smoother_coloring.toLowerCase() match {
      case "none" => // nothing to do

      case "red-black" | "rb" | "2-way" =>
        val summands = ListBuffer[L3_Expression]()
        for (dim <- 0 until numDims)
          summands += L3_FieldIteratorAccess(dim)
        val numColors = L3_IntegerConstant(2)
        localSolves.transform(localSolve => L3_ColorLoops(ListBuffer(L3_Modulo(L3_Addition(summands), numColors)), ListBuffer(localSolve)))

      //case "2-way" if 1 == numDims =>
      //  localSolve = L3_ColorLoops(generateColors(2), ListBuffer(localSolve))

      case "4-way" if 2 == numDims =>
        localSolves.transform(localSolve => L3_ColorLoops(generateColors(2), ListBuffer(localSolve)))

      case "8-way" if 3 == numDims =>
        localSolves.transform(localSolve => L3_ColorLoops(generateColors(2), ListBuffer(localSolve)))

      case "3-way" if 1 == numDims =>
        localSolves.transform(localSolve => L3_ColorLoops(generateColors(3), ListBuffer(localSolve)))

      case "9-way" if 2 == numDims =>
        localSolves.transform(localSolve => L3_ColorLoops(generateColors(3), ListBuffer(localSolve)))

      case "27-way" if 3 == numDims =>
        localSolves.transform(localSolve => L3_ColorLoops(generateColors(3), ListBuffer(localSolve)))

      case _ => Logger.error(s"Unsupported coloring scheme ${ Knowledge.solver_smoother_coloring }")
    }

    /// add advance statements
    var loopBody = localSolves
    if (Knowledge.solver_smoother_jacobiType)
      loopBody ++= entries.map(e => L3_AdvanceSlot(L3_FieldAccess(e.getSolField(level))))

    /// assemble final loop

    stmts += L3_ForLoop(numSteps, None, loopBody)

    stmts
  }
}
