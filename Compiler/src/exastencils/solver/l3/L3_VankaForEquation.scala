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
    val loopField = entries.find(_.getSolField(level).name == smootherHint.loopBase.get.name)

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

  def generateFor(entries : ListBuffer[L3_SolverForEqEntry], level : Int, numSteps : Int, smootherHint : Option[L3_GenerateSmootherHint]) = {
    val stmts = ListBuffer[L3_Statement]()

    /// generate local solve statement

    var localSolve : L3_Statement = {
      if (smootherHint.isEmpty) {
        if (1 == entries.length)
          generateLocalSolveForScalar(entries.head, level)
        else if (entries.map(_.getSolField(level).localization.name).distinct.length == 1)
          generateLocalSolveForAligned(entries, level)
        else
          generateLocalSolveForMixed(entries, level)
      } else {
        generateLocalSolverFromHints(entries, smootherHint.get, level)
      }
    }

    /// add coloring

    def numDims = /* FIXME */ Knowledge.dimensionality

    def generateColors(numColorsPerDim : Int) = {
      var colorCases = (0 until numColorsPerDim).map(ListBuffer(_)).to[ListBuffer]
      for (_ <- 1 until numDims)
        colorCases = (0 until numColorsPerDim).to[ListBuffer].flatMap(c => colorCases.map(_ :+ c))

      val colors = ListBuffer[L3_Expression]()
      colorCases.foreach(c => {
        colors += c.zipWithIndex.map({
          case (i, dim) => L3_EqEq(i, L3_Modulo(L3_FieldIteratorAccess(dim), numColorsPerDim)) : L3_Expression
        }).reduceLeft(L3_AndAnd)
      })

      colors
    }

    Knowledge.solver_smoother_coloring.toLowerCase() match {
      case "none" => // nothing to do

      case "red-black" | "rb" | "2-way" =>
        val colors = ListBuffer[L3_Expression]()

        def numColors = 2

        for (c <- 0 until numColors)
          colors += L3_EqEq(c, L3_Modulo(L3_Addition((0 until numDims).map(L3_FieldIteratorAccess(_) : L3_Expression).to[ListBuffer]), numColors))
        localSolve = L3_ColorLoops(colors, ListBuffer(localSolve))

      case "4-way" if 2 == numDims =>
        localSolve = L3_ColorLoops(generateColors(2), ListBuffer(localSolve))

      case "8-way" if 3 == numDims =>
        localSolve = L3_ColorLoops(generateColors(2), ListBuffer(localSolve))

      case "9-way" if 2 == numDims =>
        localSolve = L3_ColorLoops(generateColors(3), ListBuffer(localSolve))

      case "27-way" if 3 == numDims =>
        localSolve = L3_ColorLoops(generateColors(3), ListBuffer(localSolve))

      case _ => Logger.error(s"Unsupported coloring scheme ${ Knowledge.solver_smoother_coloring }")
    }

    /// add advance statements
    var loopBody = ListBuffer(localSolve)
    if (Knowledge.solver_smoother_jacobiType)
      loopBody ++= entries.map(e => L3_AdvanceSlot(L3_FieldAccess(e.getSolField(level))))

    /// assemble final loop

    stmts += L3_ForLoop(numSteps, None, loopBody)

    stmts
  }
}