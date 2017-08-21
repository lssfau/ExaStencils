package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.base.l4.L4_Statement
import exastencils.boundary.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.domain.l3._
import exastencils.field.l3._
import exastencils.grid.l3.L3_Localization
import exastencils.operator.l3._
import exastencils.prettyprinting.PpStream

/// L3_SolverForEquation

object L3_SolverForEquation {
  def apply(entries : List[L3_SolverForEqEntry]) = new L3_SolverForEquation(entries.to[ListBuffer])
}

case class L3_SolverForEquation(var entries : ListBuffer[L3_SolverForEqEntry]) extends L3_Statement {
  override def prettyprint(out : PpStream) = {
    out << "generate solver for "
    entries.foreach(e => out << e.solName << " in " << e.eqName << " and ")
    out.removeLast(" and ".length)
  }

  override def progress : L4_Statement = ???

  def genOperators() = {
    for (level <- Knowledge.levels) {
      // tuples with number of dimensions and localization
      val localizations = Set[(Int, L3_Localization)]()
      for (entry <- entries) {
        val field = entry.getSolField(level)
        localizations += ((field.numDimsGrid, field.localization))
      }

      for (local <- localizations) {
        val restriction = L3_DefaultRestriction.generate(s"gen_restriction_${ local._1 }D_${ local._2.name }", level, local._1, local._2, /*FIXME*/ "integral_linear")
        L3_StencilCollection.add(restriction)
        entries.filter(e => e.getSolField(level).numDimsGrid == local._1 && e.getSolField(level).localization == local._2).foreach(_.restrictPerLevel += (level -> restriction))

        val prolongation = L3_DefaultProlongation.generate(s"gen_prolongation_${ local._1 }D_${ local._2.name }", level, local._1, local._2, /*FIXME*/ "integral_linear")
        L3_StencilCollection.add(prolongation)
        entries.filter(e => e.getSolField(level).numDimsGrid == local._1 && e.getSolField(level).localization == local._2).foreach(_.prolongPerLevel += (level -> prolongation))
      }
    }
  }

  def generateFields() = {
    entries.foreach(entry => {
      val domain = L3_DomainCollection.getByIdentifier("global").get

      for (level <- Knowledge.levels) {
        val solField = entry.getSolField(level)

        if (level != Knowledge.maxLevel) {
          val rhsField = L3_Field(s"gen_rhs_${ solField.name }", level, domain,
            solField.datatype, solField.localization, Some(0.0), L3_NoBC)

          L3_FieldCollection.add(rhsField)
          entry.rhsPerLevel += (level -> rhsField)
        }

        val resBC = solField.boundary match {
          case L3_DirichletBC(_)                   => L3_DirichletBC(0.0)
          case other @ (L3_NoBC | L3_NeumannBC(_)) => other
        }

        val resField = L3_Field(s"gen_residual_${ solField.name }", level, domain,
          solField.datatype, solField.localization, Some(0.0), resBC)

        L3_FieldCollection.add(resField)
        entry.resPerLevel += (level -> resField)
      }
    })
  }

  def generateFunctions() = {
    for (level <- Knowledge.levels if level != Knowledge.maxLevel)
      entries.foreach(_.prepEqForMG(level))

    for (level <- Knowledge.levels) {
      val solverStmts = ListBuffer[L3_Statement]()

      if (level != Knowledge.minLevel) {
        // regular cycle

        // smoother
        solverStmts ++= L3_VankaForEquation.generateFor(entries, level)

        // update residual
        solverStmts ++= entries.map(_.generateUpdateRes(level))

        // update rhs@coarser
        solverStmts ++= entries.map(_.generateRestriction(level))

        // update solution@coarser
        solverStmts ++= entries.map(_.generateSetSolZero(level - 1))

        // recursion
        solverStmts += L3_FunctionCall(L3_LeveledDslFunctionReference("gen_mgCycle", level - 1, L3_UnitDatatype))

        // correction
        solverStmts ++= entries.map(_.generateCorrection(level))

        // smoother
        solverStmts ++= L3_VankaForEquation.generateFor(entries, level)

      } else {
        // cgs

        if (1 == entries.length) // TODO: find a better way to decide which solver to be used
          solverStmts ++= L3_ConjugateGradientForEquation.generateFor(entries, level)
        else
          solverStmts ++= L3_BiCGStabForEquation.generateFor(entries, level)
      }

      val fct = L3_LeveledFunction(s"gen_mgCycle", level, L3_UnitDatatype, ListBuffer(), solverStmts)
      ExaRootNode.l3_root.nodes += fct
    }
  }

}

/// L3_SolverForEqEntry

case class L3_SolverForEqEntry(solName : String, eqName : String) extends L3_Node {
  var rhsPerLevel = HashMap[Int, L3_Field]()
  var resPerLevel = HashMap[Int, L3_Field]()

  var restrictPerLevel = HashMap[Int, L3_Stencil]()
  var prolongPerLevel = HashMap[Int, L3_Stencil]()

  var localEqPerLevel = HashMap[Int, L3_Equation]()

  def getSolField(level : Int) = L3_FieldCollection.getByIdentifier(solName, level).get
  def getEq(level : Int) = {
    if (!localEqPerLevel.contains(level))
      localEqPerLevel += (level -> Duplicate(L3_EquationCollection.getByIdentifier(eqName, level).get.equation))
    localEqPerLevel(level)
  }

  def prepEqForMG(level : Int) = {
    getEq(level).rhs += L3_FieldAccess(rhsPerLevel(level))
  }

  def generateUpdateRes(level : Int) : L3_Statement = {
    // Residual = RHS - LHS
    L3_Assignment(L3_FieldAccess(resPerLevel(level)),
      Duplicate(getEq(level).rhs - getEq(level).lhs))
  }

  def generateRestriction(level : Int) : L3_Statement = {
    // RHS@coarser = Restriction * Residual
    L3_Assignment(L3_FieldAccess(rhsPerLevel(level - 1)),
      L3_StencilAccess(restrictPerLevel(level)) * L3_FieldAccess(resPerLevel(level)))
  }

  def generateCorrection(level : Int) : L3_Statement = {
    // Solution += Prolongation@coarser * Solution@coarser
    L3_Assignment(L3_FieldAccess(getSolField(level)),
      L3_StencilAccess(prolongPerLevel(level - 1)) * L3_FieldAccess(getSolField(level - 1)),
      "+=", None)
  }

  def generateSetSolZero(level : Int) : L3_Statement = {
    // Solution = 0
    L3_Assignment(L3_FieldAccess(getSolField(level)), 0.0)
  }
}

/// L3_ResolveSolverForEquations

object L3_ResolveSolverForEquations extends DefaultStrategy("Resolve solver for equation nodes") {
  this += new Transformation("Resolve", {
    // check if declaration has already been processed and promote access if possible
    case solver : L3_SolverForEquation if L3_MayBlockResolution.isDone(solver) =>
      solver.generateFields()
      solver.genOperators()
      solver.generateFunctions()

      None // consume statement
  })
}

