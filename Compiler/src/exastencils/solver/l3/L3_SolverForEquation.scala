package exastencils.solver.l3

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.l3.L3_UntilLoop
import exastencils.boundary.l3._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.logger.Logger
import exastencils.operator.l3._
import exastencils.optimization.l3.L3_GeneralSimplifyWrapper
import exastencils.prettyprinting.PpStream

/// L3_SolverForEquation

object L3_SolverForEquation {
  def apply(entries : List[L3_SolverForEqEntry], options : List[(String, Any)], modifications : List[L3_SolverModification]) = {
    val newOptions = HashMap[String, Any]()
    options.foreach(newOptions += _)
    new L3_SolverForEquation(entries.to[ListBuffer], newOptions, modifications.to[ListBuffer])
  }
}

case class L3_SolverForEquation(
    var entries : ListBuffer[L3_SolverForEqEntry],
    var options : HashMap[String, Any],
    var modifications : ListBuffer[L3_SolverModification]) extends L3_Statement {

  var processed = false

  def printAnyVal(v : Any) = {
    v match {
      case c : Char   => s"'$c'"
      case s : String => '"' + s + '"'
      case other      => other
    }
  }

  override def prettyprint(out : PpStream) = {
    out << "generate solver for "
    entries.foreach(e => out << e.solName << " in " << e.eqName << " and ")
    out.removeLast(" and ".length)
    if (options.nonEmpty) {
      out << " with {\n"
      options.foreach(o => out << o._1 << " = " << printAnyVal(o._2) << "\n")
      out << "}"
    }
    if (modifications.nonEmpty) {
      out << " modifiers {\n" <<< (modifications, "\n") << "}"
    }
  }

  override def progress : L4_Statement = ???

  def prepare() = {
    // handle options

    for ((option, value) <- options) {
      try {
        UniversalSetter(Knowledge, option, value)
      } catch {
        case _ : java.lang.NoSuchFieldException     => Logger.error(s"Trying to set parameter Knowledge.${ option } to ${ value } but this parameter is undefined")
        case _ : java.lang.IllegalArgumentException => Logger.error(s"Trying to set parameter Knowledge.${ option } to ${ value } but data types are incompatible")
      }
    }

    // declare knowledge objects
    declareFields()
    declareOperators()
  }

  def process() = {
    generateFields()
    generateOperators()

    processed = true
  }

  def getModificationsFor(target : String, level : Int) = {
    modifications.filter(
      m => target == m.target && (m.levels match {
        case Some(L3_SingleLevel(`level`)) => true
        case _                             => false
      })
    )
  }

  def extractStmtsFromMods(modification : String, mods : ListBuffer[L3_SolverModification]) = {
    val ret = mods.filter(modification == _.modification).flatMap(_.statements)
    ret
  }

  def declareOperators() = {
    for (level <- Knowledge.levels) {
      for (entry <- entries) {
        L3_StencilCollection.addDeclared(s"gen_restrictionForRes_${ entry.solName }", level)
        L3_StencilCollection.addDeclared(s"gen_restrictionForSol_${ entry.solName }", level)
        L3_StencilCollection.addDeclared(s"gen_prolongationForSol_${ entry.solName }", level)
      }
    }
  }

  def generateOperators() = {
    def defInterpolation = {
      Knowledge.discr_type.toLowerCase() match {
        case "fd" | "finitedifference" | "finitedifferences" => "linear"
        case "fv" | "finitevolume" | "finitevolumes"         => "integral_linear"
        case _                                               => Logger.error(s"Unsupported discretization type ${ Knowledge.discr_type }")
      }
    }

    for (level <- Knowledge.levels) {
      for (entry <- entries) {
        val field = entry.getSolField(level)

        // restriction for residual -> can be the value of an integral if FV are used
        val restrictionForRes = L3_DefaultRestriction.generate(s"gen_restrictionForRes_${ field.name }", level, field.numDimsGrid, field.localization, defInterpolation)
        L3_StencilCollection.add(restrictionForRes)
        entry.restrictForResPerLevel += (level -> restrictionForRes)

        // restriction for solution -> always use linear
        val restrictionForSol = L3_DefaultRestriction.generate(s"gen_restrictionForSol_${ field.name }", level, field.numDimsGrid, field.localization, "linear")
        L3_StencilCollection.add(restrictionForSol)
        entry.restrictForSolPerLevel += (level -> restrictionForSol)

        // prolongation for solution -> always use linear
        val prolongationForSol = L3_DefaultProlongation.generate(s"gen_prolongationForSol_${ field.name }", level, field.numDimsGrid, field.localization, "linear")
        L3_StencilCollection.add(prolongationForSol)
        entry.prolongForSolPerLevel += (level -> prolongationForSol)
      }
    }
  }

  def declareFields() = {
    entries.foreach(entry => {
      for (level <- Knowledge.levels) {
        // add a rhs for all levels but the finest (which was already declared on L2 by the user)
        if (level != Knowledge.maxLevel)
          L3_FieldCollection.addDeclared(s"gen_rhs_${ entry.solName }", level)

        // add residual fields
        L3_FieldCollection.addDeclared(s"gen_residual_${ entry.solName }", level)

        // add approximations (if required) for all levels but the finest
        if (Knowledge.solver_useFAS)
          L3_FieldCollection.addDeclared(s"gen_approx_${ entry.solName }", level)
      }
    })
  }

  def generateFields() = {
    // TODO: how to inherit function boundary conditions?

    entries.foreach(entry => {
      for (level <- Knowledge.levels) {
        val solField = entry.getSolField(level)

        // add a rhs for all levels but the finest (which was already declared on L2 by the user)
        if (level != Knowledge.maxLevel) {
          val rhsField = Duplicate.forceClone(solField)
          rhsField.name = s"gen_rhs_${ solField.name }"
          rhsField.initial = Some(0.0)
          rhsField.boundary = L3_NoBC

          L3_FieldCollection.add(rhsField)
          entry.rhsPerLevel += (level -> rhsField)
        }

        // add residual fields
        val resField = Duplicate.forceClone(solField)
        resField.name = s"gen_residual_${ solField.name }"
        resField.initial = Some(0.0)
        resField.boundary = solField.boundary match {
          case L3_DirichletBC(_)                   => L3_DirichletBC(0.0)
          case other @ (L3_NoBC | L3_NeumannBC(_)) => other
        }

        L3_FieldCollection.add(resField)
        entry.resPerLevel += (level -> resField)

        // add approximations (if required) for all levels but the finest
        if (Knowledge.solver_useFAS) {
          val approxField = Duplicate.forceClone(solField)
          approxField.name = s"gen_approx_${ solField.name }"

          L3_FieldCollection.add(approxField)
          entry.approxPerLevel += (level -> approxField)
        }
      }
    })
  }

  def generateFunctions() = {
    for (level <- Knowledge.levels if level != Knowledge.maxLevel) {
      entries.foreach(_.prepEqForMG(level))
      entries.transform(L3_GeneralSimplifyWrapper.process)
    }

    L3_IterativeSolverForEquation.resNormFctDoneForLevels.clear()

    // add solve function

    if (true) {
      var solveStmts = ListBuffer[L3_Statement]()
      def level = Knowledge.maxLevel

      L3_IterativeSolverForEquation.generateResNormFunction(entries, level)

      solveStmts ++= entries.map(_.generateUpdateRes(level))

      def initRes = L3_PlainVariableAccess("gen_initRes", L3_RealDatatype, false)
      def prevRes = L3_PlainVariableAccess("gen_prevRes", L3_RealDatatype, false)
      def curRes = L3_PlainVariableAccess("gen_curRes", L3_RealDatatype, false)
      def callResNorm = L3_FunctionCall(L3_LeveledDslFunctionReference("gen_resNorm", level, L3_RealDatatype))

      solveStmts += L3_VariableDeclaration(initRes, callResNorm)
      solveStmts += L3_VariableDeclaration(curRes, initRes)
      solveStmts += L3_VariableDeclaration(prevRes, curRes)

      solveStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
        L3_StringConstant("Starting residual: "), initRes))

      val loopStmts = ListBuffer[L3_Statement]()
      def curIt = L3_PlainVariableAccess("gen_curIt", L3_IntegerDatatype, false)
      solveStmts += L3_VariableDeclaration(curIt, 0)

      loopStmts += L3_Assignment(curIt, 1, "+=", None)

      loopStmts += L3_FunctionCall(L3_LeveledDslFunctionReference("gen_mgCycle", level, L3_UnitDatatype))
      loopStmts ++= entries.map(_.generateUpdateRes(level))
      loopStmts += L3_Assignment(prevRes, curRes)
      loopStmts += L3_Assignment(curRes, callResNorm)

      loopStmts += L3_FunctionCall(L3_PlainInternalFunctionReference("print", L3_UnitDatatype), ListBuffer[L3_Expression](
        L3_StringConstant("Residual after"), curIt, L3_StringConstant("iterations is"), curRes,
        L3_StringConstant("--- convergence factor is"), curRes / prevRes))

      solveStmts += L3_UntilLoop(
        (curIt >= Knowledge.solver_maxNumIts) OrOr (curRes <= Knowledge.solver_targetResReduction * initRes),
        loopStmts)

      // handle modifications
      val mods = getModificationsFor("solver", level)

      mods.count("replace" == _.modification) match {
        case 0 => // nothing to do

        case n =>
          if (n > 1) Logger.warn(s"Found more than one replace modification for the same target (solver)")
          solveStmts = extractStmtsFromMods("replace", mods)
      }

      solveStmts = extractStmtsFromMods("prepend", mods) ++ solveStmts
      solveStmts ++= extractStmtsFromMods("append", mods)

      // compose function

      val fct = L3_LeveledFunction(s"gen_solve", level, L3_UnitDatatype, ListBuffer(), solveStmts)
      ExaRootNode.l3_root.nodes += fct
    }

    for (level <- Knowledge.levels) {
      var solverStmts = ListBuffer[L3_Statement]()

      def handleStage(name : String, defStmts : ListBuffer[L3_Statement]) = {
        val mods = getModificationsFor(name, level)

        solverStmts ++= extractStmtsFromMods("prepend", mods)

        mods.count("replace" == _.modification) match {
          case 0 => solverStmts ++= defStmts

          case n =>
            if (n > 1) Logger.warn(s"Found more than one replace modification for the same target ($name)")
            solverStmts ++= extractStmtsFromMods("replace", mods)
        }

        solverStmts ++= extractStmtsFromMods("append", mods)
      }

      if (level != Knowledge.minLevel) {
        // regular cycle

        // smoother
        handleStage("smoother", L3_VankaForEquation.generateFor(entries, level, Knowledge.solver_smoother_numPre))

        // update residual
        handleStage("updateResidual", entries.map(_.generateUpdateRes(level)))

        // restriction
        handleStage("restriction", generateRestriction(level))

        // update solution@coarser
        if (!Knowledge.solver_useFAS)
          handleStage("setCoarseSolution", entries.map(_.generateSetSolZero(level - 1)))

        // recursion
        solverStmts += L3_FunctionCall(L3_LeveledDslFunctionReference("gen_mgCycle", level - 1, L3_UnitDatatype))

        // correction
        handleStage("correction", generateCorrection(level))

        // smoother
        handleStage("smoother", L3_VankaForEquation.generateFor(entries, level, Knowledge.solver_smoother_numPost))
      } else {
        // cgs
        handleStage("cgs", L3_IterativeSolverForEquation.generateIterativeSolver(Knowledge.solver_cgs, entries, level))
      }

      // handle whole cycle as stage
      val cycleStmts = solverStmts
      solverStmts = ListBuffer()
      handleStage("cycle", cycleStmts)

      val fct = L3_LeveledFunction(s"gen_mgCycle", level, L3_UnitDatatype, ListBuffer(), solverStmts)
      ExaRootNode.l3_root.nodes += fct
    }
  }

  def generateRestriction(level : Int) : ListBuffer[L3_Statement] = {
    val stmts = ListBuffer[L3_Statement]()

    if (Knowledge.solver_useFAS) {
      // todo: merge first two steps
      // SolutionApprox@coarser = Restriction * Solution
      stmts ++= entries.map(entry => L3_Assignment(L3_FieldAccess(entry.approxPerLevel(level - 1)),
        L3_StencilAccess(entry.restrictForSolPerLevel(level)) * L3_FieldAccess(entry.getSolField(level))))

      // Solution@coarser = SolutionApprox@coarser
      stmts ++= entries.map(entry => L3_Assignment(L3_FieldAccess(entry.getSolField(level - 1)),
        L3_FieldAccess(entry.approxPerLevel(level - 1))))

      // RHS@coarser = Restriction * Residual + A@coarser * Approx@coarser
      stmts ++= entries.map(entry => {
        // generate operator application first
        val opApplication = Duplicate(L3_ExpressionStatement(entry.getEq(level - 1).lhs))
        object L3_ReplaceAccesses extends QuietDefaultStrategy("Local replace of field accesses with approximation variants") {
          this += new Transformation("Search and replace", {
            case access @ L3_FieldAccess(field, _) if entries.exists(field == _.getSolField(level - 1)) =>
              access.target = entries.find(field == _.getSolField(level - 1)).get.getSolField(level - 1) // FIXME: approxPerLevel(level - 1)
              access
          })
        }

        L3_ReplaceAccesses.applyStandalone(opApplication)

        // assemble assignment
        L3_Assignment(L3_FieldAccess(entry.rhsPerLevel(level - 1)),
          L3_StencilAccess(entry.restrictForResPerLevel(level)) * L3_FieldAccess(entry.resPerLevel(level))
            + opApplication.expression)
      })
    } else {
      // RHS@coarser = Restriction * Residual
      stmts ++= entries.map(entry => L3_Assignment(L3_FieldAccess(entry.rhsPerLevel(level - 1)),
        L3_StencilAccess(entry.restrictForResPerLevel(level)) * L3_FieldAccess(entry.resPerLevel(level))))
    }

    stmts
  }

  def generateCorrection(level : Int) : ListBuffer[L3_Statement] = {
    val stmts = ListBuffer[L3_Statement]()

    if (Knowledge.solver_useFAS) {
      // Solution@coarser -= Approx@coarser
      stmts ++= entries.map(entry => L3_Assignment(L3_FieldAccess(entry.getSolField(level - 1)),
        L3_FieldAccess(entry.approxPerLevel(level - 1)), "-=", None))
    }

    // Solution += Prolongation@coarser * Solution@coarser
    stmts ++= entries.map(entry => L3_Assignment(L3_FieldAccess(entry.getSolField(level)),
      L3_StencilAccess(entry.prolongForSolPerLevel(level - 1)) * L3_FieldAccess(entry.getSolField(level - 1)),
      "+=", None))

    stmts
  }
}

/// L3_SolverForEqEntry

case class L3_SolverForEqEntry(solName : String, eqName : String) extends L3_Node {
  var rhsPerLevel = HashMap[Int, L3_Field]()
  var resPerLevel = HashMap[Int, L3_Field]()
  var approxPerLevel = HashMap[Int, L3_Field]()

  var restrictForResPerLevel = HashMap[Int, L3_Stencil]()
  var restrictForSolPerLevel = HashMap[Int, L3_Stencil]()
  var prolongForSolPerLevel = HashMap[Int, L3_Stencil]()

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

  def generateSetSolZero(level : Int) : L3_Statement = {
    // Solution = 0
    L3_Assignment(L3_FieldAccess(getSolField(level)), 0.0)
  }
}

/// L3_PrepareSolverForEquations

object L3_PrepareSolverForEquations extends DefaultStrategy("Update knowledge with information from solver for equation nodes and prepare declarations") {
  this += new Transformation("Process", {
    // check if declaration has already been processed and promote access if possible
    case solver : L3_SolverForEquation =>
      solver.prepare()
      solver // keep for later
  })
}

/// L3_ProcessSolverForEquations

object L3_ProcessSolverForEquations extends DefaultStrategy("Update knowledge with information from solver for equation nodes") {
  this += new Transformation("Process", {
    // check if declaration has already been processed and promote access if possible
    case solver : L3_SolverForEquation if !solver.processed && Knowledge.levels.flatMap(lvl => solver.entries.map(e => L3_FieldCollection.exists(e.solName, lvl))).reduce(_ && _) =>
      solver.process()
      solver // keep for later
  })
}

/// L3_ResolveSolverForEquations

object L3_ResolveSolverForEquations extends DefaultStrategy("Resolve solver for equation nodes") {
  this += new Transformation("Resolve", {
    // check if declaration has already been processed and promote access if possible
    case solver : L3_SolverForEquation if L3_MayBlockResolution.isDone(solver) =>
      solver.generateFunctions()
      None // consume statement
  })
}

