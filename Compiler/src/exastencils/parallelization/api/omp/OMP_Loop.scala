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

package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config._
import exastencils.core
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_Vectorization
import exastencils.parallelization.api.cuda.CUDA_Util
import exastencils.prettyprinting.PpStream

case class OMP_Parallel(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp parallel\n"
    out << "{\n"
    out <<< body
    out << "\n}\n"
  }
}

/// OMP_ParallelFor

case class OMP_ParallelFor(
    var loop : IR_ForLoop,
    var additionalOMPClauses : ListBuffer[OMP_Clause],
    var collapse : Int = 1) extends IR_Statement {

  /**
    * computes the actual omp collapse level,
    * which is the largest possible less or equal to `collapse` for the current `body`.
    */
  private def getCollapseLvl() : Int = {
    var res : Int = 1
    var curStmts = loop.body
    while (res < collapse) {
      val filtered = curStmts.filterNot(s => s.isInstanceOf[IR_Comment] || s == IR_NullStatement)
      if (filtered.length != 1)
        return res // no more than one statement allowed: not perfectly nested anymore, return last valid collapse level
      curStmts =
        filtered.head match {
          case s : IR_Scope   => s.body
          case l : IR_ForLoop => res += 1; l.body
          case _              => return res // any other statement: not perfectly nested anymore, return last valid collapse level
        }
    }

    res
  }

  // mainly error checking. from spec @ v5.1
  def schedule = {
    val modifiers = List("monotonic", "nonmonotonic", "simd")
    val kinds = List("static", "dynamic", "guided", "auto", "runtime")

    // signature: schedule([modifier [, modifier]:]kind[, chunk_size])
    val pattern = """(?:[A-Za-z]+\s*(?:,\s*[A-Za-z]+)?:)?\s*([A-Za-z]+)\s*(?:,\s*([0-9]+))?""".r
    Knowledge.omp_scheduling match {
      case pattern(kind) if kinds.contains(kind) =>
      case pattern(kind, _) if kinds.contains(kind) =>
      case pattern(mod, kind) if modifiers.contains(mod) && kinds.contains(kind) =>
      case pattern(mod, kind, _) if modifiers.contains(mod) && kinds.contains(kind) =>
      case pattern(mod, mod2, kind) if List(mod, mod2).forall(modifiers.contains) && kinds.contains(kind) =>
      case pattern(mod, mod2, kind, _) if List(mod, mod2).forall(modifiers.contains) && kinds.contains(kind) =>
      case _ => Logger.error("OMP_ParallelFor: Invalid Knowledge parameter: omp_scheduling = " + Knowledge.omp_scheduling)
    }
    Knowledge.omp_scheduling
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp parallel for schedule(" << schedule << ") num_threads(" << Knowledge.omp_numThreads << ')'
    if (additionalOMPClauses.nonEmpty)
      out << ' ' <<< (additionalOMPClauses, " ")
    if (collapse > 1 && Platform.omp_version >= 3 && Knowledge.omp_useCollapse)
      out << " collapse(" << getCollapseLvl() << ')'
    out << '\n'
    out << loop
  }
}

/// OMP_AddParallelSections

object OMP_AddParallelSections extends DefaultStrategy("Handle potentially parallel omp sections") {
  this += new Transformation("Adding OMP parallel for pragmas", {
    case target : IR_ForLoop if target.parallelization.potentiallyParallel && !target.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) &&
      target.parallelization.reduction.isDefined && target.hasAnnotation(IR_Vectorization.VECT_ANNOT)                                                                                   =>
      // FIXME: workaround for feature interaction
      Logger.warn("Parallelizing and Vectorizing a loop with a reduction is currently not supported! If required, contact Stefan.")
      target
    case target : IR_ForLoop if target.parallelization.potentiallyParallel && target.parallelization.parallelizationReasonable && !target.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) =>
      val additionalOMPClauses = ListBuffer[OMP_Clause]()

      if (target.parallelization.reduction.isDefined)
        additionalOMPClauses += OMP_Reduction(target.parallelization.reduction.get)

      if (target.parallelization.privateVars.nonEmpty)
        additionalOMPClauses += OMP_Private(target.parallelization.privateVars.clone())

      // add declared custom "+"-reduction for complex numbers
      var red = target.parallelization.reduction
      if(red.isDefined && red.get.target.datatype.isInstanceOf[IR_ComplexDatatype]) {
        // add combiner to util functions
        val dt = red.get.target.datatype
        //if (!IR_UtilFunctions.get.functions.exists(f => f.name == "ComplexRedAdd")) {
        //  IR_UtilFunctions.get += IR_ComplexOperations.generateOMPRedAdd(dt)
        //}
        ListBuffer[IR_Statement](
          OMP_DeclareReduction(
            "+",
            ListBuffer[IR_Datatype](dt),
            //IR_Assignment(IR_VariableAccess("omp_out", dt), IR_VariableAccess("omp_in",dt), "+="),
            IR_Addition(IR_VariableAccess("omp_out", dt), IR_VariableAccess("omp_in",dt)),
            IR_IntegerConstant(0)
          ),
          OMP_ParallelFor(target, additionalOMPClauses, target.parallelization.collapseDepth)
        )
      } else
        OMP_ParallelFor(target, additionalOMPClauses, target.parallelization.collapseDepth)
  }, false) // switch off recursion due to wrapping mechanism
}

/// OMP_ResolveMinMaxReduction

object OMP_ResolveMinMaxReduction extends DefaultStrategy("Resolve omp min and max reductions") {
  this += new Transformation("Resolve", {
    case ompSection : OMP_ParallelFor =>
      var hasApplicableReduction = false
      var prependStmts = ListBuffer[IR_Statement]()
      var appendStmts = ListBuffer[IR_Statement]()

      var toRemove = ListBuffer[OMP_Clause]()
      ompSection.additionalOMPClauses.map {
        case reduction : OMP_Reduction if "min" == reduction.op || "max" == reduction.op =>
          hasApplicableReduction = true
          toRemove += reduction

          // resolve max reductions
          val redOp = reduction.op
          val redDatatype = reduction.target.datatype
          def redExp = reduction.target
          val redExpLocalName = reduction.target.prettyprint.replaceAll("[\\[\\](){}]", "_") + "_red"
          def redExpLocal = IR_VariableAccess(redExpLocalName, redDatatype)

          val decl = IR_VariableDeclaration(IR_ArrayDatatype(redDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
          val init = (0 until Knowledge.omp_numThreads).map(fragIdx => IR_Assignment(IR_ArrayAccess(redExpLocal, fragIdx), redExp))
          val redOperands = ListBuffer[IR_Expression](redExp) ++= (0 until Knowledge.omp_numThreads).map(fragIdx => IR_ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
          val red = IR_Assignment(redExp, if ("min" == redOp) IR_Minimum(redOperands) else IR_Maximum(redOperands))

          IR_ReplaceVariableAccessWoReduction.toReplace = redExp.prettyprint
          IR_ReplaceVariableAccessWoReduction.replacement = IR_ArrayAccess(redExpLocal, IR_VariableAccess("omp_tid", IR_IntegerDatatype))
          IR_ReplaceVariableAccessWoReduction.applyStandalone(IR_Scope(ompSection.loop.body))

          prependStmts += decl
          prependStmts ++= init
          appendStmts += red

        case _ =>
      }

      ompSection.additionalOMPClauses --= toRemove

      if (hasApplicableReduction) {
        ompSection.loop.body.prepend(IR_VariableDeclaration(IR_IntegerDatatype, "omp_tid", "omp_get_thread_num()"))
        IR_Scope((prependStmts :+ ompSection) ++ appendStmts)
      } else {
        ompSection
      }
  }, false) // switch off recursion due to wrapping mechanism
}

object OMP_FixArithmeticReductionOrder extends DefaultStrategy("Fix order of arithmetic omp reductions") {
  this += new Transformation("Resolve", {
    case ompSection : OMP_ParallelFor =>
      var hasApplicableReduction = false
      var prependStmts = ListBuffer[IR_Statement]()
      var appendStmts = ListBuffer[IR_Statement]()

      var toRemove = ListBuffer[OMP_Clause]()
      ompSection.additionalOMPClauses.map {
        case reduction : OMP_Reduction if "+" == reduction.op || "-" == reduction.op || "*" == reduction.op=>
          hasApplicableReduction = true
          toRemove += reduction

          // resolve max reductions
          val redOp = reduction.op
          val redDatatype = reduction.target.datatype
          def redExp = reduction.target
          val redExpLocalName = reduction.target.prettyprint.replaceAll("[\\[\\](){}]", "_") + "_local"
          def redExpLocal = IR_VariableAccess(redExpLocalName, redDatatype)

          val decl = IR_VariableDeclaration(IR_ArrayDatatype(redDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
          val init = (0 until Knowledge.omp_numThreads).map(fragIdx => redOp match {
            case "+" => IR_Assignment(IR_ArrayAccess(redExpLocal, fragIdx), IR_RealConstant(0.0))
            case "-" => IR_Assignment(IR_ArrayAccess(redExpLocal, fragIdx), IR_RealConstant(0.0))
            case "*" => IR_Assignment(IR_ArrayAccess(redExpLocal, fragIdx), IR_RealConstant(1.0))
          })
          val redOperands = ListBuffer[IR_Expression](redExp) ++= (0 until Knowledge.omp_numThreads).map(fragIdx => IR_ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
          val red = IR_Assignment(redExp, redOp match {
            case "+" => IR_Addition(redOperands)
            case "-" => IR_Negation(IR_Addition(redOperands))
            case "*" => IR_Multiplication(redOperands)
          })

          IR_ReplaceVariableAccessWoReduction.toReplace = redExp.prettyprint
          IR_ReplaceVariableAccessWoReduction.replacement = IR_ArrayAccess(redExpLocal, IR_VariableAccess("omp_tid", IR_IntegerDatatype))
          IR_ReplaceVariableAccessWoReduction.applyStandalone(IR_Scope(ompSection.loop.body))

          prependStmts += decl
          prependStmts ++= init
          appendStmts += red

        case _ =>
      }

      ompSection.additionalOMPClauses --= toRemove

      if (hasApplicableReduction) {
        ompSection.loop.body.prepend(IR_VariableDeclaration(IR_IntegerDatatype, "omp_tid", "omp_get_thread_num()"))
        IR_Scope((prependStmts :+ ompSection) ++ appendStmts)
      } else {
        ompSection
      }
  }, false) // switch off recursion due to wrapping mechanism
}

object IR_ReplaceVariableAccessWoReduction extends QuietDefaultStrategy("Replace something with something else but skip reductions") {
  var toReplace : String = ""
  var replacement : Node = IR_VariableAccess("", IR_UnknownDatatype) // to be overwritten

  this += new Transformation("Search and replace", {
    // TODO: rely only on IR_VariableAccess => eliminate IR_StringLiteral occurrences
    case red : IR_Reduction                                     => red
    case IR_StringLiteral(s) if s == toReplace                  => core.Duplicate(replacement)
    case access : IR_VariableAccess if access.name == toReplace => core.Duplicate(replacement)
  }, false)
}

