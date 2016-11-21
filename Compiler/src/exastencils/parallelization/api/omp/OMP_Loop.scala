package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.config._
import exastencils.datastructures._
import exastencils.parallelization.api.cuda.CUDA_Util
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ReplaceVariableAccess

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

  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp parallel for schedule(static) num_threads(" << Knowledge.omp_numThreads << ')'
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
    case target : IR_ForLoop if target.parallelization.potentiallyParallel && !target.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) =>
      val additionalOMPClauses = ListBuffer[OMP_Clause]()

      if (target.parallelization.reduction.isDefined)
        additionalOMPClauses += OMP_Reduction(target.parallelization.reduction.get)

      if (target.parallelization.privateVars.nonEmpty)
        additionalOMPClauses += OMP_Private(target.parallelization.privateVars.clone())

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

      ompSection.additionalOMPClauses.map {
        case reduction : OMP_Reduction if "min" == reduction.op || "max" == reduction.op =>
          hasApplicableReduction = true

          // resolve max reductions
          val redOp = reduction.op
          val redExpName = reduction.target.name
          val redDatatype = reduction.target.datatype
          def redExp = IR_VariableAccess(redExpName, redDatatype)
          val redExpLocalName = redExpName + "_red"
          def redExpLocal = IR_VariableAccess(redExpLocalName, redDatatype)

          val decl = IR_VariableDeclaration(IR_ArrayDatatype(redDatatype, Knowledge.omp_numThreads), redExpLocalName, None)
          val init = (0 until Knowledge.omp_numThreads).map(fragIdx => IR_Assignment(IR_ArrayAccess(redExpLocal, fragIdx), redExp))
          val redOperands = ListBuffer[IR_Expression](redExp) ++= (0 until Knowledge.omp_numThreads).map(fragIdx => IR_ArrayAccess(redExpLocal, fragIdx) : IR_Expression)
          val red = IR_Assignment(redExp, if ("min" == redOp) IR_Minimum(redOperands) else IR_Maximum(redOperands))

          IR_ReplaceVariableAccess.toReplace = redExp.prettyprint
          IR_ReplaceVariableAccess.replacement = IR_ArrayAccess(redExpLocal, IR_VariableAccess("omp_tid", IR_IntegerDatatype))
          IR_ReplaceVariableAccess.applyStandalone(ompSection.loop)

          prependStmts += decl
          prependStmts ++= init
          appendStmts += red

        case _ =>
      }

      if (hasApplicableReduction) {
        ompSection.loop.body.prepend(IR_VariableDeclaration(IR_IntegerDatatype, "omp_tid", "omp_get_thread_num()"))
        IR_Scope((prependStmts :+ ompSection) ++ appendStmts)
      } else {
        ompSection
      }
  }, false) // switch off recursion due to wrapping mechanism
}
