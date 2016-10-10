package exastencils.omp.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config._
import exastencils.cuda.CudaStrategiesUtils
import exastencils.datastructures._
import exastencils.optimization.OptimizationHint
import exastencils.prettyprinting.PpStream

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
    case target : IR_ForLoop if target.parallelization.potentiallyParallel && !target.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) =>
      val additionalOMPClauses = ListBuffer[OMP_Clause]()

      if (target.parallelization.reduction.isDefined)
        if (!(Platform.omp_version < 3.1 && ("min" == target.parallelization.reduction.get.op || "max" == target.parallelization.reduction.get.op)))
          additionalOMPClauses += OMP_Reduction(target.parallelization.reduction.get)

      target match {
        case l : OptimizationHint =>
          if (l.privateVars.nonEmpty)
            additionalOMPClauses += OMP_Private(l.privateVars.clone())
        case _                    =>
      }

      OMP_ParallelFor(target, additionalOMPClauses, target.parallelization.collapseDepth)
  }, false) // switch off recursion due to wrapping mechanism
}
