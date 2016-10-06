package exastencils.omp.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.cuda.CudaStrategiesUtils
import exastencils.datastructures._
import exastencils.knowledge._
import exastencils.logger.Logger
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

/// OMP_PotentiallyParallel

object OMP_PotentiallyParallel {
  def apply(body : IR_Statement*) = new OMP_PotentiallyCritical(body.to[ListBuffer])
}

case class OMP_PotentiallyParallel(var body : ListBuffer[IR_Statement], var collapse : Int = 1) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"
}

/// OMP_HandleParallelSections

object OMP_HandleParallelSections extends DefaultStrategy("Handle potentially critical omp sections") {
  this += new Transformation("Adding OMP critical pragmas", {
    case target : OMP_PotentiallyParallel =>
      // filter target body => ignore comments and null statements
      val filtered = target.body.filterNot(s => s.isInstanceOf[IR_Comment] || s == IR_NullStatement)

      if (0 == filtered.length)
      // empty => just return body
        target.body
      else if (filtered.length > 1) {
        // more than one statement - not supported
        Logger.warn("Found OMP_PotentiallyParallel with more than one statement - not supported: " + filtered.map(_.prettyprint()).mkString(" ; "))
        target.body
      }
      else {
        // exactly one hit => handle loop
        filtered.head match {
          // ignore loops with CUDA_LOOP_ANNOTATION
          case loop : IR_ForLoop if loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) =>
            None

          // plain loop => wrap
          case loop : IR_ForLoop =>
            val additionalClauses = ListBuffer[OMP_Clause]()

            // handle reduction
            if (loop.reduction.isDefined)
              if (!(Platform.omp_version < 3.1 && ("min" == loop.reduction.get.op || "max" == loop.reduction.get.op)))
                additionalClauses += OMP_Reduction(loop.reduction.get)
            // FIXME: else?

            // TODO: integrate OptimizationHint
            loop match {
              case l : OptimizationHint => if (l.privateVars.nonEmpty) additionalClauses += OMP_Private(l.privateVars.clone())
              case _                    =>
            }

            if (target.body.count(_.isInstanceOf[IR_Comment]) > 0)
              Logger.warn("Removing comments in omp parallel for section")

            OMP_ParallelFor(IR_ForLoop(loop.begin, loop.end, loop.inc, loop.body, loop.reduction),
              additionalClauses, target.collapse)
        }
      }
  }, false)
}
