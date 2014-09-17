package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.ir._
import exastencils.knowledge._

trait OMP_PotentiallyCritical
trait OMP_PotentiallyParallel { var reduction : Option[Reduction]; var addOMPStatements = new ListBuffer[String](); var collapse = 1 }

case class OMP_Barrier() extends Statement {
  override def cpp(out : CppStream) : Unit = out << "#pragma omp barrier"
}

case class OMP_Critical(var body : Scope) extends Statement {
  def this(body : Statement) = this(new Scope(body))
  def this(body : ListBuffer[Statement]) = this(new Scope(body))

  override def cpp(out : CppStream) : Unit = out << "#pragma omp critical\n" << body
}

case class OMP_ParallelFor(var body : ForLoopStatement, var addOMPStatements : ListBuffer[String], var collapse : Int = 1) extends Statement {

  /**
    * Computes the actual omp collapse level,
    * which is the largest possible less or equal to `collapse` for the current `body`.
    */
  private def getCollapseLvl() : Int = {
    var res : Int = 1
    var stmts : ListBuffer[Statement] = body.body
    while (res < collapse) {
      val filtered = stmts.filterNot(s => s.isInstanceOf[CommentStatement] || s == NullStatement)
      if (filtered.length != 1)
        return res // no more than one statement allowed: not perfectly nested anymore, return last valid collapse level
      stmts =
        filtered(0) match {
          case s : Scope            => s.body
          case l : ForLoopStatement => { res += 1; l.body }
          case _                    => return res // any other statement: not perfectly nested anymore, return last valid collapse level
        }
    }

    res // res == collapse now
  }

  def cpp(out : CppStream) : Unit = {
    out << "#pragma omp parallel for schedule(static) num_threads("
    out << math.max(Knowledge.domain_fragLength, Knowledge.domain_numFragsPerBlock) << ')' << addOMPStatements.mkString(" ", " ", "")
    if (collapse > 1 && Knowledge.omp_version >= 3 && Knowledge.omp_useCollapse)
      out << " collapse(" << getCollapseLvl() << ')'
    out << '\n' << body
  }
}