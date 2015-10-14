package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

trait OMP_PotentiallyCritical
trait OMP_PotentiallyParallel { var reduction : Option[Reduction]; var addOMPStatements = new ListBuffer[String](); var collapse = 1 }

case class OMP_Barrier() extends Statement {
  override def prettyprint(out : PpStream) : Unit = out << "#pragma omp barrier"
}

case class OMP_Critical(var body : Scope) extends Statement {
  import OMP_Critical._

  def this(body : Statement) = this(new Scope(body))
  def this(body : ListBuffer[Statement]) = this(new Scope(body))

  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp critical"
    if (Knowledge.omp_nameCriticalSections) {
      out << s" (section_$counter)"
      counter += 1
    }
    out << "\n" << body
  }
}

case object OMP_Critical {
  var counter = 0
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

  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp parallel for schedule(static) num_threads(" << Knowledge.omp_numThreads << ')' << addOMPStatements.mkString(" ", " ", "")
    if (collapse > 1 && Knowledge.omp_version >= 3 && Knowledge.omp_useCollapse)
      out << " collapse(" << getCollapseLvl() << ')'
    out << '\n' << body
  }
}

case class OMP_WaitForFlag() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = OMP_WaitForFlag\n"
  override def prettyprint_decl : String = prettyprint

  override def expand : Output[FunctionStatement] = {
    def flag = VariableAccess("flag", Some(PointerDatatype(IntegerDatatype)))

    FunctionStatement(UnitDatatype, s"waitForFlag", ListBuffer(flag),
      ListBuffer[Statement](
        new WhileLoopStatement(NegationExpression(DerefAccess(flag)), ListBuffer[Statement]()),
        new AssignmentStatement(DerefAccess(flag), BooleanConstant(false))),
      false)
  }
}
