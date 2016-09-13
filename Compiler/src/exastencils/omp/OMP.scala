package exastencils.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

trait OMP_PotentiallyCritical

trait OMP_PotentiallyParallel {
  var reduction : Option[Reduction];
  var additionalOMPClauses = new ListBuffer[OMP_Clause];
  var collapse = 1
}

case class OMP_Barrier() extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "#pragma omp barrier"
}

case class OMP_Critical(var body : Scope) extends IR_Statement {

  import OMP_Critical._

  def this(body : IR_Statement) = this(new Scope(body))
  def this(body : ListBuffer[IR_Statement]) = this(new Scope(body))

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

case class OMP_ParallelFor(var body : ForLoopStatement, var additionalOMPClauses : ListBuffer[OMP_Clause], var collapse : Int = 1) extends IR_Statement {

  /**
    * Computes the actual omp collapse level,
    * which is the largest possible less or equal to `collapse` for the current `body`.
    */
  private def getCollapseLvl() : Int = {
    var res : Int = 1
    var stmts : ListBuffer[IR_Statement] = body.body
    while (res < collapse) {
      val filtered = stmts.filterNot(s => s.isInstanceOf[CommentStatement] || s == IR_NullStatement)
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
    out << "#pragma omp parallel for schedule(static) num_threads(" << Knowledge.omp_numThreads << ')'
    if (!additionalOMPClauses.isEmpty)
      out << ' ' <<< (additionalOMPClauses, " ")
    if (collapse > 1 && Platform.omp_version >= 3 && Knowledge.omp_useCollapse)
      out << " collapse(" << getCollapseLvl() << ')'
    out << '\n' << body
  }
}

case class OMP_WaitForFlag() extends AbstractFunctionStatement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = OMP_WaitForFlag\n"
  override def prettyprint_decl : String = prettyprint
  override def name = "waitForFlag"

  override def expand : Output[FunctionStatement] = {
    def flag = VariableAccess("flag", Some(IR_PointerDatatype(IR_VolatileDatatype(IR_BooleanDatatype))))

    FunctionStatement(IR_UnitDatatype, name, ListBuffer(FunctionArgument(flag.name, flag.datatype.get)),
      ListBuffer[IR_Statement](
        new WhileLoopStatement(IR_NegationExpression(DerefAccess(flag)), ListBuffer[IR_Statement]()),
        new AssignmentStatement(DerefAccess(flag), IR_BooleanConstant(false))),
      false)
  }
}

abstract class OMP_Clause extends Node with PrettyPrintable

case class OMP_Reduction(var op : String, var target : VariableAccess) extends OMP_Clause {
  def this(red : Reduction) = this(red.op, red.target)
  override def prettyprint(out : PpStream) : Unit = out << "reduction(" << op << " : " << target << ')'
}

case class OMP_Lastprivate(var vars : ListBuffer[VariableAccess]) extends OMP_Clause {
  def this(v : VariableAccess) = this(ListBuffer(v))
  override def prettyprint(out : PpStream) : Unit = out << "lastprivate(" <<< (vars, ", ") << ')'
}

case class OMP_Private(var vars : ListBuffer[VariableAccess]) extends OMP_Clause {
  def this(v : VariableAccess) = this(ListBuffer(v))
  override def prettyprint(out : PpStream) : Unit = out << "private(" <<< (vars, ", ") << ')'
}
