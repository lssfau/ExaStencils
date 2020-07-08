package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream

/// OMP_DeclareReduction

object OMP_DeclareReduction {
  def apply(identifier : String, typenameList : ListBuffer[String], combiner : IR_Expression, initializer : IR_Expression = IR_NullExpression) = {
    new OMP_DeclareReduction(identifier,typenameList,combiner,initializer)
  }
}

case class OMP_DeclareReduction(identifier : String, typenameList : ListBuffer[String], combiner : IR_Expression, initializer : IR_Expression = IR_NullExpression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp declare reduction(" << identifier << " : "
    if(typenameList.length > 0) {
      out << typenameList(0)
      for (i <- 1 until typenameList.length) {
        out << typenameList(i)
      }
    }
    out << " : " << combiner << ")" << "initializer(" << initializer << ")"
  }
}
