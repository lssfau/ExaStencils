package exastencils.parallelization.api.omp

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ComplexDatatype
import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// OMP_DeclareReduction

object OMP_DeclareReduction {
  def apply(identifier : String, typenameList : ListBuffer[IR_Datatype], combiner : IR_Expression, initializer : IR_Expression = IR_NullExpression) = {
    new OMP_DeclareReduction(identifier,typenameList,combiner, initializer)
  }
}

case class OMP_DeclareReduction(identifier : String, typenameList : ListBuffer[IR_Datatype], combiner : IR_Expression, initializer : IR_Expression = IR_NullExpression) extends IR_Statement {
  def plainPrintDatatype(dt : IR_Datatype) : String = {
    dt match {
      case IR_ComplexDatatype(datatype) => datatype match {
        case IR_RealDatatype   => "std::complex<double>"
        case IR_DoubleDatatype => "std::complex<double>"
        case IR_FloatDatatype  => "std::complex<float>"
        case _                 => Logger.error("datatype currently not supported for custom reduction")
      }
      case _ => Logger.error("datatype currently not supported for custom reduction")
    }
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "#pragma omp declare reduction(" << identifier << " : "
    if(typenameList.length > 0) {
      out << plainPrintDatatype(typenameList(0))
      for (i <- 1 until typenameList.length) {
        out << plainPrintDatatype(typenameList(i))
      }
    }
    out << " : " << "omp_out = " << combiner << ") "  << "initializer(omp_priv = " << initializer << ")"
  }
}
