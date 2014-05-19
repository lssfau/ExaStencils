package harald_dep.Abstract

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import harald_dep.dsl._
import harald_dep.Impl._
import harald_dep.ast.TreeL2
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

case class AbstractFunction(fname : String, location : String, rettype : String, paramlist : List[Param], stmts : List[AbstractStatement]) {

  def transformToIR() : FunctionStatement = {
    var name : String = ""
    if (fname.equals("Jac"/*Knowledge.mg_smoother*/))
      name = "smoother"
    else
      name = fname

    var palist : ListBuffer[ParameterInfo] = ListBuffer()
    var params : ListBuffer[VariableAccess] = ListBuffer()
    for (p <- paramlist) {
      palist += new ParameterInfo(p.name, DomainKnowledge.transform_datatype_cpp(p.dtype))
      params += VariableAccess(p.name, Some(DomainKnowledge.transform_datatype_cpp(p.dtype)))
    }
    var stlist : ListBuffer[Statement] = ListBuffer()
    var varlist : ListBuffer[String] = ListBuffer()

    for (st <- stmts) {
      varlist ++= st.getvariables
      stlist ++= st.transform(palist)
    }

    var m : Map[String, Int] = Map()
    for (v <- varlist)
      m += v -> (m.getOrElse(v, 0) + 1)

    return FunctionStatement(DomainKnowledge.transform_datatype_cpp(rettype), fname, params, stlist)
  }
}
