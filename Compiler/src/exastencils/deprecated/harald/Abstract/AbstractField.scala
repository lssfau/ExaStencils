package exastencils.deprecated.harald.Abstract

import scala.collection.mutable.ListBuffer

import exastencils.deprecated.harald.Impl._
import exastencils.deprecated.harald.dsl._

class AbstractField(val name : String, val datatype : String, val veclength : String, val location : String) {
  override def toString = "ExaField: '" + name + "', type = '" + datatype + "', location = '" + location + "'"

  def transform : ListBuffer[ImplField] = {
    var fieldbuf : ListBuffer[ImplField] = ListBuffer()
    if (DomainKnowledge.use_gpu) {
      fieldbuf += new ImplField(name + "_host", "", DomainKnowledge.transform_datatype_cpp(datatype), veclength.toInt, DomainKnowledge.ArrayClassName, DomainKnowledge.xsize_L2.getOrElse(1), DomainKnowledge.ysize_L2.getOrElse(1), DomainKnowledge.zsize_L2.getOrElse(1), DomainKnowledge.rule_addpoints(location), 1)
      fieldbuf += new ImplField(name, "", DomainKnowledge.transform_datatype_cpp_cuda(datatype), veclength.toInt, DomainKnowledge.ArrayClassNameGPU, DomainKnowledge.xsize_L2.getOrElse(1), DomainKnowledge.ysize_L2.getOrElse(1), DomainKnowledge.zsize_L2.getOrElse(1), DomainKnowledge.rule_addpoints(location), 1)
    } else
      fieldbuf += new ImplField(name, "", DomainKnowledge.transform_datatype_cpp(datatype), veclength.toInt, DomainKnowledge.ArrayClassName, DomainKnowledge.xsize_L2.getOrElse(1), DomainKnowledge.ysize_L2.getOrElse(1), DomainKnowledge.zsize_L2.getOrElse(1), DomainKnowledge.rule_addpoints(location), 1)

    return fieldbuf
  }
}
