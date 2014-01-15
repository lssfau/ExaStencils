package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._

  class AbstractField(val name: String, val datatype: String, val veclength : String, val location: String) {
    override def toString = "ExaField: '" + name + "', type = '" + datatype + "', location = '" + location + "'"

    if (name.equals("solution"))
      DomainKnowledge.datatype_L2 = Some(DomainKnowledge.transform_datatype_cpp(datatype))

    def transform: ListBuffer[ImplField] = {
      var fieldbuf: ListBuffer[ImplField] = ListBuffer()
      if (DomainKnowledge.use_gpu) {
        if ((DomainKnowledge.unknown_L1(0)._1.equals(name)) || DomainKnowledge.function_L1(0)._1.equals(name))
         fieldbuf += new ImplField(name+"_host", DomainKnowledge.transform_datatype_cpp(datatype), veclength.toInt, "MyArray", DomainKnowledge.xsize_L2.getOrElse(1), DomainKnowledge.ysize_L2.getOrElse(1), DomainKnowledge.zsize_L2.getOrElse(1), DomainKnowledge.rule_addpoints(location),1)
         fieldbuf += new ImplField(name, DomainKnowledge.transform_datatype_cpp_cuda(datatype), veclength.toInt,"MyArrayCuda",DomainKnowledge.xsize_L2.getOrElse(1), DomainKnowledge.ysize_L2.getOrElse(1), DomainKnowledge.zsize_L2.getOrElse(1), DomainKnowledge.rule_addpoints(location),1)
      } else
        fieldbuf += new ImplField(name, DomainKnowledge.transform_datatype_cpp(datatype), veclength.toInt,"MyArray",DomainKnowledge.xsize_L2.getOrElse(1), DomainKnowledge.ysize_L2.getOrElse(1), DomainKnowledge.zsize_L2.getOrElse(1), DomainKnowledge.rule_addpoints(location),1)

      return fieldbuf
    }
  }
