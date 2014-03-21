package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._

class AbstractField(val name : String, val datatype : String, val veclength : String, val location : String) {
  override def toString = "ExaField: '" + name + "', type = '" + datatype + "', location = '" + location + "'"

  if (name.equals("solution"))
    DomainKnowledge.datatype_L2 = Some(DomainKnowledge.transform_datatype_cpp(datatype))
}
