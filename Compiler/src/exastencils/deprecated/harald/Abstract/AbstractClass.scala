package exastencils.deprecated.harald.Abstract

import exastencils.deprecated.harald.dsl.Param

case class AbstractClass(cname : String, template : Option[String], memlist : List[Param], memflist : List[AbstractFunction])
