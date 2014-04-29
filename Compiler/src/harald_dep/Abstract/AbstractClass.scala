package harald_dep.Abstract

import harald_dep.dsl.Param

case class AbstractClass(cname: String, template: Option[String], memlist: List[Param], memflist: List[AbstractFunction]) 
