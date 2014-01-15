package harald.Abstract

import harald.dsl.Param

case class AbstractClass(cname: String, template: Option[String], memlist: List[Param], memflist: List[AbstractFunction]) 
