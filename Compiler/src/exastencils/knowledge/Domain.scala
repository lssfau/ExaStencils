package exastencils.knowledge

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.util._

case class Domain(var identifier : String,
    var size : AABB) extends Node {}

case class DomainCollection(var domains : ListBuffer[Domain] = ListBuffer()) extends Node {
  def getDomainByIdentifier(identifier : String) : Option[Domain] = {
    domains.find(d => d.identifier == identifier)
  }
}