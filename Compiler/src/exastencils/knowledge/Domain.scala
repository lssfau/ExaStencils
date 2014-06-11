package exastencils.knowledge

import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.util._

case class Domain(var identifier : String,
  var index : Int,
  var size : AABB) {}

object DomainCollection {
  var domains : ListBuffer[Domain] = ListBuffer()

  def getDomainByIdentifier(identifier : String) : Option[Domain] = {
    domains.find(d => d.identifier == identifier)
  }
}