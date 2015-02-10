package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.logger._
import exastencils.util._

case class Domain(var identifier : String,
  var index : Int,
  var size : AABB) {}

object DomainCollection {
  var domains : ListBuffer[Domain] = ListBuffer()

  def getDomainByIdentifier(identifier : String) : Option[Domain] = {
    val ret = domains.find(d => d.identifier == identifier)
    if (ret.isEmpty) Logger.warn(s"Domain $identifier was not found")
    ret
  }
}
