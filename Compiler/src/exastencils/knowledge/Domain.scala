package exastencils.knowledge

import scala.collection.mutable.ListBuffer

import exastencils.logger._
import exastencils.util._
import exastencils.domain._

trait Domain {
  def identifier : String
  def index : Int
  def shape : Any
}

case class RectangularDomain(
  var identifier : String,
  var index : Int,
  var shape : RectangularDomainShape) extends Domain {}

case class IrregularDomain(
  var identifier : String,
  var index : Int,
  var shape : IrregularDomainShape) extends Domain {}

case class ShapedDomain(
  var identifier : String,
  var index : Int,
  var shape : ShapedDomainShape) extends Domain {}

case class FileInputGlobalDomain(
  var identifier : String,
  var index : Int,
  var shape : List[FileInputDomain]) extends Domain {}

case class FileInputDomain(
  var identifier : String,
  var index : Int,
  var shape : FileInputDomainShape) extends Domain {}

object DomainCollection {
  var domains : ListBuffer[Domain] = ListBuffer()

  def getDomainByIdentifier(identifier : String) : Option[Domain] = {
    val ret = domains.find(d => d.identifier == identifier)
    if (ret.isEmpty) Logger.warn(s"Domain $identifier was not found")
    ret
  }

  def initFragments() = {
    if (Knowledge.domain_readFromFile) {
      domains
        .find { d => d.identifier == "global" } match {
          case Some(n) => n.shape.asInstanceOf[List[FileInputDomain]].foreach { f => f.shape.asInstanceOf[FileInputDomainShape].initFragments() }
          case None    => Logger.error("There is no domain labeled \"global\"")
        }
    } else if (Knowledge.domain_useCase != "") {
      domains
        .find { d => d.identifier != "global" } match {
          case Some(n) => n.shape.asInstanceOf[ShapedDomainShape].initFragments()
          case None    => Logger.error("There is no domain labeled \"global\"")
        }
    } else {
      domains
        .find { d => d.identifier == "global" } match {
          case Some(n) => n.shape.asInstanceOf[DomainShape].initFragments()
          case None    => Logger.error("There is no domain labeled \"global\"")
        }
    }

  }
}
