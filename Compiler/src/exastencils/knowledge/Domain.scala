package exastencils.knowledge

import exastencils.domain._
import exastencils.domain.ir.IR_Domain

case class RectangularDomain(
    var identifier : String,
    var index : Int,
    var shape : RectangularDomainShape) extends IR_Domain {}

case class IrregularDomain(
    var identifier : String,
    var index : Int,
    var shape : IrregularDomainShape) extends IR_Domain {}

case class ShapedDomain(
    var identifier : String,
    var index : Int,
    var shape : ShapedDomainShape) extends IR_Domain {}

case class FileInputGlobalDomain(
    var identifier : String,
    var index : Int,
    var shape : List[FileInputDomain]) extends IR_Domain {}

case class FileInputDomain(
    var identifier : String,
    var index : Int,
    var shape : FileInputDomainShape) extends IR_Domain {}
