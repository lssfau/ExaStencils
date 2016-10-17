package exastencils.deprecated.domain

import exastencils.domain.ir.IR_Domain

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class RectangularDomain(
    var identifier : String,
    var index : Int,
    var shape : RectangularDomainShape) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class IrregularDomain(
    var identifier : String,
    var index : Int,
    var shape : IrregularDomainShape) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class ShapedDomain(
    var identifier : String,
    var index : Int,
    var shape : ShapedDomainShape) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class FileInputGlobalDomain(
    var identifier : String,
    var index : Int,
    var shape : List[FileInputDomain]) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class FileInputDomain(
    var identifier : String,
    var index : Int,
    var shape : FileInputDomainShape) extends IR_Domain {}
