package exastencils.deprecated.domain

import exastencils.domain.ir.IR_Domain

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class RectangularDomain(
    var name : String,
    var index : Int,
    var shape : RectangularDomainShape) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class IrregularDomain(
    var name : String,
    var index : Int,
    var shape : IrregularDomainShape) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class ShapedDomain(
    var name : String,
    var index : Int,
    var shape : ShapedDomainShape) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class FileInputGlobalDomain(
    var name : String,
    var index : Int,
    var shape : List[FileInputDomain]) extends IR_Domain {}

@deprecated("old code from the 'domain from file' extension -> to be re-integrated", "17.10.16")
case class FileInputDomain(
    var name : String,
    var index : Int,
    var shape : FileInputDomainShape) extends IR_Domain {}
