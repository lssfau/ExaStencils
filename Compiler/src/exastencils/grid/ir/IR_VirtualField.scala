package exastencils.grid.ir

import exastencils.base.ir._
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject

/// IR_VirtualField

case class IR_VirtualField(
    var name : String,
    var level : Int,
    var domain : IR_Domain,
    var datatype : IR_Datatype,
    var localization : String) extends IR_LeveledKnowledgeObject
