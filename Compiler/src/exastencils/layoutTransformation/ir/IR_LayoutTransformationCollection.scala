package exastencils.layoutTransformation.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Node

case class IR_LayoutTransformationCollection(var trafoStmts : ListBuffer[IR_LayoutTransformStatement]) extends IR_Node
