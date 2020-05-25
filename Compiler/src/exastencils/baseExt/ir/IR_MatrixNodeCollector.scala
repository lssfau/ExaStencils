package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_WhileLoop
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node

class IR_MatrixWriteCollector extends Collector {
  var writes = ListBuffer[ListBuffer[String]]()

  def openNewScope() = {
    writes += writes.last.clone()
  }

  def closeScope() = {
    writes.trimEnd(1)
  }

  def addWrite(destname : String) {
    writes.last += destname
  }

  override def enter(node : Node) : Unit = {
    node match {
      case _ : IR_LoopOverFragments                                                          => openNewScope()
      case _ : IR_ForLoop                                                                    => openNewScope()
      case _ : IR_WhileLoop                                                                  => openNewScope()
      case _ : IR_IfCondition                                                                => openNewScope()
      case _ : IR_Scope                                                                      => openNewScope()
      case _ : IR_Function                                                                   => openNewScope()
      case assign @ IR_Assignment(dest @ IR_VariableAccess(_, _), _, _)                      => addWrite(dest.name)
      case assign @ IR_Assignment(IR_HighDimAccess(dest @ IR_VariableAccess(_, _), _), _, _) => addWrite(dest.name)
      case c @ IR_FunctionCall(_, args)                                                      =>
        args.foreach(a => a match {
          case va @ IR_VariableAccess(_, _) => addWrite(va.name)
          case _                            =>
        })
      case _                                                                                 =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : IR_Scope             => closeScope()
      case _ : IR_Function          => closeScope()
      case _ : IR_LoopOverFragments => closeScope()
      case _ : IR_ForLoop           => closeScope()
      case _ : IR_WhileLoop         => closeScope()
      case _ : IR_IfCondition       => closeScope()
      case _                        =>
    }
  }

  override def reset() : Unit = {
    writes.clear()
    writes += new ListBuffer()
  }

  def writeInScope(key : String) : Boolean = {
    //TODO check not this write?
    writes.last.find(p => p == key) != None
  }

}
