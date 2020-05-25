package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_WhileLoop
import exastencils.config.Knowledge
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger.Logger

class IR_MatrixWriteCollector extends Collector {
  var writes = ListBuffer[ListBuffer[String]]()
  this.reset()

  def openNewScope() = {
    writes += writes.last.clone()
  }

  def closeScope() = {
    writes.trimEnd(1)
  }

  def addWrite(dest : IR_Expression) {
    dest match {
      case va : IR_VariableAccess => writes.last += va.name
        // access to matrix variable transformed to a matrix expression
      case x : IR_MatrixExpression if(x.get(0,0).isInstanceOf[IR_HighDimAccess]) => writes.last += x.get(0,0).asInstanceOf[IR_HighDimAccess].uniqueID
      case _ => Logger.error("unexpected type")
    }
  }

  override def enter(node : Node) : Unit = {
    node match {
      case _ : IR_LoopOverFragments                                                          => openNewScope()
      case _ : IR_ForLoop                                                                    => openNewScope()
      case _ : IR_WhileLoop                                                                  => openNewScope()
      case _ : IR_IfCondition                                                                => openNewScope()
      case _ : IR_Scope                                                                      => openNewScope()
      case _ : IR_Function                                                                   => openNewScope()
      case assign @ IR_Assignment(dest @ IR_VariableAccess(_, _), _, _)                      => addWrite(dest)
      case assign @ IR_Assignment(IR_HighDimAccess(dest @ IR_VariableAccess(_, _), _), _, _) => addWrite(dest)
      case c @ IR_FunctionCall(_, args)                                                      =>
        args.foreach(a => a match {
          case va @ IR_VariableAccess(_, _) => addWrite(va)
          case _                            =>
        })
      case det @ IR_Determinant(arg, _) if (Knowledge.experimental_inplaceDeterminant)       => addWrite(arg)
      case inv @ IR_Inverse(arg, _, _, _) if (Knowledge.experimental_inplaceInversion)       => addWrite(arg)
      case s @ IR_SetElement(arg)                                                            => addWrite(arg(0))
      case s @ IR_SetSlice(arg)                                                              => addWrite(arg(0))
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
    writes += new ListBuffer[String]()
  }

  def writeInScope(key : String) : Boolean = {
    //TODO check not this write?
    writes.last.find(p => p == key) != None
  }

}
