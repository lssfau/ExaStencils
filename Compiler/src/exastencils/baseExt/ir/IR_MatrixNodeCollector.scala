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
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.base.ir.IR_WhileLoop
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger

class IR_MatrixVarCollector extends Collector {
  var writes = ListBuffer[String]()
  var decls = ListBuffer[ListBuffer[IR_VariableDeclaration]]()
  this.reset()

  def openNewScope() = {
    decls += decls.last.clone()
  }

  def closeScope() = {
    decls.trimEnd(1)
  }

  def addWrite(dest : IR_Expression) {
    dest match {
      case va : IR_VariableAccess => writes += va.name
      case fa : IR_FieldAccess => writes += fa.name
      // access to matrix variable transformed to a matrix expression
      case x : IR_MatrixExpression if (x.get(0, 0).isInstanceOf[IR_HighDimAccess]) => writes += x.get(0, 0).asInstanceOf[IR_HighDimAccess].uniqueID
      case _                                                                       => Logger.error(s"unexpected type ${dest}")
    }
  }

  def addDecl(d : IR_VariableDeclaration) {
    decls.last += d
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
      case d : IR_VariableDeclaration                                                        => addDecl(d)
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
    decls.clear()
    decls += new ListBuffer[IR_VariableDeclaration]()
  }

  def lastDecl(key : String) : Option[IR_VariableDeclaration] = {
    var d = decls.last.find(p => p.name == key)
    var idx : Int = decls.length - 1
    while (d.isEmpty && idx >= 0) {
      d = decls(idx).find(p => p.name == key)
      idx -= 1
    }
    d
  }

  def writeInScope(key : String) : Boolean = {
    //TODO check not this write?
    return writes.find(p => p == key) != None
  }

}
