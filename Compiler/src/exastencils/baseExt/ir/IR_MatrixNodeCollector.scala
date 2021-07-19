package exastencils.baseExt.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.base.ir.IR_WhileLoop
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger


class IR_MatrixVarCollector extends Collector {
  var writes = ListBuffer[scala.collection.mutable.Set[String]]()
  var decls = ListBuffer[mutable.HashMap[String, IR_VariableDeclaration]]()
  //var accs = ListBuffer[mutable.HashMap[String, ]]
  this.reset()

  def openNewScope() = {
    decls += decls.last.clone()
    writes += writes.last.clone()
  }

  def closeScope() = {
    decls.trimEnd(1)
    writes.trimEnd(1)
  }

  def addWrite(dest : IR_Expression) {
    dest match {
      case va : IR_VariableAccess => writes.last += va.name
        //TODO add h
      //case hda : IR_HighDimAccess => writes+=matWriteAccess(hda.uniqueID,hda.index.asInstanceOf)
      case fa : IR_FieldAccess => writes.last += fa.name
      case _                                                                       => Logger.error(s"unexpected type ${dest}")
    }
  }

  def addDecl(d : IR_VariableDeclaration) {
    decls.last += (d.name -> d)
  }

  override def enter(node : Node) : Unit = {
    node match {
      case _ : IR_LoopOverFragments                                                          => openNewScope()
      case _ : IR_ForLoop                                                                    => openNewScope()
      case _ : IR_WhileLoop                                                                  => openNewScope()
      case _ : IR_IfCondition                                                                => openNewScope()
      case _ : IR_Scope                                                                      => openNewScope()
      case _ : IR_Function                                                                   => openNewScope()
      case _ @ IR_Assignment(dest @ IR_VariableAccess(_, _), _, _)                      => addWrite(dest)
      case _ @ IR_Assignment(IR_HighDimAccess(dest @ IR_VariableAccess(_, _), _), _, _) => addWrite(dest)
      case _ @ IR_FunctionCall(_, args)                                                      =>
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
    decls += new mutable.HashMap[String, IR_VariableDeclaration]()
    writes += new mutable.HashSet[String]()
  }

  def lastDecl(key : String) : Option[IR_VariableDeclaration] = {
    var found = false
    var d : Option[IR_VariableDeclaration] = None
    var idx : Int = decls.length - 1
    while (!found && idx >= 0) {
        d = decls(idx).get(key)
        if(d.isDefined) found = true
        idx -= 1
    }
    d
  }

  def writePresent(key : String) : Boolean = {
    writes.last.exists({ s : String => if (s == key) true else false })
  }

  def isConstMatExpr(e : IR_Expression) : Boolean = {
    e match {
      case expression : IR_MatrixExpression =>
        expression.expressions.forall {
          case _ : IR_Number => true
          case _             => false
        }
      case _                                => false
    }
  }

  def getConstInitVal(key : String) : Option[IR_Expression] = {
    var found = false
    var write = false
    var d : Option[IR_VariableDeclaration] = None
    var idx : Int = decls.length - 1
    while (!found && idx >= 0) {
      d = decls(idx).get(key)
      if(d.isDefined) found = true
      write = writePresent(key)
      idx -= 1
    }
    if(write) None
    else {
      if(d.isDefined) {
        val init = d.get.initialValue
        if(init.isEmpty) None
        else {
          init
        }
      } else None
    }
  }
}
