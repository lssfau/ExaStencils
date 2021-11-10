package exastencils.visualization.ir.visit

import exastencils.base.ir.IR_FuturePlainFunction

trait IR_VisItFuturePlainFunction extends IR_FuturePlainFunction {

  allowInlining = false

  override def name_=(newName : String) : Unit = name = newName
  override def prettyprint_decl() : String = prettyprint()
}
