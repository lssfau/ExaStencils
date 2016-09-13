package exastencils.base.l4

import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting._

trait L4_Expression extends L4_Node with L4_Progressable with PrettyPrintable {
  def progress : IR_Expression
}
