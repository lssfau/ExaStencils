package test.datastructures.ir

import exastencils.base.ir._

class Expressions {
  def testNumbers = {

    import exastencils.base.ir.IR_ImplicitConversion._

    var x : IR_Expression = IR_RealConstant(40) + IR_IntegerConstant(2)
    x = x + 3
    x = 3 * 7
    x = x / 2.5

    var y = 21 + IR_RealConstant(1)
  }
}
