package test.datastructures.ir

import exastencils.datastructures.ir._

class Expressions {
  def testNumbers = {

    import exastencils.datastructures.ir.ImplicitConversions._

    var x : Expression = FloatConstant(40) + IntegerConstant(2)
    x = x + 3
    x = 3 * 7
    x = x / 2.5

    var y = 21 + FloatConstant(1)
  }
}
