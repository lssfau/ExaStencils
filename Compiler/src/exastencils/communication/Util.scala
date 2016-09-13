package exastencils.communication

import exastencils.base.ir.IR_SpecialDatatype
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.prettyprinting._

case class GeneratedMPITag(var from : Expression, var to : Expression, var dirOfSend : Int, var concurrencyId : Int) extends Expression with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = RemoteSend\n"

  def expand : Output[Expression] = {
    // ("((unsigned int)" ~ from ~ " << 20)") + ("((unsigned int)(" ~ to ~ ") << 10)") + concurrencyId
    //CastExpression(SpecialDatatype("unsigned int"), from << IntegerConstant(20)) + CastExpression(SpecialDatatype("unsigned int"), to << IntegerConstant(10)) + concurrencyId
    (CastExpression(IR_SpecialDatatype("unsigned int"), from << IntegerConstant(31 - 8))
      + CastExpression(IR_SpecialDatatype("unsigned int"), to << IntegerConstant(31 - 16))
      + CastExpression(IR_SpecialDatatype("unsigned int"), dirOfSend << IntegerConstant(31 - 21))
      + concurrencyId)
  }
}
