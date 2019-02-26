package exastencils.communication.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.domain.ir.IR_IV_FragmentConnection
import exastencils.prettyprinting.PpStream

/// IR_IV_CommTrafoId

case class IR_IV_CommTrafoId(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  //TODO add field.level to prettyprint() and resolveName()
  // They are actually not needed because commmunication is independent of field.level

  //override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, field.level, neighIdx)
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  //override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", field.level.toString, neighIdx.prettyprint)
  override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)

}

case class IR_IV_NeighFragId(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  //TODO add field.level to prettyprint() and resolveName()
  // They are actually not needed because commmunication is independent of field.level

  //override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, field.level, neighIdx)
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  //override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", field.level.toString, neighIdx.prettyprint)
  override def resolveName() = s"neighFragId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)
}

// Index of the neighbor inside the communicating fragment
case class IR_IV_CommNeighIdx(var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection {
  //TODO add field.level to prettyprint() and resolveName()
  // They are actually not needed because commmunication is independent of field.level

  //override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, field.level, neighIdx)
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  //override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", field.level.toString, neighIdx.prettyprint)
  override def resolveName() = s"commNeighIdx" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)
}

object IR_CommTrafoCollection {
  val trafoArray : Array[List[(Int, Int)]] = Array(
    List((0, 1), (2, 3), (1, 0), (3, 2)), // 0
    List((0, 3), (2, 0), (1, 2), (3, 1)), // 1
    List((0, 0), (2, 2), (1, 1), (3, 3)), // 2
    List((0, 2), (2, 1), (1, 3), (3, 0)) // 3
  )
}