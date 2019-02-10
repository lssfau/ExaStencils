package exastencils.communication.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.domain.ir.IR_IV_FragmentConnection
import exastencils.prettyprinting.PpStream

case class IR_IV_CommTrafoId (var domain : IR_Expression, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_FragmentConnection{
  //TODO add field.level to prettyprint() and resolveName()
  // They are actually not needed because commmunication is independent of field.level

  //override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, field.level, neighIdx)
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, neighIdx)

  //override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", field.level.toString, neighIdx.prettyprint)
  override def resolveName() = s"commTrafoId" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(false)

}

object IR_CommTrafoCollection{
  val trafoArray : Array[List[(Int, Int)]] = Array(
    List((0,1),(2,3),(1,0),(3,2)),
    List((0,3),(2,0),(1,2),(3,1)),
    List((0,0),(2,2),(1,1),(3,3)),
    List((0,2),(2,1),(1,3),(3,0))
    )
}

//object IR_CommTrafoCollectionNew{
//  // cascade this into Array[Array[Int]]
//  val trafoArray : Array[(Int, Int)] = Array(
//    (0,1),(2,3),(1,0),(3,2),    //  0  1  2  3
//    (0,3),(2,0),(1,2),(3,1),    //  4  5  6  7
//    (0,0),(2,2),(1,1),(3,3),    //  8  9 10 11
//    (0,2),(2,1),(1,3),(3,0)     // 12 13 14 15
//  )
//}