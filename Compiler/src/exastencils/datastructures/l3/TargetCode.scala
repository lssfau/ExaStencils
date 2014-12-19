package exastencils.datastructures.l3

import scala.language.implicitConversions
import exastencils.datastructures._
import scala.collection.mutable._

/** Dynamic computations */
object TargetCode {
  def apply(comps : Node*) = new TargetCode(comps.toList)
}
class TargetCode(val computation : List[Node]) {

  def ++(rhs : TargetCode) : TargetCode = {
    new TargetCode(computation ++ rhs.computation)
  }

  def toList() : List[Node] = computation
}

abstract sealed class DynamicValue extends Value

class DynamicRValue(val tcExpression : l4.Expression, val scType : ScType) extends DynamicValue with RValue {

}
class DynamicLValue(val tcExpression : l4.Expression, val scType : ScType) extends DynamicValue with LValue {
  override def deref = ???
}

case class TcStatement(val stm : l4.Statement)

trait StatementsTcb {
  def build() : List[l4.Statement]
}

object TcbImplicits {

  implicit def TcStatement2Tcb(stm : l4.Statement) = new TcStatementTcb(stm)

}
class TcStatementTcb(val stm : l4.Statement) extends StatementsTcb {
  override def build() : List[l4.Statement] = List(stm)
}

class TcbFunction(val tcId : String, val tcArgs : List[l4.Variable]) extends StatementsTcb {

  val body = new TcbBlock

  def build() : List[l4.Statement] = {
    List(
      new l4.FunctionStatement(
        l4.LeveledIdentifier(tcId, l4.AllLevelsSpecification()),
        l4.UnitDatatype(),
        tcArgs,
        body.build()))
  }
}

/** A block is a list of statements. */
class TcbBlock() {
  private val stms = ListBuffer[StatementsTcb]()

  def +=(stm : StatementsTcb) {
    stms += stm
  }

  def build() : List[l4.Statement] = (stms flatMap { _.build }).toList
}

case class TcUnit() extends l4.Expression {
  def progressToIr : ir.Expression = throw new Exception("This should not be transformed")
  def prettyprint(out : exastencils.prettyprinting.PpStream) = { out << "<<<<<< Unit >>>>>>" }
}

