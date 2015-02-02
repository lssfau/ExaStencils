package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4
import exastencils.logger._

/** Static values. */
trait StaticValue extends Value

/** Static locations */
trait StaticLValue extends StaticValue with LValue {
  def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue)
}

case class IntegerLValue(tcId : String) extends StaticLValue {

  override def scType = IntegerDatatype()

  def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue) {
    val tcAccess = new l4.BasicAccess(tcId)
    block += l4.AssignmentStatement(tcAccess, tcRhs.tcExpression, "=")
  }

  override def deref = ???
}

case class FieldLValue(tcId : String) extends StaticLValue {

  override def scType = FieldDatatype()

  override def writeTcAssignment(block : TcbBlock, tcRhs : DynamicRValue) {

    val tcAccess = new l4.FieldAccess(
      tcId,
      l4.CurrentLevelSpecification(),
      l4.IntegerConstant(0))

    block += l4.AssignmentStatement(tcAccess, tcRhs.tcExpression, "=")
  }

  override def deref = FieldRValue(tcId)
}

/* =================================================================== */

/** Static values */
trait StaticRValue extends StaticValue with RValue

case class IntegerRValue(v : Int) extends StaticRValue {
  override def scType = IntegerDatatype()
}
case class FloatRValue(v : Double) extends StaticRValue {
  override def scType = RealDatatype()
}
case class StencilRValue() extends StaticRValue {
  override def scType = StencilDatatype()
}

case class FieldRValue(tcId : String) extends StaticRValue {

  override def scType = FieldDatatype()

  def toTc() : l4.Expression = {
    l4.FieldAccess(
      tcId,
      l4.CurrentLevelSpecification(),
      l4.IntegerConstant(0))
  }
}

trait AbstractFunctionRValue {
  def scType = FunctionDatatype()

  def scReturnType : ScType

  def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression

  def staticApplication(ctx : Context, args : List[Expression]) : StaticValue
}

/** User defined functions. */
case class FunctionRValue(
  val id : String,
  val scReturnType : ScType,
  val arguments : List[FunctionArgument],
  val body : List[Statement])
    extends StaticRValue with AbstractFunctionRValue {

  // runtime arguments
  def dynamicArguments : List[FunctionArgument] = {
    // return only dynamic arguments
    arguments filter { a => !a.datatype.isStatic }
  }

  def staticArguments : List[FunctionArgument] = {
    arguments filter { a => a.datatype.isStatic }
  }

  def mangleName(args : List[StaticValue]) : String = ???

  /** Return the target code of an instance of this function. */
  def writeTcInstance(
    ctx : Context,
    givenStaticArgs : List[StaticValue],
    predefinedTcId : Option[String]) {

    val tcId = predefinedTcId match {
      case Some(i) => i
      case None    => mangleName(givenStaticArgs)
    }

    val tcArgs = dynamicArguments map {
      case FunctionArgument(id, scType) =>
        l4.Variable(l4.BasicIdentifier(id), scType.toTcType)
    }

    // bind function arguments
    val body_env = new Environment(Some(ctx.env))
    for ((givenArg, requestedArg) <- givenStaticArgs zip staticArguments) {
      body_env.bind(requestedArg.id, givenArg)
    }

    // tcb for the function body
    val body_tcb = new TcbBlock()

    val body_ctx = new Context(body_env, body_tcb, ctx.stencils, ctx.fields)

    // transform to target code and concat
    body foreach { _.writeTc(body_ctx) }

    // write the function
    ctx.tcb += new l4.FunctionStatement(
      l4.LeveledIdentifier(tcId, l4.AllLevelsSpecification()),
      l4.UnitDatatype(),
      tcArgs,
      body_tcb.build())
  }

  override def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression = {
    ???
  }

  override def staticApplication(ctx : Context, args : List[Expression]) : StaticValue = {
    ???
  }
}

/* =================================================================== */

case class StaticListRValue(val elements : List[StaticValue]) extends StaticRValue {
  def scType = StaticListDatatype()
}

