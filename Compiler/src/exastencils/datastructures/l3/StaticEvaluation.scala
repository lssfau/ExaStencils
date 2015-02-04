package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4
import exastencils.logger._
import scala.collection.mutable._

/** Static values. */
trait StaticValue extends Value

case class NilStaticValue() extends StaticValue {
  override def scType = NilDatatype()
}

/**
  * This is a store for a static value.
  *  This corresponds to a cell in Lisp, e.g.
  */
class StaticLocation(var value : StaticValue = NilStaticValue()) {
  //override
  def scType = LocationDatatype()

  def write(newValue : StaticValue) {
    value = newValue
  }

  def read() = value
}

case class StaticConstant(v : StaticValue = NilStaticValue()) extends StaticLocation(v) {
  override def write(newValue : StaticValue) {
    throw new Exception("Attempt to update a constant value.")
  }
}

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
      l4.SlotModifier.Constant(0))

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
      l4.SlotModifier.Constant(0))
  }
}

trait AbstractFunctionRValue {
  def scType = FunctionDatatype()

  def scReturnType : ScType

  def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression

  def staticApplication(ctx : Context, args : List[Expression]) : StaticLocation
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

  private def createBodyContext(ctx : Context, givenStaticArgs : List[StaticLocation]) = {

    // bind static function arguments
    val body_ctx = ctx.createNewScope()
    for ((givenArg, requestedArg) <- givenStaticArgs zip staticArguments) {
      body_ctx.env.bind(requestedArg.id, givenArg)
    }

    body_ctx
  }

  /** Return the target code of an instance of this function. */
  def writeTcInstance(
    ctx : Context,
    givenStaticArgs : List[StaticLocation],
    predefinedTcId : Option[String]) {

    val tcId = predefinedTcId match {
      case Some(i) => i
      case None    => mangleName(givenStaticArgs map { _.read })
    }

    val tcArgs = dynamicArguments map {
      case FunctionArgument(id, scType) =>
        l4.Variable(l4.BasicIdentifier(id), scType.toTcType)
    }

    val body_ctx = createBodyContext(ctx, givenStaticArgs)

    // bind dynamic arguments
    for (arg <- dynamicArguments) {
      body_ctx.env.bind(arg.id, StaticConstant(arg.datatype.createDynamicLocation(body_ctx)))
    }

    // transform to target code and concat
    body foreach { _.writeTc(body_ctx) }

    // write the function
    ctx.tcb += new l4.FunctionStatement(
      l4.LeveledIdentifier(tcId, l4.AllLevelsSpecification()),
      l4.UnitDatatype(),
      tcArgs,
      body_ctx.tcb.build())
  }

  /** This function has only static arguments. */
  def isStatic : Boolean = dynamicArguments.isEmpty

  override def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression = {
    ???
  }

  override def staticApplication(ctx : Context, args : List[Expression]) : StaticLocation = {
    if (!isStatic) {
      throw new Exception("Static evaluation of function %s which is not static.".format(id))
    }

    val evaluated_args = args map { a => a.eval(ctx) }

    val body_ctx = createBodyContext(ctx, evaluated_args)

    for (stm <- body) {
      stm.exec(ctx) match {
        case Some(v) => return StaticConstant(v)
        case None    =>
      }
    }

    StaticConstant()
  }
}

/* =================================================================== */

case class ListStaticValue(es : List[StaticValue]) extends StaticValue {
  def scType = StaticListDatatype()

  val elements = ArrayBuffer[StaticValue]()
  elements.appendAll(es)

  def append(x : StaticValue) {
    elements.append(x)
  }
}

