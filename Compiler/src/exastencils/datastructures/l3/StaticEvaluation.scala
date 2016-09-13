package exastencils.datastructures.l3

import scala.collection.mutable._

import exastencils.base.l4.L4_UnitDatatype
import exastencils.datastructures.l3.TcbImplicits._
import exastencils.datastructures.l4

/** Static values. */
trait StaticValue extends Value

case class NilStaticValue() extends StaticValue {
  override def scType = NilDatatype()
}

case class StaticInteger(n : Int) extends StaticValue {
  override def scType = IntegerDatatype()
}

case class StaticReal(n : Double) extends StaticValue {
  override def scType = RealDatatype()
}

/* =================================================================== */

trait AbstractFunctionRValue {
  def scType = FunctionDatatype()

  def scReturnType : ScType

  def writeTcApplication(ctx : Context, args : List[Expression]) : DynamicLocation

  def staticApplication(ctx : Context, args : List[Expression]) : StaticLocation
}

/** User defined functions. */
case class FunctionRValue(
    val id : String,
    val scReturnType : ScType,
    val arguments : List[FunctionArgument],
    val body : List[Statement])
  extends StaticValue with AbstractFunctionRValue {

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

    val body_ctx = createBodyContext(ctx, givenStaticArgs)

    // bind dynamic arguments
    val tcArgs = ListBuffer[l4.FunctionArgument]()
    for (arg <- dynamicArguments) {
      val argLocation = arg.datatype.createDynamicLocation(body_ctx)
      body_ctx.env.bind(arg.id, StaticConstant(argLocation))

      tcArgs += l4.FunctionArgument(l4.BasicIdentifier(argLocation.argumentTc.access.name), argLocation.argumentTc.datatype)
    }

    // transform to target code and concat
    body foreach { _.writeTc(body_ctx) }

    // write the function
    ctx.tcb += new l4.FunctionStatement(
      l4.LeveledIdentifier(tcId, l4.AllLevelsSpecification),
      L4_UnitDatatype,
      tcArgs.toList,
      body_ctx.tcb.build())
  }

  /** This function has only static arguments. */
  def isStatic : Boolean = dynamicArguments.isEmpty

  override def writeTcApplication(ctx : Context, args : List[Expression]) : DynamicLocation = {
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

