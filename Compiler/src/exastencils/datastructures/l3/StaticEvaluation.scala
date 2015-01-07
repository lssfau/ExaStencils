package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4
import exastencils.core.Logger
import exastencils.math._

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

  def staticApplication(env : Environment, args : List[Expression]) : StaticValue
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

    val body_ctx = new Context(body_env, body_tcb, ctx.stencils)

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

  override def staticApplication(env : Environment, args : List[Expression]) : StaticValue = {
    ???
  }
}

case class ApplyStencilBuiltin() extends StaticRValue with AbstractFunctionRValue {
  def scReturnType = FieldDatatype()

  override def writeTcApplication(
    ctx : Context,
    args : List[Expression]) : l4.Expression = {

    import ctx.env
    import ctx.tcb

    // expect precisely three arguments
    args match {
      case List(destField, stencilArg, sourceField) =>
        val f = destField.lEval(env) match {
          case FieldLValue(fieldId) => fieldId
          case _ =>
            throw new Exception("First parameter of apply needs to be a field.")
        }

        val A = stencilArg.rEval(env) match {
          case stencil : StaticListRValue => stencil
          case _                          => throw new Exception("Second argument to apply is not a stencil")
        }

        val u = sourceField.rEval(env) match {
          case FieldRValue(fieldId) => fieldId
          case _ =>
            throw new Exception("Third parameter of apply needs to be a field")
        }

        val A_id = ctx.stencils.add(A)

        val A_access = new l4.StencilAccess(A_id, l4.CurrentLevelSpecification())
        val u_access = new l4.FieldAccess(u,
          l4.CurrentLevelSpecification(),
          new l4.IntegerConstant(0))

        val f_access = new l4.FieldAccess(f,
          l4.CurrentLevelSpecification(),
          new l4.IntegerConstant(0))

        val convExpr = new l4.StencilConvolution(A_access, u_access)

        tcb += new l4.AssignmentStatement(f_access, convExpr, "=")

        TcUnit() // no return value
      case _ => Logger.error("Apply takes three arguments but %d were given".format(args.length))

    }
  }

  override def staticApplication(env : Environment, args : List[Expression]) : StaticValue = {
    ???
  }
}

case class DiagInvBuiltin() extends StaticRValue with AbstractFunctionRValue {
  def scReturnType = StaticListDatatype()

  override def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression = {
    ???
  }

  override def staticApplication(env : Environment, args : List[Expression]) : StaticValue = {
    args match {
      case List(arg1) =>

        val inputStencil = arg1.rEval(env) match {
          case s : StaticListRValue => s
        }
        Stencil(inputStencil).diagInv().toSc()
    }
  }
}

/* =================================================================== */

case class StaticListRValue(val elements : List[StaticValue]) extends StaticRValue {
  def scType = StaticListDatatype()
}

