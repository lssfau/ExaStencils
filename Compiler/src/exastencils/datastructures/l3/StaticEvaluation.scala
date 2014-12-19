package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4
import exastencils.core.Logger

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

  def writeTcApplication(env : Environment, block : TcbBlock, args : List[Expression]) : l4.Expression
}

/** User defined functions. */
case class FunctionRValue(
  val id : String,
  val scReturnType : ScType,
  val arguments : List[Variable],
  val body : List[Statement])
    extends StaticRValue with AbstractFunctionRValue {

  // runtime arguments
  def dynamicArguments : List[Variable] = {
    // return only dynamic arguments
    arguments filter { a => !a.datatype.isStatic }
  }

  def staticArguments : List[Variable] = {
    arguments filter { a => a.datatype.isStatic }
  }

  def mangleName(args : List[StaticValue]) : String = ???

  /** Return the target code of an instance of this function. */
  def writeTcInstance(env : Environment, block : TcbBlock, givenStaticArgs : List[StaticValue], predefinedTcId : Option[String]) {

    val tcId = predefinedTcId match {
      case Some(i) => i
      case None    => mangleName(givenStaticArgs)
    }

    val tcArgs = dynamicArguments map {
      case Variable(id, scType) =>
        l4.Variable(l4.BasicIdentifier(id), scType.toTcType)
    }

    // bind function arguments
    val body_env = new Environment(Some(env))
    for ((givenArg, requestedArg) <- givenStaticArgs zip staticArguments) {
      body_env.bind(requestedArg.id, givenArg)
    }

    // transform to target code and concat
    val funTc = new TcbFunction(tcId, tcArgs)
    body foreach { _.writeTc(body_env, funTc.body) }

    block += funTc
  }

  def writeTcApplication(env : Environment, block : TcbBlock, args : List[Expression]) : l4.Expression = {
    ???
  }
}

case class ApplyStencilBuiltin() extends StaticRValue with AbstractFunctionRValue {
  def scReturnType = FieldDatatype()

  override def writeTcApplication(
    env : Environment,
    block : TcbBlock,
    args : List[Expression]) : l4.Expression = {

    // expect precisely three arguments
    args match {
      case List(destField, stencilArg, sourceField) =>
        val f = destField.lEval(env) match {
          case FieldLValue(fieldId) => fieldId
          case _ =>
            throw new Exception("First parameter of apply needs to be a field.")
        }
        val A = stencilArg.rEval(env)
        val u = sourceField.rEval(env) match {
          case FieldRValue(fieldId) => fieldId
          case _ =>
            throw new Exception("Third parameter of apply needs to be a field")
        }

        val A_access = new l4.StencilAccess("TODOresolve", l4.CurrentLevelSpecification())
        val u_access = new l4.FieldAccess(u,
          l4.CurrentLevelSpecification(),
          new l4.IntegerConstant(0),
          -1) // FIXME@Christian: array index

        val f_access = new l4.FieldAccess(f,
          l4.CurrentLevelSpecification(),
          new l4.IntegerConstant(0),
          -1) // FIXME@Christian: array index

        val convExpr = new l4.StencilConvolution(A_access, u_access)

        block += new l4.AssignmentStatement(f_access, convExpr, "=")

        TcUnit() // no return value
      case _ => Logger.error("Apply takes three arguments but %d were given".format(args.length))

    }
  }

}

