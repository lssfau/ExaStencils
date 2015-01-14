package exastencils.datastructures.l3

import TcbImplicits._
import exastencils.datastructures.l4
import exastencils.core.Logger

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
        val f = destField.lEval(ctx) match {
          case FieldLValue(fieldId) => fieldId
          case _ =>
            throw new Exception("First parameter of apply needs to be a field.")
        }

        val A = stencilArg.rEval(ctx) match {
          case stencil : StaticListRValue => stencil
          case _                          => throw new Exception("Second argument to apply is not a stencil")
        }

        val u = sourceField.rEval(ctx) match {
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
      case _ => Logger.error("Apply takes three arguments but %d were given.".format(args.length))

    }
  }

  override def staticApplication(ctx : Context, args : List[Expression]) : StaticValue = {
    ???
  }
}

case class DiagInvBuiltin() extends StaticRValue with AbstractFunctionRValue {
  def scReturnType = StaticListDatatype()

  override def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression = {
    throw new Exception("The function diag_inv can only be applied at compile time")
  }

  override def staticApplication(ctx : Context, args : List[Expression]) : StaticValue = {
    args match {
      case List(arg1) =>

        val inputStencil = arg1.rEval(ctx) match {
          case s : StaticListRValue => s
        }
        Stencil(inputStencil).diagInv().toSc()
    }
  }
}

case class InstantiateFieldBuiltin() extends StaticRValue with AbstractFunctionRValue {
  def scReturnType = FieldDatatype()

  override def writeTcApplication(ctx : Context, args : List[Expression]) : l4.Expression = {
    throw new Exception("Fields can only be instantiated at compile time")
  }

  override def staticApplication(ctx : Context, args : List[Expression]) : StaticValue = {
    args match {
      case List() =>
        val id = ctx.fields.add()
        FieldLValue(id)
      case _ => throw new Exception("The function field takes no arguments.")
    }
  }
}
