package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.optimization.ir._

/// IR_ResolveLocalSolve

object IR_ResolveLocalSolve extends DefaultStrategy("Resolve IR_LocalSolve nodes") {
  this += new Transformation("Perform expandSpecial for applicable nodes", {
    case solve : IR_LocalSolve => solve.expandSpecial
  })
}

/// IR_Equation

// TODO: move eq node to more fitting file/package
case class IR_Equation(var lhs : IR_Expression, var rhs : IR_Expression) extends IR_Node

/// IR_LocalSolve

case class IR_LocalSolve(var unknowns : ListBuffer[IR_FieldAccess], var equations : ListBuffer[IR_Equation], var relax : Option[IR_Expression]) extends IR_Statement with IR_SpecialExpandable {
  var fVals = ListBuffer[IR_Addition]()
  var AVals = ListBuffer[ListBuffer[IR_Addition]]()

  def matchUnknowns(other : IR_FieldAccess) : Int = {
    for (i <- unknowns.indices)
      if (other.fieldSelection.field.codeName == unknowns(i).fieldSelection.field.codeName && other.index == unknowns(i).index)
        return i // match

    -1 // no match => constant value
  }

  object IR_ContainsUnknownAccesses extends QuietDefaultStrategy("Check for (field) accesses to unknowns") {
    var found : Boolean = false

    def hasSome(node : Node) : Boolean = {
      found = false
      applyStandalone(node)
      found
    }

    this += new Transformation("Match field accesses", {
      case access : IR_FieldAccess if matchUnknowns(access) >= 0 =>
        found = true
        access
    })
  }

  def processExpression(pos : Int, ex : IR_Expression, switchSign : Boolean) : Unit = {
    ex match {
      case const : IR_Number =>
        fVals(pos).summands += (if (switchSign) const else IR_Negative(const))

      case const : IR_Expression if !IR_ContainsUnknownAccesses.hasSome(IR_ExpressionStatement(const)) =>
        // generic expression not relying on field accesses to unknown values => handle as const
        fVals(pos).summands += (if (switchSign) const else IR_Negative(const))

      case IR_Negative(exp) =>
        processExpression(pos, exp, !switchSign)

      case add : IR_Addition =>
        add.summands.foreach(ex => processExpression(pos, ex, switchSign))

      case sub : IR_Subtraction =>
        processExpression(pos, sub.left, switchSign)
        processExpression(pos, sub.right, !switchSign)

      case access : IR_FieldAccess =>
        val uPos = matchUnknowns(access)
        if (uPos < 0)
          fVals(pos).summands += (if (switchSign) access else IR_Negative(access)) // no match -> rhs
        else
          AVals(pos)(uPos).summands += IR_RealConstant(if (switchSign) -1 else 1) // match -> matrix

      case mult : IR_Multiplication =>
        // split into known and unknown
        var localFactors = ListBuffer[IR_Expression]()
        var localUnknowns = ListBuffer[IR_FieldAccess]()
        for (ex <- mult.factors) {
          ex match {
            case const : IR_Expression if !IR_ContainsUnknownAccesses.hasSome(IR_ExpressionStatement(const)) =>
              // generic expression not relying on field accesses to unknown values => handle as const
              localFactors += const

            case access : IR_FieldAccess =>
              if (matchUnknowns(access) < 0)
                localFactors += access
              else localUnknowns += access
            case e : IR_Multiplication   =>
              Logger.warn(s"Nested multiplication expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Addition         =>
              Logger.warn(s"Nested addition expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Subtraction      =>
              Logger.warn(s"Nested subtraction expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Expression       =>
              Logger.warn(s"Unknown, currently unsupported nested expression found: $e")
              localFactors += e
          }
        }
        if (localUnknowns.size > 1)
          Logger.warn("Non-linear equations are currently unsupported")
        if (localUnknowns.isEmpty) // no unknowns -> add to rhs
          fVals(pos).summands += (if (switchSign) mult else IR_Negative(mult))
        else // unknowns detected -> add to matrix
          AVals(pos)(matchUnknowns(localUnknowns.head)).summands += (
            if (switchSign)
              IR_Negative(IR_Multiplication(localFactors))
            else
              IR_Multiplication(localFactors))

      case _ => Logger.warn(s"Found unsupported node type ${ ex.getClass.getName }: $ex")
    }
  }

  def sortEquations() = {
    // preparation: bring all entries to left side and simplify
    var zeroEqs = equations.map(eq => Duplicate(eq.lhs - eq.rhs) : IR_Expression)
    zeroEqs = zeroEqs.map(IR_SimplifyExpression.simplifyFloatingExpr)

    // flatten computations to facilitate further processing
    // zeroEqs.foreach(eq => IR_FlattenComputation.doUntilDoneStandalone(IR_ExpressionStatement(eq)))
    // for (_ <- 0 until 10)
    // zeroEqs.foreach(eq => IR_FlattenComputation.applyStandalone(IR_ExpressionStatement(eq)))
    //IR_FlattenComputation.applyStandalone(zeroEqs)
    IR_FlattenComputation.doUntilDoneStandalone(zeroEqs)

    // process single expressions (parts of the equations) - build matrix and rhs
    for (eqNumber <- zeroEqs.indices)
      processExpression(eqNumber, zeroEqs(eqNumber), false)
  }

  def expandSpecial : Output[IR_Scope] = {
    fVals = ListBuffer.fill(unknowns.length)(IR_Addition())
    AVals = ListBuffer.fill(unknowns.length)(ListBuffer.fill(unknowns.length)(IR_Addition()))

    sortEquations()

    // choose strategy used for inverting local matrix
    if (Knowledge.experimental_applySchurCompl && IR_LocalSchurCompl.suitable(AVals))
      IR_Scope(IR_LocalSchurCompl(AVals, fVals, unknowns, relax))
    else
      IR_Scope(IR_LocalDirectInvert(AVals, fVals, unknowns, relax))
  }
}
