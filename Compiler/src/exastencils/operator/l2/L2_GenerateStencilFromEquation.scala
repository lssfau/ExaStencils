package exastencils.operator.l2

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.logger.Logger
import exastencils.optimization.l2._

/// L2_GenerateStencilFromEquation

object L2_GenerateStencilFromEquation {
  // input
  var targetFields = ListBuffer[L2_FieldAccess]()

  // output
  var stencils = HashMap[L2_FieldAccess, ListBuffer[L2_StencilOffsetEntry]]()
  var rhs : L2_Expression = 0

  def matchUnknowns(target : L2_FieldAccess) : Option[L2_StencilOffsetEntry] = {
    // skip frozen field accesses
    if (target.frozen) return None

    val findStencil = stencils.find(_._1.name == target.name)

    // check if a stencil is defined for the target field
    if (findStencil.isEmpty) return None

    // check if the offset is already registered
    val stencil = findStencil.get._2
    val findOffset = stencil.find(_.offset == target.getOffset)

    if (findOffset.isDefined) return findOffset

    // target field with new offset -> generate new entry
    stencil += L2_StencilOffsetEntry(Duplicate(target.getOffset), 0.0)
    stencil.find(_.offset == target.getOffset)
  }

  object L2_ContainsUnknownAccesses extends QuietDefaultStrategy("Check for (field) accesses to unknowns") {
    var found : Boolean = false

    def hasSome(node : Node) : Boolean = {
      found = false
      applyStandalone(node)
      found
    }

    this += new Transformation("Match field accesses", {
      case access : L2_FieldAccess if matchUnknowns(access).isDefined =>
        found = true
        access
    })
  }

  def processExpression(pos : Int, ex : L2_Expression, switchSign : Boolean) : Unit = {
    ex match {
      case const : L2_Number =>
        rhs += (if (switchSign) const else L2_Negative(const))

      case const : L2_Expression if !L2_ContainsUnknownAccesses.hasSome(L2_ExpressionStatement(const)) =>
        // generic expression not relying on field accesses to unknown values => handle as const
        rhs += (if (switchSign) const else L2_Negative(const))

      case L2_Negative(exp) =>
        processExpression(pos, exp, !switchSign)

      case add : L2_Addition =>
        add.summands.foreach(ex => processExpression(pos, ex, switchSign))

      case sub : L2_Subtraction =>
        processExpression(pos, sub.left, switchSign)
        processExpression(pos, sub.right, !switchSign)

      case access : L2_FieldAccess =>
        val stencilEntry = matchUnknowns(access)
        if (stencilEntry.isEmpty)
          rhs += (if (switchSign) access else L2_Negative(access)) // no match -> rhs
        else
          stencilEntry.get.coefficient += L2_RealConstant(if (switchSign) -1 else 1) // match -> matrix

      case mult : L2_Multiplication =>
        // split into known and unknown
        var localFactors = ListBuffer[L2_Expression]()
        var localUnknowns = ListBuffer[L2_FieldAccess]()
        for (ex <- mult.factors) {
          ex match {
            case const : L2_Expression if !L2_ContainsUnknownAccesses.hasSome(L2_ExpressionStatement(const)) =>
              // generic expression not relying on field accesses to unknown values => handle as const
              localFactors += const

            case access : L2_FieldAccess                         =>
              if (matchUnknowns(access).isEmpty)
                localFactors += access
              else localUnknowns += access
            case e @ L2_Division(access : L2_FieldAccess, right) =>
              if (!L2_ContainsUnknownAccesses.hasSome(L2_ExpressionStatement(right))) {
                localUnknowns += access
                localFactors += L2_Division(1, right)
              } else {
                Logger.warn(s"Nested division expressions are currently unsupported: $e")
                localFactors += e
              }
            case e : L2_Multiplication                           =>
              Logger.warn(s"Nested multiplication expressions are currently unsupported: $e")
              localFactors += e
            case e : L2_Addition                                 =>
              Logger.warn(s"Nested addition expressions are currently unsupported: $e")
              localFactors += e
            case e : L2_Subtraction                              =>
              Logger.warn(s"Nested subtraction expressions are currently unsupported: $e")
              localFactors += e
            case e : L2_Expression                               =>
              Logger.warn(s"Unknown, currently unsupported nested expression found: $e")
              localFactors += e
          }
        }
        if (localUnknowns.size > 1)
          Logger.warn("Non-linear equations are currently unsupported")
        if (localUnknowns.isEmpty) // no unknowns -> add to rhs
          rhs += (if (switchSign) mult else L2_Negative(mult))
        else // unknowns detected -> add to matrix
          matchUnknowns(localUnknowns.head).get.coefficient += (
            if (switchSign)
              L2_Negative(L2_Multiplication(localFactors))
            else
              L2_Multiplication(localFactors))

      case _ => Logger.warn(s"Found unsupported node type ${ ex.getClass.getName }: $ex")
    }
  }

  def sortEquation(equation : L2_Equation) = {
    // preparation: bring all entries to left side and simplify
    val zeroEq = equation.asZeroEquation()

    // flatten computations to facilitate further processing - also flatten unknowns for comparability
    L2_FlattenComputation.doUntilDoneStandalone(zeroEq)
    L2_FlattenComputation.doUntilDoneStandalone(targetFields)

    // process single expressions (parts of the equations) - build matrix and rhs
    processExpression(0, zeroEq, false)
  }

  def process(equation : L2_Equation) : Unit = {
    // init stencils and rhs
    stencils = HashMap()
    targetFields.foreach(field => stencils += (field -> ListBuffer[L2_StencilOffsetEntry]()))
    rhs = L2_Addition()

    sortEquation(equation)

    rhs = L2_GeneralSimplifyWrapper.process(rhs)
    stencils.foreach(_._2.transform(L2_GeneralSimplifyWrapper.process))
  }
}
