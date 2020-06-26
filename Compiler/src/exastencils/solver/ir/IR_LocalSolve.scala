//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.solver.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.logger.Logger
import exastencils.optimization.ir._

/// IR_LocalSolve

case class IR_LocalSolve(
    var unknowns : ListBuffer[IR_FieldAccess],
    var equations : ListBuffer[IR_Equation],
    var jacobiType : Boolean,
    var relax : Option[IR_Expression]) extends IR_Statement with IR_SpecialExpandable {
  var omitConditions = false

  var fVals = ListBuffer[IR_Addition]()
  var AVals = ListBuffer[ListBuffer[IR_Addition]]()

  var AInv : IR_Expression = _

  def matchUnknowns(other : IR_FieldAccess) : Int = {
    if (other.frozen)
      return -1 // skip frozen fields

    for (i <- unknowns.indices)
      if (other.field.codeName == unknowns(i).field.codeName && other.index == unknowns(i).index)
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

            case access : IR_FieldAccess                         =>
              if (matchUnknowns(access) < 0)
                localFactors += access
              else localUnknowns += access
            case e @ IR_Division(access : IR_FieldAccess, right) =>
              if (!IR_ContainsUnknownAccesses.hasSome(IR_ExpressionStatement(right))) {
                localUnknowns += access
                localFactors += IR_Division(1, right)
              } else {
                Logger.warn(s"Nested division expressions are currently unsupported: $e")
                localFactors += e
              }
            case e : IR_Multiplication                           =>
              Logger.warn(s"Nested multiplication expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Addition                                 =>
              Logger.warn(s"Nested addition expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Subtraction                              =>
              Logger.warn(s"Nested subtraction expressions are currently unsupported: $e")
              localFactors += e
            case e : IR_Expression                               =>
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
    val zeroEqs = equations.map(_.asZeroEquation())

    // flatten computations to facilitate further processing - also flatten unknowns for comparability
    IR_FlattenComputation.doUntilDoneStandalone(zeroEqs)
    IR_FlattenComputation.doUntilDoneStandalone(unknowns)

    // process single expressions (parts of the equations) - build matrix and rhs
    for (eqNumber <- zeroEqs.indices)
      processExpression(eqNumber, zeroEqs(eqNumber), false)
  }

  def mapToExp(add : IR_Addition) : IR_Expression = {
    add match {
      case IR_Addition(ListBuffer()) => IR_RealConstant(0) // empty entries correspond to zero
      case ex : IR_Expression        => IR_GeneralSimplifyWrapper.process(ex)
    }
  }

  def matrixIsConst = {
    fVals = ListBuffer.fill(unknowns.length)(IR_Addition())
    AVals = ListBuffer.fill(unknowns.length)(ListBuffer.fill(unknowns.length)(IR_Addition()))

    sortEquations()

    val AExp = AVals.map(_.map(mapToExp))

    AExp.flatMap(_.map(_.isInstanceOf[IR_Number])).reduce(_ && _)
  }

  def matrixIsPosIndependent = {
    // TODO: check if the matrix can be constructed independently of the grid position
    false
  }

  def getMatrix = {
    // assumes previous call to matrixIsConst or expandSpecial
    IR_MatrixExpression(Some(IR_MatrixDatatype(IR_RealDatatype, AVals.length, AVals.length)),
      Duplicate(AVals.map(_.map(mapToExp))))
  }

  def isSolvableWithoutInverse(structure : String): Boolean = {
      structure match {
        case "Schur" => true
        //case "Diagonal" =>
        case _ => false
      }
  }

  def expandSpecial : Output[IR_Scope] = {
    fVals = ListBuffer.fill(unknowns.length)(IR_Addition())
    AVals = ListBuffer.fill(unknowns.length)(ListBuffer.fill(unknowns.length)(IR_Addition()))

    sortEquations()

    val fExp = fVals.map(mapToExp)
    val AExp = AVals.map(_.map(mapToExp))

    //TODO sizecheck? go rt if mat too large
    //TODO check field for matrix structure
    val matStructure : IR_MatStructure =
    if(unknowns(0).field.matStructure.isDefined) {
      unknowns(0).field.matStructure.get
    } else if(Knowledge.experimental_classifyLES) {
      IR_DetermineMatrixStructure(AVals)
    } else IR_MatStructure("Filled")


      // choose strategy used for inverting local matrix
    if (AInv != null) {
      AInv match {
        case va : IR_VariableAccess   => IR_Scope(IR_LocalPreCompInvert(va, fExp, unknowns, jacobiType, relax))
        case me : IR_MatrixExpression => IR_Scope(IR_LocalConstInvert(me, fExp, unknowns, jacobiType, relax))
        case _                        => Logger.error(s"Unsupported AInv: $AInv")
      }
    }
    else if (isSolvableWithoutInverse(matStructure.structure))
        IR_Scope(IR_LocalSchurComplGeneralized(AExp, fExp, unknowns, jacobiType, relax, omitConditions, matStructure))
    else
      IR_Scope(IR_LocalDirectInvert(AExp, fExp, unknowns, jacobiType, relax, omitConditions, matStructure))
  }
}

