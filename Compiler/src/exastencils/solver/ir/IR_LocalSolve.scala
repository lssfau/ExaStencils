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
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.field.ir.IR_SlotAccess
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.logger.Logger
import exastencils.optimization.ir._

/// IR_LocalSolve

case class IR_LocalSolve(
    var unknowns : ListBuffer[IR_FieldLikeAccess],
    var equations : ListBuffer[IR_Equation],
    var jacobiType : Boolean,
    var relax : Option[IR_Expression]) extends IR_Statement with IR_SpecialExpandable {
  var omitConditions = Knowledge.experimental_forceOmitCondInLocalSolve

  var fVals = ListBuffer[IR_Addition]()
  var AVals = ListBuffer[ListBuffer[IR_Addition]]()

  var AInv : IR_Expression = _

  def matchUnknowns(other : IR_FieldLikeAccess) : Int = {
    if (other.frozen)
      return -1 // skip frozen fields

    for (i <- unknowns.indices) {
      val sameSlot = if (other.field.numSlots == 1 && unknowns(i).field.numSlots == 1) {
        true
      } else {
        (other.slot, unknowns(i).slot) match {
          case (IR_SlotAccess(IR_IV_ActiveSlot(_, fragIdx0), o0), IR_SlotAccess(IR_IV_ActiveSlot(_, fragIdx1), o1)) => fragIdx0 == fragIdx1 && o0 == o1
          case (IR_IntegerConstant(s0), IR_IntegerConstant(s1))                                                       => s0 == s1
          case _                                                                                                      => false
        }
      }

      if (other.field.codeName == unknowns(i).field.codeName && other.index == unknowns(i).index && sameSlot)
        return i // match
    }

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
      case access : IR_FieldLikeAccess if matchUnknowns(access) >= 0 =>
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

      case access : IR_FieldLikeAccess =>
        val uPos = matchUnknowns(access)
        if (uPos < 0)
          fVals(pos).summands += (if (switchSign) access else IR_Negative(access)) // no match -> rhs
        else
          AVals(pos)(uPos).summands += IR_RealConstant(if (switchSign) -1 else 1) // match -> matrix

      case mult : IR_Multiplication =>
        // split into known and unknown
        var localFactors = ListBuffer[IR_Expression]()
        var localUnknowns = ListBuffer[IR_FieldLikeAccess]()
        var localSwitchSign = switchSign

        def handleFactor(ex : IR_Expression) {
          ex match {
            case const : IR_Expression if !IR_ContainsUnknownAccesses.hasSome(IR_ExpressionStatement(const)) =>
              // generic expression not relying on field accesses to unknown values => handle as const
              localFactors += const

            case access : IR_FieldLikeAccess =>
              if (matchUnknowns(access) < 0)
                localFactors += access
              else
                localUnknowns += access

            case neg : IR_Negative =>
              localSwitchSign = !localSwitchSign
              handleFactor(neg.left)

            case e @ IR_Division(access : IR_FieldLikeAccess, right) =>
              if (!IR_ContainsUnknownAccesses.hasSome(IR_ExpressionStatement(right))) {
                localUnknowns += access
                localFactors += IR_Division(1, right)
              } else {
                Logger.error(s"Nested division expressions are currently unsupported: $e")
              }

            case e : IR_Multiplication =>
              for (ex <- mult.factors)
                handleFactor(ex)

            case e : IR_Addition    => Logger.error(s"Nested addition expressions are currently unsupported: $e")
            case e : IR_Subtraction => Logger.error(s"Nested subtraction expressions are currently unsupported: $e")
            case e : IR_Expression  => Logger.error(s"Unknown, currently unsupported nested expression found: $e")
          }
        }

        for (ex <- mult.factors)
          handleFactor(ex)

        if (localUnknowns.size > 1)
          Logger.error("Non-linear equations are currently unsupported")
        if (localUnknowns.isEmpty) // no unknowns -> add to rhs
          fVals(pos).summands += (if (localSwitchSign) mult else IR_Negative(mult))
        else // unknowns detected -> add to matrix
          AVals(pos)(matchUnknowns(localUnknowns.head)).summands += (
            if (localSwitchSign)
              IR_Negative(IR_Multiplication(localFactors))
            else
              IR_Multiplication(localFactors))

      case div : IR_Division =>
        if (IR_ContainsUnknownAccesses.hasSome(IR_ExpressionStatement(div.right)))
          Logger.error("Divisions with unknowns in the divisor are not supported")

        processExpression(pos, IR_Multiplication(div.left, IR_Division(1, div.right)), switchSign)

      case _ => Logger.error(s"Found unsupported node type ${ ex.getClass.getName }: $ex")
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

  def isSolvableWithoutInverse(msi : IR_MatShape) : Boolean = {
    msi.shape match {
      case "schur" => true
      //case "Diagonal" =>
      case _ => false
    }
  }

  def findMatShape(faccs : ListBuffer[IR_FieldLikeAccess]) : Option[IR_MatShape] = {
    //TODO shapes can differ for different fields
    // just use first shape found for now
    for (f <- faccs) {
      if (f.field.matShape.isDefined)
        return Some(f.field.matShape.get)
    }
    None
  }

  def expandSpecial : Output[IR_Scope] = {
    fVals = ListBuffer.fill(unknowns.length)(IR_Addition())
    AVals = ListBuffer.fill(unknowns.length)(ListBuffer.fill(unknowns.length)(IR_Addition()))

    sortEquations()

    val fExp = fVals.map(mapToExp)
    val AExp = AVals.map(_.map(mapToExp))

    //TODO sizecheck? go rt if mat too large
    val shapeFromField = findMatShape(unknowns)

    val msi : IR_MatShape =
      if (shapeFromField.isDefined) {
        // structure given in field declaration
        shapeFromField.get
      } else if (Knowledge.experimental_classifyLocMat || Knowledge.experimental_applySchurCompl) {
        // structure to specify (blocksize to specify for apply schur compl, for backwards compatibility)
        // TODO: if all local matrices have the same structure: classify only once
        val shape = IR_ClassifyMatShape(AVals)
        if (Knowledge.experimental_matrixDebugConfig)
          Logger.warn(shape.toStringList())
        shape
      } else if (Knowledge.experimental_locMatShape != "filled") {
        // structure for all local matrices given in knowledge
        IR_MatShape(Knowledge.experimental_locMatShape)
          .addInfo("block", Knowledge.experimental_locMatBlocksize)
          .addInfo("A", Knowledge.experimental_locMatShapeA)
          .addInfo("Ablock", Knowledge.experimental_locMatBlocksizeA)
      } else IR_MatShape("filled")

    if (Knowledge.experimental_matrixDebugConfig)
      Logger.warn(s"Local matrix is of shape ${ msi.toStringList() }")

    // choose strategy used for inverting local matrix
    // inverse precalculated
    if (AInv != null) {
      AInv match {
        case va : IR_VariableAccess   => IR_Scope(IR_LocalPreCompInvert(va, fExp, unknowns, jacobiType, relax))
        case me : IR_MatrixExpression => IR_Scope(IR_LocalConstInvert(me, fExp, unknowns, jacobiType, relax))
        case _                        => Logger.error(s"Unsupported AInv: $AInv")
      }
    }
    // if matrix has schur structure and blocksize of D block is 1 -> solvable without inverse
    //else if (isSolvableWithoutInverse(msi) && AVals.length - msi.size("block") == 1)
    //   IR_Scope(IR_LocalSchurComplGeneralized(AExp, fExp, unknowns, jacobiType, relax, omitConditions, msi))
    else {
      // invert matrix with given structure information
      IR_Scope(IR_LocalDirectInvert(AExp, fExp, unknowns, jacobiType, relax, omitConditions, msi))
    }
  }
}

