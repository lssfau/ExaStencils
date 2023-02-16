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

package exastencils.baseExt.ir

import scala.collection.mutable.HashMap

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config._
import exastencils.domain.ir.IR_DomainCollection
import exastencils.field.ir.IR_FieldCollection
import exastencils.fieldlike.ir.IR_FieldLikeCollections
import exastencils.prettyprinting._

/// IR_InternalVariable

abstract class IR_InternalVariable(
    var canBePerFragment : Boolean,
    var canBePerDomain : Boolean,
    var canBePerField : Boolean,
    var canBePerLevel : Boolean,
    var canBePerNeighbor : Boolean) extends IR_Expression {

  override def datatype = resolveDatatype()
  override def prettyprint(out : PpStream) : Unit = out << resolveName

  def usesFragmentArrays : Boolean = true
  def usesDomainArrays : Boolean = true
  def usesFieldArrays : Boolean = true
  def usesLevelArrays : Boolean = true
  def usesNeighborArrays : Boolean = true

  def resolveName() : String
  def resolveDatatype() : IR_Datatype
  def resolveDefValue() : Option[IR_Expression] = None

  def getDeclaration() : IR_VariableDeclaration = {
    var datatype : IR_Datatype = resolveDatatype()

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      datatype = IR_ArrayDatatype(datatype, Knowledge.domain_numFragmentsPerBlock)
    if (canBePerDomain && usesDomainArrays && IR_DomainCollection.objects.size > 1)
      datatype = IR_ArrayDatatype(datatype, IR_DomainCollection.objects.size)
    if (canBePerField && usesFieldArrays && IR_FieldCollection.objects.size > 1) // TODO: fieldlike
      datatype = IR_ArrayDatatype(datatype, IR_FieldCollection.objects.size)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      datatype = IR_ArrayDatatype(datatype, Knowledge.numLevels)
    if (canBePerNeighbor && usesNeighborArrays && DefaultNeighbors.neighbors.size > 1)
      datatype = IR_ArrayDatatype(datatype, DefaultNeighbors.neighbors.size)

    new IR_VariableDeclaration(datatype, resolveName())
  }

  def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = body

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      wrappedBody = IR_LoopOverFragments(wrappedBody)
    if (canBePerDomain && usesDomainArrays && IR_DomainCollection.objects.size > 1)
      wrappedBody = IR_LoopOverDomains(wrappedBody)
    if (canBePerField && usesFieldArrays && IR_FieldCollection.objects.size > 1) // TODO: fieldlike
      wrappedBody = IR_LoopOverFields(wrappedBody)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      wrappedBody = IR_LoopOverLevels(wrappedBody)
    if (canBePerNeighbor && usesNeighborArrays && DefaultNeighbors.neighbors.size > 1)
      wrappedBody = IR_LoopOverNeighbors(wrappedBody)

    wrappedBody
  }

  def getCtor() : Option[IR_Statement] = {
    if (resolveDefValue().isDefined)
      Some(wrapInLoops(IR_Assignment(resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt), resolveDefValue().get)))
    else
      None
  }

  def getDtor() : Option[IR_Statement] = None

  def resolvePostfix(fragment : String, domain : String, field : String, level : String, neigh : String) : String = {
    var postfix : String = ""

    if (canBePerFragment && !usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      postfix += "_" + fragment
    if (canBePerDomain && !usesDomainArrays && IR_DomainCollection.objects.size > 1)
      postfix += "_" + domain
    if (canBePerField && !usesFieldArrays && IR_FieldLikeCollections.collections.map(_.objects.size).sum > 1)
      postfix += "_" + field
    if (canBePerLevel && !usesLevelArrays && Knowledge.numLevels > 1)
      postfix += "_" + level
    if (canBePerNeighbor && !usesNeighborArrays && DefaultNeighbors.neighbors.size > 1)
      postfix += "_" + neigh

    postfix
  }

  def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = baseAccess

    // reverse compared to datatype wrapping, since we need to unwrap it "from outer to inner"
    if (canBePerNeighbor && usesNeighborArrays && DefaultNeighbors.neighbors.size > 1)
      access = IR_ArrayAccess(access, neigh)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1) {
      val simplifiedLevel : IR_Expression =
        level match {
          case IR_IntegerConstant(v) => v - Knowledge.minLevel
          case _                     => level - Knowledge.minLevel
        }
      access = IR_ArrayAccess(access, simplifiedLevel)
    }
    if (canBePerField && usesFieldArrays && IR_FieldCollection.objects.size > 1) // TODO: fieldlike
      access = IR_ArrayAccess(access, field)
    if (canBePerDomain && usesDomainArrays && IR_DomainCollection.objects.size > 1)
      access = IR_ArrayAccess(access, domain)
    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      access = IR_ArrayAccess(access, fragment)

    access
  }

  def registerIV(declarations : HashMap[String, IR_VariableDeclaration], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    declarations += (resolveName -> getDeclaration)
    for (ctor <- getCtor())
      ctors += (resolveName -> ctor)
    for (dtor <- getDtor())
      dtors += (resolveName -> dtor)
  }
}

/// IR_UnduplicatedVariable

// can be used as base class for variables that don't need to be duplicated per fragment/domain/field/level/neighbor
abstract class IR_UnduplicatedVariable extends IR_InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}
