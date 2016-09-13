package exastencils.datastructures.ir.iv

import scala.collection.mutable.HashMap

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

abstract class InternalVariable(var canBePerFragment : Boolean, var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeighbor : Boolean) extends IR_Expression {
  override def prettyprint(out : PpStream) : Unit = out << resolveName

  def usesFragmentArrays : Boolean = true
  def usesDomainArrays : Boolean = true
  def usesFieldArrays : Boolean = true
  def usesLevelArrays : Boolean = true
  def usesNeighborArrays : Boolean = true

  def resolveName : String
  def resolveDatatype : IR_Datatype
  def resolveDefValue : Option[IR_Expression] = None

  def getDeclaration() : VariableDeclarationStatement = {
    var datatype : IR_Datatype = resolveDatatype

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      datatype = IR_ArrayDatatype(datatype, Knowledge.domain_numFragmentsPerBlock)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      datatype = IR_ArrayDatatype(datatype, DomainCollection.domains.size)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      datatype = IR_ArrayDatatype(datatype, FieldCollection.fields.size)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      datatype = IR_ArrayDatatype(datatype, Knowledge.numLevels)
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      datatype = IR_ArrayDatatype(datatype, Fragment.neighbors.size)

    new VariableDeclarationStatement(datatype, resolveName)
  }

  def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = body

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      wrappedBody = new LoopOverFragments(wrappedBody)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      wrappedBody = new LoopOverDomains(wrappedBody)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      wrappedBody = new LoopOverFields(wrappedBody)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      wrappedBody = new LoopOverLevels(wrappedBody)
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      wrappedBody = new LoopOverNeighbors(wrappedBody)

    wrappedBody
  }

  def getCtor() : Option[IR_Statement] = {
    if (resolveDefValue.isDefined)
      Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt), resolveDefValue.get)))
    else
      None
  }

  def getDtor() : Option[IR_Statement] = None

  def resolvePostfix(fragment : String, domain : String, field : String, level : String, neigh : String) : String = {
    var postfix : String = ""

    if (canBePerFragment && !usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      postfix += "_" + fragment
    if (canBePerDomain && !usesDomainArrays && DomainCollection.domains.size > 1)
      postfix += "_" + domain
    if (canBePerField && !usesFieldArrays && FieldCollection.fields.size > 1)
      postfix += "_" + field
    if (canBePerLevel && !usesLevelArrays && Knowledge.numLevels > 1)
      postfix += "_" + level
    if (canBePerNeighbor && !usesNeighborArrays && Fragment.neighbors.size > 1)
      postfix += "_" + neigh

    postfix
  }

  def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = baseAccess

    // reverse compared to datatype wrapping, since we need to unwrap it "from outer to inner"
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      access = new ArrayAccess(access, neigh)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1) {
      val simplifiedLevel : IR_Expression =
        level match {
          case IR_IntegerConstant(v) => v - Knowledge.minLevel
          case _                     => level - Knowledge.minLevel
        }
      access = new ArrayAccess(access, simplifiedLevel)
    }
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      access = new ArrayAccess(access, field)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      access = new ArrayAccess(access, domain)
    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      access = new ArrayAccess(access, fragment)

    access
  }

  def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, IR_Statement], dtors : HashMap[String, IR_Statement]) = {
    declarations += (resolveName -> getDeclaration)
    for (ctor <- getCtor())
      ctors += (resolveName -> ctor)
    for (dtor <- getDtor())
      dtors += (resolveName -> dtor)
  }
}

abstract class UnduplicatedVariable extends InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}
