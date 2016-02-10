package exastencils.datastructures.ir.iv

import scala.collection.mutable.HashMap

import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._

abstract class InternalVariable(var canBePerFragment : Boolean, var canBePerDomain : Boolean, var canBePerField : Boolean, var canBePerLevel : Boolean, var canBePerNeighbor : Boolean) extends Expression {
  override def prettyprint(out : PpStream) : Unit = out << resolveName

  def usesFragmentArrays : Boolean = true
  def usesDomainArrays : Boolean = true
  def usesFieldArrays : Boolean = true
  def usesLevelArrays : Boolean = true
  def usesNeighborArrays : Boolean = true

  def resolveName : String
  def resolveDataType : Datatype
  def resolveDefValue : Option[Expression] = None

  def getDeclaration() : VariableDeclarationStatement = {
    var dt : Datatype = resolveDataType

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      dt = ArrayDatatype(dt, Knowledge.domain_numFragmentsPerBlock)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      dt = ArrayDatatype(dt, DomainCollection.domains.size)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      dt = ArrayDatatype(dt, FieldCollection.fields.size)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      dt = ArrayDatatype(dt, Knowledge.numLevels)
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      dt = ArrayDatatype(dt, Fragment.neighbors.size)

    new VariableDeclarationStatement(dt, resolveName)
  }

  def wrapInLoops(body : Statement) : Statement = {
    var wrappedBody = body

    // NOTE: reverse order due to wrapping
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      wrappedBody = new LoopOverNeighbors(wrappedBody)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1)
      wrappedBody = new LoopOverLevels(wrappedBody)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      wrappedBody = new LoopOverFields(wrappedBody)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      wrappedBody = new LoopOverDomains(wrappedBody)
    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      wrappedBody = new LoopOverFragments(wrappedBody)

    wrappedBody
  }

  def getCtor() : Option[Statement] = {
    if (resolveDefValue.isDefined)
      Some(wrapInLoops(AssignmentStatement(resolveAccess(resolveName, LoopOverFragments.defIt, LoopOverDomains.defIt, LoopOverFields.defIt, LoopOverLevels.defIt, LoopOverNeighbors.defIt), resolveDefValue.get)))
    else
      None
  }

  def getDtor() : Option[Statement] = None

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

  def resolveAccess(baseAccess : Expression, fragment : Expression, domain : Expression, field : Expression, level : Expression, neigh : Expression) : Expression = {
    var access = baseAccess

    if (canBePerFragment && usesFragmentArrays && Knowledge.domain_numFragmentsPerBlock > 1)
      access = new ArrayAccess(access, fragment)
    if (canBePerDomain && usesDomainArrays && DomainCollection.domains.size > 1)
      access = new ArrayAccess(access, domain)
    if (canBePerField && usesFieldArrays && FieldCollection.fields.size > 1)
      access = new ArrayAccess(access, field)
    if (canBePerLevel && usesLevelArrays && Knowledge.numLevels > 1) {
      val simplifiedLevel : Expression =
        if (level.isInstanceOf[IntegerConstant])
          level.asInstanceOf[IntegerConstant].value - Knowledge.minLevel
        else
          level - Knowledge.minLevel
      access = new ArrayAccess(access, simplifiedLevel)
    }
    if (canBePerNeighbor && usesNeighborArrays && Fragment.neighbors.size > 1)
      access = new ArrayAccess(access, neigh)

    access
  }

  def registerIV(declarations : HashMap[String, VariableDeclarationStatement], ctors : HashMap[String, Statement], dtors : HashMap[String, Statement]) = {
    declarations += (resolveName -> getDeclaration)
    if (getCtor().isDefined)
      ctors += (resolveName -> getCtor().get)
    if (getDtor().isDefined)
      dtors += (resolveName -> getDtor().get)
  }
}

abstract class UnduplicatedVariable extends InternalVariable(false, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveName
}

