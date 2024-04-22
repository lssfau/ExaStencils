package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.AbstractMap

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest._
import exastencils.waLBerla.ir.field.IR_IV_WaLBerlaGetField
import exastencils.waLBerla.ir.field.IR_IV_WaLBerlaGetFieldData
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

// implicit ordering for interface member classes
object IR_WaLBerlaInterfaceMember {
  // order by type
  val byTypeOrd : Ordering[IR_WaLBerlaInterfaceMember] = Ordering.by {
    // it is necessary to specify the initialization order in the constructor -> done here via implicit sorting
    case _ : IR_WaLBerlaBlockForest     => 0
    case _ : IR_WaLBerlaLocalBlocks     => 1
    case _ : IR_WaLBerlaBlockDataID     => 2
    case _ : IR_IV_WaLBerlaGetField     => 3
    case _ : IR_IV_WaLBerlaGetFieldData => 4
    case _                              => 5
  }
  // order by name
  val byNameOrd : Ordering[IR_WaLBerlaInterfaceMember] = Ordering.by { member : IR_WaLBerlaInterfaceMember => member.name }

  // combine both orderings
  implicit val ord = Ordering.by { member : IR_WaLBerlaInterfaceMember => (member, member) }(Ordering.Tuple2(byTypeOrd, byNameOrd))
}

// IV-like datastructure for interface members
abstract class IR_WaLBerlaInterfaceMember(
    var canBePerBlock : Boolean,
    var canBePerLevel : Boolean,
    var canBePerNeighbor : Boolean) extends IR_Access with IR_InternalVariableLike {

  def name : String
  def resolveName() : String = IR_WaLBerlaUtil.getGeneratedName(name)

  def resolveDatatype() : IR_Datatype
  override def datatype = resolveDatatype()

  def resolveMemberBaseAccess() : IR_Access = IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(name), getWrappedDatatype())

  def isPrivate : Boolean

  private def numFragments : Int = Knowledge.domain_numFragmentsPerBlock
  def minLevel : Int = Knowledge.minLevel
  def maxLevel : Int = Knowledge.maxLevel
  def numLevels : Int = maxLevel - minLevel + 1
  def numNeighbors : Int = DefaultNeighbors.neighbors.size

  def usesStdVectorForBlocks = canBePerBlock && hasMultipleBlocks && !numBlocksKnown
  def numBlocksKnown : Boolean = Knowledge.domain_isPartitioningKnown
  def hasMultipleBlocks : Boolean = if (numBlocksKnown) numFragments > 1 else true
  def hasMultipleLevels : Boolean = numLevels > 1
  def hasMultipleNeighbors : Boolean =  DefaultNeighbors.neighbors.size > 1

  def getWrappedDatatype() : IR_Datatype = {
    var datatype : IR_Datatype = resolveDatatype()

    if (canBePerBlock && hasMultipleBlocks)
      datatype = if (usesStdVectorForBlocks) IR_StdVectorDatatype(datatype) else IR_StdArrayDatatype(datatype, numFragments)

    getWrappedDatatypePerBlock(datatype)
  }

  def getWrappedDatatypePerBlock(datatype: IR_Datatype) : IR_Datatype = {
    var dt = datatype

    if (canBePerLevel && hasMultipleLevels)
      dt = IR_StdArrayDatatype(dt, numLevels)
    if (canBePerNeighbor && hasMultipleNeighbors)
      dt = IR_StdArrayDatatype(dt, numNeighbors)

    dt
  }

  override def getDeclaration() : IR_VariableDeclaration =
    new IR_VariableDeclaration(getWrappedDatatype(), resolveName())

  // TODO: interface instantiated after last expand pass -> manually expanded loops
  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = body

    if (canBePerBlock && hasMultipleBlocks)
      wrappedBody = IR_WaLBerlaLoopOverLocalBlockArray(wrappedBody).expandSpecial().inner

    wrapInLoopsPerBlock(wrappedBody)
  }

  def wrapInLoopsPerBlock(body : IR_Statement) = {
    var wrappedBody = body

    if (canBePerLevel && hasMultipleLevels)
      wrappedBody = IR_LoopOverLevels(wrappedBody).expand().inner
    if (canBePerNeighbor && hasMultipleNeighbors)
      wrappedBody = IR_LoopOverNeighbors(wrappedBody).expand().inner

    wrappedBody
  }

  override def registerIV(declarations : AbstractMap[String, IR_VariableDeclaration], ctors : AbstractMap[String, IR_Statement], dtors : AbstractMap[String, IR_Statement]) : Unit = {
    super.registerIV(declarations, ctors, dtors)

    // extend ctor: reserve memory for std::vector
    val k = resolveName()
    if (usesStdVectorForBlocks) {
      val acc = resolveAccessPerBlock(resolveMemberBaseAccess(), IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
      val reserveCall = wrapInLoopsPerBlock(IR_MemberFunctionCall(acc, "reserve", IR_WaLBerlaLocalBlocks().size()))

      // adapt ctor
      if (getCtor().isDefined && ctors.contains(k))
        ctors(k) = IR_Scope(reserveCall, ctors(k))
      else
        ctors(k) = reserveCall
    }
  }

  def resolveAccessPerBlock(baseAccess : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = baseAccess

    // reverse compared to datatype wrapping, since we need to unwrap it "from outer to inner"
    if (canBePerNeighbor && hasMultipleNeighbors)
      access = IR_ArrayAccess(access, neigh)
    if (canBePerLevel && hasMultipleLevels) {
      val simplifiedLevel : IR_Expression =
        level match {
          case IR_IntegerConstant(v) => v - minLevel
          case _                     => level - minLevel
        }
      access = IR_ArrayAccess(access, simplifiedLevel)
    }

    access
  }

  def resolveAccess(baseAccess : IR_Expression, block : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = resolveAccessPerBlock(baseAccess, level, neigh)

    if (canBePerBlock && hasMultipleBlocks)
      access = IR_ArrayAccess(access, block)

    access
  }

  def resolvePostfix(block : String, level : String, neigh : String) : String = {
    var postfix : String = ""

    if (canBePerBlock && hasMultipleBlocks)
      postfix += "_" + block
    if (canBePerLevel && hasMultipleLevels)
      postfix += "_" + level
    if (canBePerNeighbor && hasMultipleNeighbors)
      postfix += "_" + neigh

    postfix
  }

  override def resolvePostfix(block : String, domain : String, field : String, level : String, neigh : String) : String =
    resolvePostfix(block, level, neigh)

  override def resolveAccess(baseAccess : IR_Expression, block : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression =
    resolveAccess(baseAccess, block, level, neigh)

  override def prettyprint(out : PpStream) : Unit = out << resolveName
}

// for interface members which can be initialized with a non-default constructor via initializer lists
abstract class IR_WaLBerlaInterfaceParameter(
    canBePerBlock : Boolean,
    canBePerLevel : Boolean,
    canBePerNeighbor : Boolean) extends IR_WaLBerlaInterfaceMember(canBePerBlock, canBePerLevel, canBePerNeighbor) {

  def initializerListEntry : (IR_Access, IR_Expression) = (resolveMemberBaseAccess(), ctorParameter.access)
  def ctorParameter : IR_FunctionArgument = IR_FunctionArgument(name, getWrappedDatatype())
}
