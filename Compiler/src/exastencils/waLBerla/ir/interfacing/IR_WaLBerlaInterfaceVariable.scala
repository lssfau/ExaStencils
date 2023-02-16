package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.config.Knowledge
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

object CollectWaLBerlaInterfaceMembers extends QuietDefaultStrategy("Collect waLBerla iface members") {
  override def applyStandalone[T](nodes : mutable.Buffer[T]) : Unit = {
    collectedMembers.clear()
    super.applyStandalone(nodes)
  }

  var collectedMembers = ListBuffer[IR_WaLBerlaInterfaceMember]()
  this += Transformation("..", {
    case iv : IR_WaLBerlaInterfaceMember =>
      collectedMembers += iv
      iv
  })
}

// implicit ordering for interface member classes
object IR_WaLBerlaInterfaceMember {
  // order by type
  val byTypeOrd : Ordering[IR_WaLBerlaInterfaceMember] = Ordering.by {
    // it is necessary to specify the initialization order in the constructor -> done here via implicit sorting
    case _ : IR_WaLBerlaBlockForest => 0
    case _ : IR_WaLBerlaBlockDataID => 1
    case _                          => 2
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

  def resolveMemberBaseAccess() : IR_Access = IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(name), datatype)

  def isPrivate : Boolean

  def numBlocks : Int = Knowledge.domain_numFragmentsPerBlock
  def numLevels : Int = Knowledge.numLevels
  def numNeighbors : Int = DefaultNeighbors.neighbors.size

  def hasMultipleBlocks : Boolean = numBlocks > 1
  def hasMultipleLevels : Boolean = numLevels > 1
  def hasMultipleNeighbors : Boolean =  DefaultNeighbors.neighbors.size > 1

  override def getDeclaration() : IR_VariableDeclaration = {
    var datatype : IR_Datatype = resolveDatatype()

    if (canBePerBlock && hasMultipleBlocks)
      datatype = IR_StdArrayDatatype(datatype, numBlocks)
    if (canBePerLevel && hasMultipleLevels)
      datatype = IR_StdArrayDatatype(datatype, numLevels)
    if (canBePerNeighbor && hasMultipleNeighbors)
      datatype = IR_StdArrayDatatype(datatype, numNeighbors)

    new IR_VariableDeclaration(datatype, resolveName())
  }

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = body

    // TODO: loops currently manually expanded
    if (canBePerBlock && hasMultipleBlocks)
      wrappedBody = IR_LoopOverFragments(wrappedBody).expandSpecial().inner
    if (canBePerLevel && hasMultipleLevels)
      wrappedBody = IR_LoopOverLevels(wrappedBody).expand().inner
    if (canBePerNeighbor && hasMultipleNeighbors)
      wrappedBody = IR_LoopOverNeighbors(wrappedBody).expand().inner

    wrappedBody
  }

  def resolveName() : String = IR_WaLBerlaUtil.getGeneratedName(name)
  def resolveDatatype() : IR_Datatype

  override def datatype = resolveDatatype()

  def resolveAccess(baseAccess : IR_Expression, block : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = baseAccess

    // reverse compared to datatype wrapping, since we need to unwrap it "from outer to inner"
    if (canBePerNeighbor && hasMultipleNeighbors)
      access = IR_ArrayAccess(access, neigh)
    if (canBePerLevel && hasMultipleLevels) {
      val simplifiedLevel : IR_Expression =
        level match {
          case IR_IntegerConstant(v) => v - Knowledge.minLevel
          case _                     => level - Knowledge.minLevel
        }
      access = IR_ArrayAccess(access, simplifiedLevel)
    }
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

  override def resolvePostfix(fragment : String, domain : String, field : String, level : String, neigh : String) : String =
    resolvePostfix(fragment, level, neigh)

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression =
    resolveAccess(baseAccess, fragment, level, neigh)

  override def prettyprint(out : PpStream) : Unit = out << resolveName
}

// for interface members which can be initialized with a non-default constructor via initializer lists
abstract class IR_WaLBerlaInterfaceParameter(
    canBePerBlock : Boolean,
    canBePerLevel : Boolean,
    canBePerNeighbor : Boolean) extends IR_WaLBerlaInterfaceMember(canBePerBlock, canBePerLevel, canBePerNeighbor) {

  def initializerListEntry : (IR_Access, IR_Expression) = (resolveMemberBaseAccess(), ctorParameter.access)
  def ctorParameter : IR_FunctionArgument
}
