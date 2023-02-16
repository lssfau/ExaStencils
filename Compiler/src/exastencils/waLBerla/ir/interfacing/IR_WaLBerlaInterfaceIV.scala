package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
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
trait IR_WaLBerlaInterfaceMember extends IR_Access {
  def name : String
  def datatype : IR_Datatype

  def isPrivate : Boolean

  def resolveAccess() : IR_Access
  def resolveMemberBaseAccess() : IR_Access = IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(name), datatype)

  def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(datatype, IR_WaLBerlaUtil.getGeneratedName(name))
  def getCtor() : Option[IR_Statement] = None
  def getDtor() : Option[IR_Statement] = None

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()
}

// for interface members which can be initialized with a non-default constructor via initializer lists
trait IR_WaLBerlaInterfaceParameter extends IR_WaLBerlaInterfaceMember {
  def initializerListEntry : (IR_Access, IR_Expression) = (resolveMemberBaseAccess(), ctorParameter.access)
  def ctorParameter : IR_FunctionArgument
}
