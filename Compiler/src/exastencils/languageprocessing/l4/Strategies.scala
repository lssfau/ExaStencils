package exastencils.languageprocessing.l4

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core.collectors.L4CommCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge

object CollectCommInformation extends DefaultStrategy("Collecting information relevant for adding communication statements") {
  var commCollector : L4CommCollector = new L4CommCollector(HashMap())

  override def apply(node : Option[Node] = None) = {
    commCollector.reset()
    this.register(commCollector)
    super.apply(node)
    this.unregister(commCollector)
  }

  override def applyStandalone(node : Node) = {
    commCollector.reset()
    this.register(commCollector)
    super.applyStandalone(node)
    this.unregister(commCollector)
  }

  this += new Transformation("Collect", { // FIXME: add visitor strategy defining dummy trafo?
    case n : Node => n
  })
}

object ResolveL4 extends DefaultStrategy("Resolving L4 specifics") {
  this += new Transformation("geometricCoordinate", {
    case FunctionCallExpression(LeveledAccess("geometricCoordinate_x", level), List()) => BasicAccess("xPos")
    case FunctionCallExpression(LeveledAccess("geometricCoordinate_y", level), List()) => BasicAccess("yPos")
    case FunctionCallExpression(LeveledAccess("geometricCoordinate_z", level), List()) => BasicAccess("zPos")
    case FunctionCallExpression(BasicAccess("geometricCoordinate_x"), List()) => BasicAccess("xPos")
    case FunctionCallExpression(BasicAccess("geometricCoordinate_y"), List()) => BasicAccess("yPos")
    case FunctionCallExpression(BasicAccess("geometricCoordinate_z"), List()) => BasicAccess("zPos")
    case FunctionCallExpression(LeveledAccess("geometricCoordinate", level), List(IntegerConstant(0))) => BasicAccess("xPos")
    case FunctionCallExpression(LeveledAccess("geometricCoordinate", level), List(IntegerConstant(1))) => BasicAccess("yPos")
    case FunctionCallExpression(LeveledAccess("geometricCoordinate", level), List(IntegerConstant(2))) => BasicAccess("zPos")
  })

  this += new Transformation("Constants", {
    case BasicAccess("PI") | BasicAccess("M_PI") | BasicAccess("Pi") => FloatConstant(math.Pi)
  })

  this += new Transformation("gridWidth", {
    case FunctionCallExpression(LeveledAccess("gridWidth_x", SingleLevelSpecification(level)), List()) => FloatConstant(knowledge.Knowledge.discr_hx(level - knowledge.Knowledge.minLevel))
    case FunctionCallExpression(LeveledAccess("gridWidth_y", SingleLevelSpecification(level)), List()) => FloatConstant(knowledge.Knowledge.discr_hy(level - knowledge.Knowledge.minLevel))
    case FunctionCallExpression(LeveledAccess("gridWidth_z", SingleLevelSpecification(level)), List()) => FloatConstant(knowledge.Knowledge.discr_hz(level - knowledge.Knowledge.minLevel))
    case FunctionCallExpression(LeveledAccess("gridWidth", SingleLevelSpecification(level)), List(IntegerConstant(0))) => FloatConstant(knowledge.Knowledge.discr_hx(level - knowledge.Knowledge.minLevel))
    case FunctionCallExpression(LeveledAccess("gridWidth", SingleLevelSpecification(level)), List(IntegerConstant(1))) => FloatConstant(knowledge.Knowledge.discr_hy(level - knowledge.Knowledge.minLevel))
    case FunctionCallExpression(LeveledAccess("gridWidth", SingleLevelSpecification(level)), List(IntegerConstant(2))) => FloatConstant(knowledge.Knowledge.discr_hz(level - knowledge.Knowledge.minLevel))
  })
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ AssignmentStatement(lhs : FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      var statements : ListBuffer[Statement] =
        CollectCommInformation.commCollector.communicates.map(comm =>
          CommunicateStatement(comm._1, "both", List( /* FIXME: add radius */ )) : Statement).to[ListBuffer]

      statements += LoopOverFragmentsStatement(List(
        LoopOverPointsStatement(lhs, false, None, None, None, None, List(assignment), None)),
        None)

      statements
    }

    // FIXME: handle reductions
    // FIXME: handle stencil fields
  }, false /* recursion must be switched of due to wrapping mechanism */ )
}
