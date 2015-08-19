package exastencils.languageprocessing.l4

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.core.collectors.L4CommCollector
import exastencils.core.collectors.L4ValueCollector
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.l4._
import exastencils.knowledge
import exastencils.logger._

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
  val specialFields : ListBuffer[String] = ListBuffer(
    "get_node_pos_x", "get_node_pos_y", "get_node_pos_z",
    "get_stag_cv_width_x", "get_stag_cv_width_y", "get_stag_cv_width_z",
    "get_cell_width_x", "get_cell_width_y", "get_cell_width_z",
    "get_cell_center_to_face_x", "get_cell_center_to_face_y", "get_cell_center_to_face_z")

  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()
    var valueCollector = new L4ValueCollector

    // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
    this.register(valueCollector)
    //    this.execute(new Transformation("Put Values into collector", {
    //      case x : ValueDeclarationStatement => x
    //    }))

    this.execute(new Transformation("ResolveValuesInExpressions", {
      case x : UnresolvedAccess if (x.level == None && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${x.name} as no matching Val was found"""); x }
          case _    => value.get
        }
      }
      case x : UnresolvedAccess if (x.level.isDefined && x.level.get.isInstanceOf[SingleLevelSpecification] && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name + "_" + x.level.get.asInstanceOf[SingleLevelSpecification].level)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${x.name} as no matching Val was found"""); x }
          case _    => value.get
        }
      }
    }))
    this.unregister(valueCollector)

    // resolve accesses
    this.execute(new Transformation("ResolveAccessSpecifications", {
      case access : UnresolvedAccess =>
        if (StateManager.root_.asInstanceOf[Root].fields.exists(f => access.name == f.identifier.name))
          access.resolveToFieldAccess
        else if (specialFields.contains(access.name))
          access.resolveToSpecialFieldAccess
        else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => access.name == s.identifier.name))
          access.resolveToStencilAccess
        else if (StateManager.root_.asInstanceOf[Root].stencilFields.exists(s => access.name == s.identifier.name))
          access.resolveToStencilFieldAccess
        else access.resolveToBasicOrLeveledAccess
    }))

    this.execute(new Transformation("special functions and constants", {
      // geometricCoordinate
      case FunctionCallExpression(LeveledAccess("geometricCoordinate_x", level), List()) => BasicAccess("xPos")
      case FunctionCallExpression(LeveledAccess("geometricCoordinate_y", level), List()) => BasicAccess("yPos")
      case FunctionCallExpression(LeveledAccess("geometricCoordinate_z", level), List()) => BasicAccess("zPos")
      case FunctionCallExpression(BasicAccess("geometricCoordinate_x"), List()) => BasicAccess("xPos")
      case FunctionCallExpression(BasicAccess("geometricCoordinate_y"), List()) => BasicAccess("yPos")
      case FunctionCallExpression(BasicAccess("geometricCoordinate_z"), List()) => BasicAccess("zPos")
      case FunctionCallExpression(LeveledAccess("geometricCoordinate", level), List(IntegerConstant(0))) => BasicAccess("xPos")
      case FunctionCallExpression(LeveledAccess("geometricCoordinate", level), List(IntegerConstant(1))) => BasicAccess("yPos")
      case FunctionCallExpression(LeveledAccess("geometricCoordinate", level), List(IntegerConstant(2))) => BasicAccess("zPos")

      // gridWidth
      case FunctionCallExpression(LeveledAccess("gridWidth_x", SingleLevelSpecification(level)), List()) => FloatConstant(knowledge.Knowledge.discr_hx(level - knowledge.Knowledge.minLevel))
      case FunctionCallExpression(LeveledAccess("gridWidth_y", SingleLevelSpecification(level)), List()) => FloatConstant(knowledge.Knowledge.discr_hy(level - knowledge.Knowledge.minLevel))
      case FunctionCallExpression(LeveledAccess("gridWidth_z", SingleLevelSpecification(level)), List()) => FloatConstant(knowledge.Knowledge.discr_hz(level - knowledge.Knowledge.minLevel))
      case FunctionCallExpression(LeveledAccess("gridWidth", SingleLevelSpecification(level)), List(IntegerConstant(0))) => FloatConstant(knowledge.Knowledge.discr_hx(level - knowledge.Knowledge.minLevel))
      case FunctionCallExpression(LeveledAccess("gridWidth", SingleLevelSpecification(level)), List(IntegerConstant(1))) => FloatConstant(knowledge.Knowledge.discr_hy(level - knowledge.Knowledge.minLevel))
      case FunctionCallExpression(LeveledAccess("gridWidth", SingleLevelSpecification(level)), List(IntegerConstant(2))) => FloatConstant(knowledge.Knowledge.discr_hz(level - knowledge.Knowledge.minLevel))

      // levelIndex
      case FunctionCallExpression(LeveledAccess("levels", SingleLevelSpecification(level)), List()) => IntegerConstant(level)
      case FunctionCallExpression(LeveledAccess("levelIndex", SingleLevelSpecification(level)), List()) => IntegerConstant(level - knowledge.Knowledge.minLevel)
      case FunctionCallExpression(LeveledAccess("levelString", SingleLevelSpecification(level)), List()) => StringConstant(level.toString())

      // constants
      case BasicAccess("PI") | BasicAccess("M_PI") | BasicAccess("Pi") => FloatConstant(math.Pi)
    }))

    this.commit()
  }
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ AssignmentStatement(lhs : FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      var statements : ListBuffer[Statement] =
        CollectCommInformation.commCollector.communicates.map(comm =>
          CommunicateStatement(comm._1, "both", List( /* FIXME: add radius */ )) : Statement).to[ListBuffer]

      statements += LoopOverFragmentsStatement(List(
        LoopOverPointsStatement(lhs, None, false, None, None, None, None, List(assignment), None)),
        None)

      statements
    }

    // FIXME: handle reductions
    // FIXME: handle stencil fields
    // FIXME: handle region loops
  }, false /* recursion must be switched of due to wrapping mechanism */ )
}
