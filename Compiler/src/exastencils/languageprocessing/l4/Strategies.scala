package exastencils.languageprocessing.l4

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
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
  val virtualFields : ListBuffer[String] = ListBuffer(
    "vf_nodePosition_x", "vf_nodePosition_y", "vf_nodePosition_z",
    "nodePosition_x", "nodePosition_y", "nodePosition_z",

    "vf_cellCenter_x", "vf_cellCenter_y", "vf_cellCenter_z",
    "cellCenter_x", "cellCenter_y", "cellCenter_z",

    "vf_boundaryCoord_x", "vf_boundaryCoord_y", "vf_boundaryCoord_z",
    "boundaryCoord_x", "boundaryCoord_y", "boundaryCoord_z",

    "vf_gridWidth_x", "vf_gridWidth_y", "vf_gridWidth_z",
    "gridWidth_x", "gridWidth_y", "gridWidth_z",

    "vf_cellWidth_x", "vf_cellWidth_y", "vf_cellWidth_z",
    "cellWidth_x", "cellWidth_y", "cellWidth_z",

    "vf_stagCVWidth_x", "vf_stagCVWidth_y", "vf_stagCVWidth_z",
    "stagCVWidth_x", "stagCVWidth_y", "stagCVWidth_z",

    "vf_cellVolume", "vf_xStagCellVolume", "vf_yStagCellVolume", "vf_zStagCellVolume",
    "cellVolume", "xStagCellVolume", "yStagCellVolume", "zStagCellVolume",

    "vf_cellCenterToFace_x", "vf_cellCenterToFace_y", "vf_cellCenterToFace_z").map(_.toLowerCase())

  def resolveParameterToConstant(obj : AnyRef, ident : String) : L4_Expression = {
    val ret = obj.getClass.getMethod(ident).invoke(obj)

    if (ret.isInstanceOf[Int]) L4_IntegerConstant(ret.asInstanceOf[Int])
    else if (ret.isInstanceOf[Float]) L4_RealConstant(ret.asInstanceOf[Float])
    else if (ret.isInstanceOf[Boolean]) L4_BooleanConstant(ret.asInstanceOf[Boolean])
    else if (ret.isInstanceOf[String]) L4_StringConstant(ret.asInstanceOf[String])
    else Logger.error(s"Trying to access parameter $ident from L4 with unsupported type")
  }

  override def apply(applyAtNode : Option[Node]) = {
    this.transaction()
    var valueCollector = new L4ValueCollector

    // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
    this.register(valueCollector)

    this.execute(new Transformation("Resolve Values in Expressions", {
      case x : UnresolvedAccess if (x.level == None && x.slot == None && x.arrayIndex == None)                                                         => {
        var value = valueCollector.getValue(x.name)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${ x.name } as no matching Val was found"""); x }
          case _    => Duplicate(value.get)
        }
      }
      case x : UnresolvedAccess if (x.level.isDefined && x.level.get.isInstanceOf[SingleLevelSpecification] && x.slot == None && x.arrayIndex == None) => {
        var value = valueCollector.getValue(x.name + "@@" + x.level.get.asInstanceOf[SingleLevelSpecification].level)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${ x.name } as no matching Val was found"""); x }
          case _    => Duplicate(value.get)
        }
      }
    }))
    this.unregister(valueCollector)

    // resolve globals (lower precendence than local values!)
    val globalVals = collection.mutable.HashMap[String, L4_Expression]()
    StateManager.root_.asInstanceOf[l4.Root].globals.foreach(g => g.values.foreach(x => x.identifier match {
      case v : LeveledIdentifier => globalVals += ((v.name + "@@" + v.level, x.expression))
      case _                     => globalVals += ((x.identifier.name, x.expression))
    }))

    this.execute(new Transformation("Resolve Global Values", {
      case x : UnresolvedAccess if (x.level == None && x.slot == None && x.arrayIndex == None)                                                         => {
        var value = globalVals.get(x.name)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${ x.name } as no matching Global Val was found"""); x }
          case _    => Duplicate(value.get)
        }
      }
      case x : UnresolvedAccess if (x.level.isDefined && x.level.get.isInstanceOf[SingleLevelSpecification] && x.slot == None && x.arrayIndex == None) => {
        var value = globalVals.get(x.name + "@@" + x.level.get.asInstanceOf[SingleLevelSpecification].level)
        value match {
          case None => { Logger.info(s"""Could not resolve identifier ${ x.name } as no matching Global Val was found"""); x }
          case _    => Duplicate(value.get)
        }
      }
    }))
    globalVals.clear()

    // resolve accesses
    this.execute(new Transformation("Resolve AccessSpecifications", {
      case access : UnresolvedAccess =>
        if (StateManager.root_.asInstanceOf[Root].fields.exists(f => access.name == f.identifier.name))
          access.resolveToFieldAccess
        else if (virtualFields.contains(access.name.toLowerCase()))
          access.resolveToVirtualFieldAccess
        else if (StateManager.root_.asInstanceOf[Root].stencils.exists(s => access.name == s.identifier.name))
          access.resolveToStencilAccess
        else if (StateManager.root_.asInstanceOf[Root].stencilFields.exists(s => access.name == s.identifier.name))
          access.resolveToStencilFieldAccess
        else access.resolveToBasicOrLeveledAccess
    }))

    this.execute(new Transformation("special functions and constants", {
      // get knowledge/settings/platform
      case FunctionCallExpression(BasicAccess("getKnowledge"), List(L4_StringConstant(ident))) =>
        resolveParameterToConstant(knowledge.Knowledge, ident)
      case FunctionCallExpression(BasicAccess("getSetting"), List(L4_StringConstant(ident)))   =>
        resolveParameterToConstant(Settings, ident)
      case FunctionCallExpression(BasicAccess("getPlatform"), List(L4_StringConstant(ident)))  =>
        resolveParameterToConstant(knowledge.Platform, ident)

      // levelIndex
      case FunctionCallExpression(LeveledAccess("levels", SingleLevelSpecification(level)), List())      => L4_IntegerConstant(level)
      case FunctionCallExpression(LeveledAccess("levelIndex", SingleLevelSpecification(level)), List())  => L4_IntegerConstant(level - knowledge.Knowledge.minLevel)
      case FunctionCallExpression(LeveledAccess("levelString", SingleLevelSpecification(level)), List()) => L4_StringConstant(level.toString())

      // constants
      case BasicAccess("PI") | BasicAccess("M_PI") | BasicAccess("Pi") => L4_RealConstant(math.Pi)
    }))

    this.execute(new Transformation("Resolving string constants to literals", {
      case f : FunctionCallExpression =>
        f.identifier.name match {
          case "startTimer"        => f.arguments = f.arguments.map(a => if (a.isInstanceOf[L4_StringConstant]) L4_StringLiteral(a.asInstanceOf[L4_StringConstant].value); else a)
          case "stopTimer"         => f.arguments = f.arguments.map(a => if (a.isInstanceOf[L4_StringConstant]) L4_StringLiteral(a.asInstanceOf[L4_StringConstant].value); else a)
          case "getMeanFromTimer"  => f.arguments = f.arguments.map(a => if (a.isInstanceOf[L4_StringConstant]) L4_StringLiteral(a.asInstanceOf[L4_StringConstant].value); else a)
          case "getMeanTime"       => f.arguments = f.arguments.map(a => if (a.isInstanceOf[L4_StringConstant]) L4_StringLiteral(a.asInstanceOf[L4_StringConstant].value); else a)
          case "getTotalFromTimer" => f.arguments = f.arguments.map(a => if (a.isInstanceOf[L4_StringConstant]) L4_StringLiteral(a.asInstanceOf[L4_StringConstant].value); else a)
          case "getTotalTime"      => f.arguments = f.arguments.map(a => if (a.isInstanceOf[L4_StringConstant]) L4_StringLiteral(a.asInstanceOf[L4_StringConstant].value); else a)
          case _                   =>
        }
        f
    }))

    //    var variableCollector = new L4VariableCollector()
    //    this.register(variableCollector)
    //    this.execute(new Transformation("Resolving to Variable Accesses", {
    //      case x : BasicAccess => {
    //        var value = variableCollector.getValue(x.name)
    //        if (value.isDefined) {
    //          Logger.warn("VarResolve: found:     " + x.name)
    //          //          VariableAccess(x.name, None, value.get)
    //          x
    //        } else {
    //          Logger.warn("VarResolve: not found: " + x.name)
    //          x
    //        }
    //      }
    //    }))
    //    this.unregister(variableCollector)

    this.commit()
  }
}

object ReplaceExpressions extends DefaultStrategy("Replace something with something else") {
  var replacements : Map[String, L4_Expression] = Map()

  override def applyStandalone(node : Node) = {
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    super.applyStandalone(node)
    Logger.setLevel(oldLvl)
  }

  this += new Transformation("SearchAndReplace", {
    case origAccess : UnresolvedAccess if replacements.exists(_._1 == origAccess.name) => {
      // includes accesses used as identifiers in function calls
      var newAccess = Duplicate(replacements.get(origAccess.name).get)
      newAccess match {
        case newAccess : UnresolvedAccess => {
          if (origAccess.slot.isDefined) {
            if (newAccess.slot.isDefined) Logger.warn("Overriding slot on access in function instantiation")
            newAccess.slot = origAccess.slot
          }
          if (origAccess.level.isDefined) {
            if (newAccess.level.isDefined) Logger.warn("Overriding level on access in function instantiation")
            newAccess.level = origAccess.level
          }
          if (origAccess.offset.isDefined) {
            if (newAccess.offset.isDefined) Logger.warn("Overriding offset on access in function instantiation")
            newAccess.offset = origAccess.offset
          }
          if (origAccess.arrayIndex.isDefined) {
            if (newAccess.arrayIndex.isDefined) Logger.warn("Overriding array index on access in function instantiation")
            newAccess.arrayIndex = origAccess.arrayIndex
          }
          if (origAccess.dirAccess.isDefined) {
            if (newAccess.dirAccess.isDefined) Logger.warn("Overriding direction access on access in function instantiation")
            newAccess.dirAccess = origAccess.dirAccess
          }
        }
        case _                            =>
      }
      newAccess
    }
  })
}

object ResolveFunctionTemplates extends DefaultStrategy("Resolving function templates and instantiations") {
  this += new Transformation("Find and resolve", {
    case functionInst : FunctionInstantiationStatement => {
      val template = StateManager.root.asInstanceOf[Root].functionTemplates.find(_.name == functionInst.templateName)
      if (template.isEmpty) Logger.warn(s"Trying to instantiate unknown function template ${ functionInst.templateName }")
      var instantiated = Duplicate(FunctionStatement(functionInst.targetFct, template.get.returntype, template.get.functionArgs, template.get.statements))

      ReplaceExpressions.replacements = Map() ++ (template.get.templateArgs zip functionInst.args).toMap[String, L4_Expression]
      ReplaceExpressions.applyStandalone(instantiated)
      StateManager.root.asInstanceOf[Root].functions += instantiated
      None
    }
  })

  //  this += new Transformation("Remove function templates", {
  //    case root : Root =>
  //      root.functionTemplates.clear; root
  //    case functionTemplate : FunctionTemplateStatement => None
  //  })
}

object ResolveBoundaryHandlingFunctions extends DefaultStrategy("ResolveBoundaryHandlingFunctions") {

  case class CombinedIdentifier(var name : String, var level : Int) {}

  def fromIdentifier(ident : Identifier) : CombinedIdentifier = {
    val level = ident.asInstanceOf[LeveledIdentifier].level.asInstanceOf[SingleLevelSpecification].level
    CombinedIdentifier(ident.name, level)
  }
  def fromLeveledAccess(access : Access) : CombinedIdentifier = {
    val level = access.asInstanceOf[LeveledAccess].level.asInstanceOf[SingleLevelSpecification].level
    CombinedIdentifier(access.asInstanceOf[LeveledAccess].name, level)
  }
  def fromFieldAccess(access : FieldAccess) : CombinedIdentifier = {
    val level = access.level.asInstanceOf[SingleLevelSpecification].level
    CombinedIdentifier(access.name, level)
  }
  def fromStencilFieldAccess(access : StencilFieldAccess) : CombinedIdentifier = {
    val level = access.level.asInstanceOf[SingleLevelSpecification].level
    val fieldName = StateManager.root.asInstanceOf[Root].stencilFields.find(sf => fromIdentifier(sf.identifier) == CombinedIdentifier(access.name, level)).get.fieldName
    CombinedIdentifier(fieldName, level)
  }

  var bcs : HashMap[CombinedIdentifier, FunctionCallExpression] = HashMap()

  override def apply(node : Option[Node] = None) = {
    bcs.clear
    super.apply(node)
  }

  this += new Transformation("Find applicable fields", {
    case field : FieldDeclarationStatement => {
      if (field.boundary.isDefined) {
        if (field.boundary.get.isInstanceOf[FunctionCallExpression]) {
          val fctCall = field.boundary.get.asInstanceOf[FunctionCallExpression]
          val fctDecl = StateManager.root.asInstanceOf[Root].functions.find(
            _ match {
              case f : FunctionStatement if f.identifier.isInstanceOf[LeveledIdentifier]
                && fromIdentifier(f.identifier) == fromLeveledAccess(fctCall.identifier) => true
              case _                                                                     => false
            }).get.asInstanceOf[FunctionStatement]
          if (fctDecl.returntype eq L4_UnitDatatype) {
            bcs(fromIdentifier(field.identifier)) = fctCall
          }
        }
      }
      field
    }
  })

  this += new Transformation("Replace applicable boundary condition updates", {
    case applyBC : ApplyBCsStatement => {
      val field = applyBC.field match {
        case field : FieldAccess     => fromFieldAccess(field)
        case sf : StencilFieldAccess => fromStencilFieldAccess(sf)
        case _                       => Logger.warn(_) // FIXME WTF ?
      }
      val fctCall = bcs.find(_._1 == field)
      if (fctCall.isDefined)
        FunctionCallStatement(Duplicate(fctCall.get._2))
      else
        applyBC
    }
  })

  this += new Transformation("Remove obsolete field bc's", {
    case field : FieldDeclarationStatement => {
      val fctCall = bcs.find(_._1 == fromIdentifier(field.identifier))
      if (fctCall.isDefined)
        field.boundary = None
      field
    }
  })
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ AssignmentStatement(lhs : FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      var commStatements = CollectCommInformation.commCollector.communicates.map(comm =>
        CommunicateStatement(comm._1, "both", List(/* FIXME: add radius */), None)).toList

      LoopOverFragmentsStatement(List(
        LoopOverPointsStatement(lhs, None, false, None, None, None, None, List(assignment), None, commStatements, List())),
        None)
    }

    // FIXME: handle reductions
    // FIXME: handle stencil fields
    // FIXME: handle region loops
  }, false /* recursion must be switched of due to wrapping mechanism */)
}

//object UnifyInnerTypes extends DefaultStrategy("Unify inner types of (constant) vectors and matrices") {
//  var vectors = ListBuffer[VectorExpression]()
//  var matrices = ListBuffer[MatrixExpression]()
//
//  override def apply(applyAtNode : Option[Node]) = {
//    this.execute(new Transformation("Find vectors and matrices", {
//      case x : VectorExpression =>
//        vectors.+=(x); x
//      case x : MatrixExpression => matrices.+=(x); x
//    }))
//
//    vectors.foreach(vector => {
//      if (vector.isConstant) {
//        var reals = vector.expressions.filter(_.isInstanceOf[FloatConstant]).length
//        var ints = vector.expressions.filter(_.isInstanceOf[IntegerConstant]).length
//        if (ints > 0 && reals > 0) {
//          vector.expressions = vector.expressions.map(e => if (e.isInstanceOf[FloatConstant]) e; else FloatConstant(e.asInstanceOf[IntegerConstant].v))
//        }
//      }
//    })
//
//    matrices.foreach(matrix => {
//      if (matrix.isConstant) {
//        var reals = matrix.expressions.collect { case x : VectorExpression => x.expressions.filter(_.isInstanceOf[FloatConstant]).length } reduce ((a, b) => a + b)
//        var ints = matrix.expressions.collect { case x : VectorExpression => x.expressions.filter(_.isInstanceOf[IntegerConstant]).length } reduce ((a, b) => a + b)
//        if (ints > 0 && reals > 0) {
//          matrix.expressions.foreach(exp => {
//            exp.expressions = exp.expressions.map(e => if (e.isInstanceOf[FloatConstant]) e; else FloatConstant(e.asInstanceOf[IntegerConstant].v))
//          })
//        }
//      }
//    })
//  }
//}
