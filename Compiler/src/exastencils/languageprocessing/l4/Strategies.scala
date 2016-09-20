package exastencils.languageprocessing.l4

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.field.l4._
import exastencils.knowledge
import exastencils.logger._
import exastencils.stencil.l4._

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

// FIXME: name
object ResolveL4_Pre extends DefaultStrategy("Resolving L4 specifics") {
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

    this.execute(new Transformation("special functions and constants", {
      // get knowledge/settings/platform
      case L4_FunctionCall(access : UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getKnowledge" == access.name =>
        resolveParameterToConstant(knowledge.Knowledge, ident)
      case L4_FunctionCall(access : UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getSetting" == access.name   =>
        resolveParameterToConstant(Settings, ident)
      case L4_FunctionCall(access : UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getPlatform" == access.name  =>
        resolveParameterToConstant(knowledge.Platform, ident)

      // levelIndex
      case L4_FunctionCall(UnresolvedAccess("levels", _, Some(SingleLevelSpecification(level)), _, _, _), ListBuffer())      =>
        L4_IntegerConstant(level)
      case L4_FunctionCall(UnresolvedAccess("levelIndex", _, Some(SingleLevelSpecification(level)), _, _, _), ListBuffer())  =>
        L4_IntegerConstant(level - knowledge.Knowledge.minLevel)
      case L4_FunctionCall(UnresolvedAccess("levelString", _, Some(SingleLevelSpecification(level)), _, _, _), ListBuffer()) =>
        L4_StringConstant(level.toString)

      // constants
      case access : UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
        L4_RealConstant(math.Pi)
    }))

    this.execute(new Transformation("Resolving string constants to literals", {
      case f : L4_FunctionCall =>
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

object ResolveL4_Post extends DefaultStrategy("Resolving L4 specifics") {
  // resolve accesses
  this += new Transformation("Resolve AccessSpecifications", {
    case access : UnresolvedAccess =>
      if (L4_FieldCollection.exists(access.name))
        access.resolveToFieldAccess
      else if (L4_StencilCollection.exists(access.name))
        access.resolveToStencilAccess
      else if (L4_StencilFieldCollection.exists(access.name))
        access.resolveToStencilFieldAccess
      else access.resolveToBasicOrLeveledAccess
  })
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
  def fromFieldAccess(access : L4_FieldAccess) : CombinedIdentifier = {
    val level = access.target.level
    CombinedIdentifier(access.name, level)
  }
  def fromStencilFieldAccess(access : L4_StencilFieldAccess) : CombinedIdentifier = {
    val level = access.target.level
    val fieldName = L4_StencilFieldCollection.getByIdentifier(access.name, level).get.field.identifier
    CombinedIdentifier(fieldName, level)
  }

  var bcs : HashMap[CombinedIdentifier, L4_FunctionCall] = HashMap()

  override def apply(node : Option[Node] = None) = {
    bcs.clear

    // gather applicable fields
    for (field <- L4_FieldCollection.objects) {
      if (field.boundary.isDefined) {
        if (field.boundary.get.isInstanceOf[L4_FunctionCall]) {
          val fctCall = field.boundary.get.asInstanceOf[L4_FunctionCall]
          val fctDecl = StateManager.root.asInstanceOf[Root].functions.find {
            case f : L4_Function if f.identifier.isInstanceOf[LeveledIdentifier]
              && fromIdentifier(f.identifier) == fromLeveledAccess(fctCall.identifier) => true
            case _                                                                     => false
          }.get
          if (fctDecl.returntype eq L4_UnitDatatype) {
            bcs(CombinedIdentifier(field.identifier, field.level)) = fctCall
          }
        }
      }
    }

    super.apply(node)

    // remove obsolete field bc's
    for (field <- L4_FieldCollection.objects) {
      val fctCall = bcs.find(_._1 == CombinedIdentifier(field.identifier, field.level))
      if (fctCall.isDefined)
        field.boundary = None
      field
    }
  }

  this += new Transformation("Find applicable fields", {
    case field : L4_FieldDecl => {
      if (field.boundary.isDefined) {
        if (field.boundary.get.isInstanceOf[L4_FunctionCall]) {
          val fctCall = field.boundary.get.asInstanceOf[L4_FunctionCall]
          val fctDecl = StateManager.root.asInstanceOf[Root].functions.find {
            case f : L4_Function if f.identifier.isInstanceOf[LeveledIdentifier]
              && fromIdentifier(f.identifier) == fromLeveledAccess(fctCall.identifier) => true
            case _                                                                     => false
          }.get
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
        case field : L4_FieldAccess     => fromFieldAccess(field)
        case sf : L4_StencilFieldAccess => fromStencilFieldAccess(sf)
        case _                          => Logger.warn(_) // FIXME WTF ?
      }
      val fctCall = bcs.find(_._1 == field)
      if (fctCall.isDefined)
        L4_ExpressionStatement(Duplicate(fctCall.get._2))
      else
        applyBC
    }
  })
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ AssignmentStatement(lhs : L4_FieldAccess, rhs, op) => {
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
