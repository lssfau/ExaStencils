package exastencils.languageprocessing.l4

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge
import exastencils.l4.L4_Communicate
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
      case L4_FunctionCall(access : L4_UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getKnowledge" == access.name =>
        resolveParameterToConstant(knowledge.Knowledge, ident)
      case L4_FunctionCall(access : L4_UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getSetting" == access.name   =>
        resolveParameterToConstant(Settings, ident)
      case L4_FunctionCall(access : L4_UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getPlatform" == access.name  =>
        resolveParameterToConstant(knowledge.Platform, ident)

      // levelIndex
      case L4_FunctionCall(L4_UnresolvedAccess("levels", _, Some(L4_SingleLevel(level)), _, _, _), ListBuffer())      =>
        L4_IntegerConstant(level)
      case L4_FunctionCall(L4_UnresolvedAccess("levelIndex", _, Some(L4_SingleLevel(level)), _, _, _), ListBuffer())  =>
        L4_IntegerConstant(level - knowledge.Knowledge.minLevel)
      case L4_FunctionCall(L4_UnresolvedAccess("levelString", _, Some(L4_SingleLevel(level)), _, _, _), ListBuffer()) =>
        L4_StringConstant(level.toString)

      // constants
      case access : L4_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
        L4_RealConstant(math.Pi)
    }))

    this.execute(new Transformation("Resolving string constants to literals", {
      case f : L4_FunctionCall =>
        f.function.name match {
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
    case origAccess : L4_UnresolvedAccess if replacements.exists(_._1 == origAccess.name) => {
      // includes accesses used as identifiers in function calls
      var newAccess = Duplicate(replacements.get(origAccess.name).get)
      newAccess match {
        case newAccess : L4_UnresolvedAccess => {
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
        case _                               =>
      }
      newAccess
    }
  })
}

object WrapL4FieldOpsStrategy extends DefaultStrategy("Adding communcation and loops to L4 statements") {
  this += new Transformation("Search and wrap", {
    case assignment @ L4_Assignment(lhs : L4_FieldAccess, rhs, op) => {
      CollectCommInformation.applyStandalone(assignment)

      var commStatements = CollectCommInformation.commCollector.communicates.map(comm =>
        L4_Communicate(comm._1, "both", List(/* FIXME: add radius */), None)).toList

      L4_LoopOverFragments(List(
        L4_LoopOverField(lhs, None, false, None, None, None, None, List(assignment), None, commStatements, List())),
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
