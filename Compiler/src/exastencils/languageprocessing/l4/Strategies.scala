package exastencils.languageprocessing.l4

import scala.collection.mutable.{ Node => _, _ }

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.l4._
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
