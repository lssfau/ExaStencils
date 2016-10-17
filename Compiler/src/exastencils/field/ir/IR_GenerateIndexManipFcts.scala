package exastencils.field.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.datastructures.ir.iv
import exastencils.deprecated.ir.IR_DimToString

/// IR_GenerateIndexManipFcts

object IR_GenerateIndexManipFcts extends DefaultStrategy("Generate index manipulation functions") {
  // collect indices -> per layout/level
  // add function with
  //  get old_inner IE-IB
  //  calculate offset as new_inner - old_inner
  //  adapt IE,GRB,GRE,etc with +offset

  var layoutMap = mutable.HashMap[String, (String, IR_Expression)]()

  override def apply(node : Option[Node] = None) = {
    layoutMap.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    layoutMap.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case idx : iv.IndexFromField =>
      layoutMap += (s"${ idx.layoutIdentifier }_${ idx.level.prettyprint }" -> (idx.layoutIdentifier, idx.level))
      idx
  })

  this += new Transformation("Generate functions", {
    case functions : IR_UserFunctions =>
      for (layout <- layoutMap) {
        var body = ListBuffer[IR_Statement]()
        def newInnerSize(dim : Integer) = IR_VariableAccess(s"newInnerSize_${ IR_DimToString(dim) }", IR_IntegerDatatype)
        def idxShift(dim : Integer) = IR_VariableAccess(s"idxShift_${ IR_DimToString(dim) }", IR_IntegerDatatype)

        // compile body for all dimensions - TODO: adapt to field layout dimensionality if required
        for (dim <- 0 until Knowledge.dimensionality) {
          // calculate index shift
          body += IR_VariableDeclaration(idxShift(dim), newInnerSize(dim) - (
            iv.IndexFromField(layout._2._1, layout._2._2, "IE", dim) -
              iv.IndexFromField(layout._2._1, layout._2._2, "IB", dim)))

          // adapt indices
          for (idxIdent <- List("IE", "DRB", "DRE", "GRB", "GRE", "PRB", "PRE", "TOT")) {
            body += IR_Assignment(
              iv.IndexFromField(layout._2._1, layout._2._2, idxIdent, dim),
              idxShift(dim), "+=")
          }
        }

        // wrap body in fragment loop
        body = ListBuffer[IR_Statement](IR_LoopOverFragments(body))

        // set up function
        functions += IR_Function(
          IR_UnitDatatype,
          s"resizeInner_${ layout._2._1 }_${ layout._2._2.prettyprint }",
          Knowledge.dimensions.map(dim => IR_FunctionArgument(newInnerSize(dim))).to[ListBuffer],
          body,
          false) // no inlining
      }

      // generate a special resize functions for all fields on a given level
      for (level <- Knowledge.maxLevel to Knowledge.minLevel by -1) {
        var body = ListBuffer[IR_Statement]()
        def newInnerSize(dim : Integer) = IR_VariableAccess(s"newInnerSize_${ IR_DimToString(dim) }", IR_IntegerDatatype)

        // generate function calls with adapted sizes
        for (layout <- layoutMap.filter(level == _._2._2.prettyprint.toInt).toSeq.sortBy(_._1)) {
          body += IR_FunctionCall(s"resizeInner_${ layout._2._1 }_${ layout._2._2.prettyprint }",
            Knowledge.dimensions.map(dim => newInnerSize(dim) : IR_Expression).to[ListBuffer])
        }

        // set up function
        functions += IR_Function(
          IR_UnitDatatype,
          s"resizeAllInner_${ level.prettyprint() }",
          Knowledge.dimensions.map(dim => IR_FunctionArgument(newInnerSize(dim))).to[ListBuffer],
          body,
          false) // no inlining
      }

      // return extended collection
      functions
  })
}
