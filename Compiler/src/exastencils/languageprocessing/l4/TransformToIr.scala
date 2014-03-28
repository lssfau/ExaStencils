package exastencils.languageprocessing.l4

import exastencils.datastructures.Strategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.l4.FunctionStatement
import exastencils.datastructures.l4.Identifier
import exastencils.datastructures.l4.SingleLevelSpecification
import exastencils.datastructures.l4.ListLevelSpecification
import exastencils.datastructures.l4.LevelSpecification
import exastencils.datastructures.l4.SingleLevelSpecification
import exastencils.datastructures.l4.ListLevelSpecification
import scala.collection.mutable.ListBuffer
import exastencils.core.Duplicate
import exastencils.datastructures.l4.RangeLevelSpecification
import exastencils.datastructures.l4.SingleLevelSpecification
import exastencils.datastructures.l4.SingleLevelSpecification
import exastencils.core.ERROR

class TransformToIr {
  var strategy = new Strategy("TransformToIr")

  def doDuplicateFunction(function : FunctionStatement, l : LevelSpecification) : List[FunctionStatement] = {
    var functions = new ListBuffer[FunctionStatement]()
    l match {
      case x : SingleLevelSpecification => functions += function
      case x : ListLevelSpecification => {
        x.levels.foreach(level => {
          functions ++= doDuplicateFunction(function, level)
        })
      }
      case x : RangeLevelSpecification => {
        for (i <- math.min(x.begin, x.end) until math.max(x.begin, x.end)) {
          var f = Duplicate(function)
          f.identifier = new Identifier(f.identifier.name, Some(SingleLevelSpecification(i)))
          functions += f
        }
      }
      case _ => ERROR(s"Invalid level specification for function $function: $l")
    }
    return functions.toList
  }

  def apply() = {
    strategy += new Transformation("UnfoldLeveledFunctions", {
      case f : FunctionStatement => f.identifier match {
        case Identifier(name, Some(level)) => level match {
          case _ => doDuplicateFunction(f, level)
        }
      }
    })

    strategy.apply
  }

}