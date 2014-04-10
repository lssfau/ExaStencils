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
import exastencils.datastructures.l4
import exastencils.datastructures.ir
import exastencils.datastructures.Node
import exastencils.datastructures.ir.ComplexDatatype
import exastencils.core.ImplicitConversions._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir.BinaryExpression
import exastencils.datastructures.l4.FieldDeclarationStatement
import exastencils.datastructures.l4.FieldDeclarationStatement
import exastencils.datastructures.l4.FieldDeclarationStatement
import exastencils.datastructures.l4.SingleLevelSpecification

object ProgressToIr extends Strategy("ProgressToIr") {

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

  /*  def doTransformToIr(node : l4.Datatype) : ir.Datatype = {
    node match {
      case x : l4.IntegerDatatype => new ir.IntegerDatatype
      case x : l4.StringDatatype  => new ir.StringDatatype
      case x : l4.UnitDatatype    => new ir.StringDatatype
      case x : l4.ArrayDatatype   => new ir.ArrayDatatype(doTransformToIr(x.datatype))
      case x : l4.ComplexDatatype => new ir.ComplexDatatype(doTransformToIr(x.datatype))
    }
  }

  def resolveLeveledName(id : l4.Identifier) : String = s"${id.name}__level_${
    id.level match {
      case Some(x) => x.toString()
      case None    => "_"
    }
  }"

  def doTransformToIr(node : l4.Variable) : ir.VariableAccess = {
    new ir.VariableAccess(resolveLeveledName(node.identifier), Some(doTransformToIr(node.datatype)))
  }

  def doTransformToIr(node : l4.Expression) : ir.Expression = {
    node match {
      case l4.StringConstant(x)  => ir.StringConstant(x)
      case l4.IntegerConstant(x) => ir.IntegerConstant(x)
      case l4.FloatConstant(x)   => ir.FloatConstant(x)
      case l4.BooleanConstant(x) => ir.BooleanConstant(x)
      case _                     => ERROR(s"No rule for progression of L4 node ${node}")
    }
  }

  def doTransformToIr(node : l4.Statement) : ir.Statement = {
    node match {
      case x : l4.FunctionStatement => ir.FunctionStatement(doTransformToIr(x.returntype),
        resolveLeveledName(x.identifier),
        x.arguments.map(doTransformToIr(_)),
        x.statements.map(doTransformToIr(_)))
      case x : l4.RepeatUpStatement => new ir.ForLoopStatement(
        "int" ~ "i" ~ "=" ~ "0", // FIXME
        ir.BinaryExpression(ir.BinaryOperators.Lower, ir.VariableAccess("i"), ir.IntegerConstant(x.number)),
        "i" ~ "=" ~ "i" ~ "+" ~ "1", // FIXME
        x.statements.map(doTransformToIr(_)))
      case x : l4.FunctionCallStatement => new ir.ExpressionStatement(new ir.FunctionCallExpression(
        resolveLeveledName(x.identifier), x.arguments.map(doTransformToIr(_))))
      case _ => ERROR(s"No rule for progression of L4 node ${node}")
    }
  }
*/
  // def apply() = {
  this += new Transformation("UnfoldLeveledFunctions", {
    case f : FunctionStatement => f.identifier match {
      case Identifier(name, Some(level)) => level match {
        case _ => doDuplicateFunction(f, level)
      }
      case Identifier(name, None) => f
    }
  })

  //    var root = new ir.Root()

  /*    strategy += new Transformation("ProgressNodesToIr", {
      case x : l4.Statement => root += doTransformToIr(x); x
    })
*/
  //   this.apply
  //    println("new root:" + root)
  // }

  def duplicateFields(field : FieldDeclarationStatement, level : LevelSpecification) : List[FieldDeclarationStatement] = {
    var fields = new ListBuffer[FieldDeclarationStatement]()
    level match {
      case level : SingleLevelSpecification =>
        fields += field
      case level : ListLevelSpecification =>
        level.levels.foreach(level => fields ++= duplicateFields(field, level))
      case levels : RangeLevelSpecification =>
        for (level <- math.min(levels.begin, levels.end) to math.max(levels.begin, levels.end)) {
          var f = Duplicate(field)
          f.level = Some(SingleLevelSpecification(level))
          fields += f
        }
      case _ => ERROR(s"Invalid level specification for field $field: $level")
    }
    return fields.toList
  }

  this += new Transformation("UnfoldLeveledFieldDeclarations", {
    case field : FieldDeclarationStatement =>
      if (field.level.isEmpty)
        field
      else {
        duplicateFields(field, field.level.get)
      }
  })

}