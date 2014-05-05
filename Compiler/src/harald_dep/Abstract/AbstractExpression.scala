package harald_dep.Abstract

import scala.collection.mutable.ListBuffer
import harald_dep.dsl._
import harald_dep.Impl._
import harald_dep.ast.TreeManager
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._

sealed abstract class AbstractExpression {
  def value(context : Context) : String
  def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression
  def getvariables : ListBuffer[String] = ListBuffer()
}

case class AbstractBinaryOp(operator : BinaryOperators.Value, left : AbstractExpression, right : AbstractExpression) extends AbstractExpression {
  override def value(context : Context) = left.value(context)
  override def toString = left.toString + " " + operator + " " + right.toString

  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    // check for convolution M * v
    if (operator == BinaryOperators.Multiplication)
      left match {
        case AbstractVariable(id1, l1) => {
          val stencilCollection = StateManager.findFirst[StencilCollection]().get

          if (stencilCollection.getStencilByIdentifier(id1, 0).isDefined) {
            val stencil = stencilCollection.getStencilByIdentifier(id1, 0).get

            right match {
              case AbstractVariable(id2, l2) => {
                val levstr = l2.transform(scopeparas, modifier, scopetype)
                val fieldCollection = StateManager.findFirst[FieldCollection]().get

                val field : Field = id2 match {
                  case "solution" => fieldCollection.getFieldByIdentifier("Solution", levstr.cpp.toInt).get
                  case "Res"      => fieldCollection.getFieldByIdentifier("Residual", levstr.cpp.toInt).get
                  case "f"        => fieldCollection.getFieldByIdentifier("RHS", levstr.cpp.toInt).get
                }

                if (modifier.getOrElse("").equals("ToCoarse")) {
                  return StencilConvolution(stencil, field, new MultiIndex(DimArray().map(i => (2 * (dimToString(i) : Expression)) : Expression)))
                } else if (modifier.getOrElse("").equals("ToFine")) {
                  return StencilConvolution(stencil, field, new MultiIndex(DimArray().map(i => ((dimToString(i) : Expression) / 2) : Expression)))
                } else {
                  return StencilConvolution(stencil, field)
                }
              }
              case _ =>
            }
          }
        }
        case _ =>
      }

    return new BinaryExpression(operator, left.transform(scopeparas, modifier, scopetype), right.transform(scopeparas, modifier, scopetype))
  }
  override def getvariables : ListBuffer[String] = {
    var s1 = left.getvariables
    var s2 = right.getvariables
    for (s <- s2)
      s1 += s
    return s1
  }
}

case class AbstractFCall(fname : String, arglist : List[AbstractExpression]) extends AbstractExpression {
  override def value(context : Context) = "return"
  override def toString = fname + "(" + arglist.mkString(",") + ")"

  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    var args : ListBuffer[Expression] = ListBuffer()
    for (a <- arglist)
      args += a.transform(scopeparas, modifier, "argument")

    if (fname.equals("inverse")) {
      return (1.0 / arglist(0).transform(scopeparas, modifier, "argument"))
    }

    if (fname.equals("diag")) {
      var stencilCollection = StateManager.findFirst[StencilCollection]().get
      var curStencil = stencilCollection.getStencilByIdentifier(arglist(0).toString.substring(0, arglist(0).toString.size - 2) /* FIXME: avoid stripping level usage */ , 0).get
      return curStencil.entries(0).weight
    }
    if (fname.equals("random"))
      return "(rand()/static_cast<double>(RAND_MAX))" * args(0) // TODO
    if (fname.equals("fasterReduce") && DomainKnowledge.use_gpu)
      return new StringConstant("fasterReduce (Res[lev].begin(), solution[lev].x1_*solution[lev].x2_, f[lev].begin())") // TODO

    return new FunctionCallExpression(fname, args)

  }
}

case class AbstractLiteral(text : String) extends AbstractExpression {
  override def value(context : Context) = text
  override def toString = text
  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    return new StringConstant(text)
  }
}

case class AbstractIntLiteral(value : Long) extends AbstractExpression {
  override def value(context : Context) = value.toString
  override def toString = value.toString
  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    return new IntegerConstant(value)
  }
}
case class AbstractFloatLiteral(value : Double) extends AbstractExpression {
  override def value(context : Context) = value.toString
  override def toString = value.toString
  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    return new FloatConstant(value)
  }
}

case class AbstractStringLiteral(text : String) extends AbstractExpression {
  override def value(context : Context) = text
  override def toString = text
  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    return new StringConstant("\"" + text + "\"")
  }
}

case class AbstractVariable(id : String, lev : AbstractExpression) extends AbstractExpression {
  override def value(context : Context) = {
    context.resolve(id) match {
      case Some(binding) => binding.expr.value(context)
      case None          => throw new RuntimeException("Unknown identifier: " + id)
    }
  }
  override def toString = id + " " + lev

  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {

    var ti : TypeInfo = new TypeInfo(id, 0)
    for (e <- TreeManager.tree.exaFields)
      if (e.name.equals(id)) {
        // COMM_HACK
        return id match {
          case "solution" => (
            if ("statement" == scopetype || "expression" == scopetype) {
              //s"curFragment.solData[0][" ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp()
              val fieldCollection = StateManager.findFirst[FieldCollection]().get
              val field : Field = fieldCollection.getFieldByIdentifier("Solution", lev.transform(scopeparas, modifier, scopetype).cpp.toInt).get
              new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex())
            } else
              s"*curFragment.solData[0][FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]")
          case "Res" => (
            if ("statement" == scopetype || "expression" == scopetype) {
              //s"curFragment.resData[0][" ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp()
              val fieldCollection = StateManager.findFirst[FieldCollection]().get
              val field : Field = fieldCollection.getFieldByIdentifier("Residual", lev.transform(scopeparas, modifier, scopetype).cpp.toInt).get
              new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex())
            } else
              s"*curFragment.resData[0][FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]")
          case "f" => (
            if ("statement" == scopetype || "expression" == scopetype) {
              //s"curFragment.rhsData[0][" ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp()
              val fieldCollection = StateManager.findFirst[FieldCollection]().get
              val field : Field = fieldCollection.getFieldByIdentifier("RHS", lev.transform(scopeparas, modifier, scopetype).cpp.toInt).get
              new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex())
            } else
              s"*curFragment.rhsData[0][FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]")
          case _ => (
            if ("statement" == scopetype || "expression" == scopetype)
              id ~ "[FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ "]" ~ DomainKnowledge.rule_idxArray_cpp()
            else
              id ~ "[FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ "]")
        }
      }

    for (e <- TreeManager.tree.exaOperators)
      if (e.name.equals(id)) {
        if ("statement" == scopetype || "expression" == scopetype)
          return id ~ "[0]" ~ "(Matrix (i0,i1))"
        else
          return id ~ "[0]"
      }

    for (e <- scopeparas) {
      if (e.name.equals(id))
        if (e.dtype.startsWith("Container")) {
          ti = new TypeInfo(id, 1)
        }
    }

    if (id.contains("Stencil")) {
      if ("statement" == scopetype || "expression" == scopetype)
        return id ~ "[0]" ~ "(Matrix (i0,i1))"
      else
        return id ~ "[0]"
    }

    ti.d match {
      case 0 => return id
      case 1 => return id ~ DomainKnowledge.rule_idxArray_cpp()
      case 2 => return id ~ "(Matrix (i0,i1))"
    }
  }

  override def getvariables : ListBuffer[String] = {
    for (e <- TreeManager.tree.exaFields)
      if (e.name.equals(id))
        return ListBuffer(id)
    for (e <- TreeManager.tree.exaOperators)
      if (e.name.equals(id))
        return ListBuffer(id)
    return ListBuffer()
  }
}
