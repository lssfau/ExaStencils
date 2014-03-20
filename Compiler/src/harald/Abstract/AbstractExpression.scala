package harald.Abstract

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.Impl._
import harald.ast.TreeManager
import harald.expert.StencilGenerator
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
          for (e1 <- TreeManager.tree.Stencils)
            if (e1.name.equals(id1))
              right match {
                case AbstractVariable(id2, l2) => {
                  var lb : ListBuffer[Expression] = new ListBuffer()

                  val levstr = l2.transform(scopeparas, modifier, scopetype)
                  val fieldCollection = StateManager.findFirst[FieldCollection]().get

                  // COMM_HACK
                  if ("statement" == l2.transform(scopeparas, modifier, "argument") || "expression" == l2.transform(scopeparas, modifier, "argument")) {
                    id2 match {
                      case "solution" => //lb += "curFragment.solData[0][" ~ levstr ~ "]->getDataRef" ~ "[" ~ l2.transform(scopeparas, modifier, "argument") ~ "]"
                        val field : Field = fieldCollection.getFieldByIdentifier("Solution", levstr.cpp.toInt).get
                        lb += new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) /*TODO*/ .expand(new StackCollector)
                      case "Res" => //lb += "curFragment.resData[0][" ~ levstr ~ "]->getDataRef" ~ "[" ~ l2.transform(scopeparas, modifier, "argument") ~ "]"
                        val field : Field = fieldCollection.getFieldByIdentifier("Residual", levstr.cpp.toInt).get
                        lb += new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) /*TODO*/ .expand(new StackCollector)
                      case "f" => //lb += "curFragment.rhsData[0][" ~ levstr ~ "]->getDataRef" ~ "[" ~ l2.transform(scopeparas, modifier, "argument") ~ "]"
                        val field : Field = fieldCollection.getFieldByIdentifier("RHS", levstr.cpp.toInt).get
                        lb += new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) /*TODO*/ .expand(new StackCollector)
                      case _ => lb += id2 ~ "[" ~ l2.transform(scopeparas, modifier, "argument") ~ "]"
                    }
                  } else {
                    id2 match {
                      case "solution" => lb += "*curFragment.solData[0][" ~ levstr ~ "]"
                      case "Res"      => lb += "*curFragment.resData[0][" ~ levstr ~ "]"
                      case "f"        => lb += "*curFragment.rhsData[0][" ~ levstr ~ "]"
                      case _          => lb += id2
                    }
                  }

                  if (modifier.getOrElse("").equals("ToCoarse")) {

                    println(e1.entries.mkString(", "))

                    // temp classes
                    case class StencilEntry(var offset : MultiIndex, var weight : Expression) {}
                    case class Stencil(var entries : ListBuffer[StencilEntry] = new ListBuffer) extends Node {}
                    case class StencilConvolution(var stencil : Stencil, var field : Field, var targetIdx : MultiIndex = DefaultLoopMultiIndex()) extends Expression with Expandable {
                      override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n";

                      def expand(collector : StackCollector) : Expression = {
                        stencil.entries.map(e =>
                          e.weight * (new FieldAccess("curFragment.", field, 0, new MultiIndex(targetIdx, e.offset, _ + _))). /*FIXME*/ expand(new StackCollector))
                          .toArray[Expression].reduceLeft(_ + _)
                      }
                    }

                    // temp stencil
                    var stencil = new Stencil
                    for (i <- 0 until e1.length)
                      stencil.entries += StencilEntry(new MultiIndex(IdxKnowledge.StencilToidx(Knowledge.dimensionality, e1.length)(i).toArray), e1.entries(i)) // s"$id1[0].entries[$i]")
                    // find field
                    val field : Field = fieldCollection.getFieldByIdentifier("Residual", levstr.cpp.toInt).get

                    // temp conv
                    var conv = StencilConvolution(stencil, field, new MultiIndex((0 until Knowledge.dimensionality).toArray.map(i => (2 * (dimToString(i) : Expression) - 1) : Expression)))

                    return conv.expand(new StackCollector).cpp
                  } else if (modifier.getOrElse("").equals("ToFine")) {
                    // temp classes
                    case class StencilEntry(var offset : MultiIndex, var weight : Expression) {}
                    case class Stencil(var entries : ListBuffer[StencilEntry] = new ListBuffer) extends Node {}
                    case class StencilConvolution(var stencil : Stencil, var field : Field, var targetIdx : MultiIndex = DefaultLoopMultiIndex()) extends Expression with Expandable {
                      override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n";

                      def expand(collector : StackCollector) : Expression = {
                        stencil.entries.map(e =>
                          e.weight * (new FieldAccess("curFragment.", field, 0, new MultiIndex(targetIdx, e.offset, _ + _))). /*FIXME*/ expand(new StackCollector))
                          .toArray[Expression].reduceLeft(_ + _)
                      }
                    }

                    // temp stencil
                    var stencil = new Stencil
                    for (i <- 0 until e1.length)
                      stencil.entries += StencilEntry(new MultiIndex(IdxKnowledge.StencilToidx(Knowledge.dimensionality, e1.length)(i).toArray), e1.entries(i)) // s"$id1[0].entries[$i]")
                    // find field
                    val field : Field = fieldCollection.getFieldByIdentifier("Solution", levstr.cpp.toInt).get

                    // temp conv
                    var conv = StencilConvolution(stencil, field, new MultiIndex((0 until Knowledge.dimensionality).toArray.map(i => (((dimToString(i) : Expression) + 1) / 2) : Expression)))

                    return conv.expand(new StackCollector).cpp
                  } else {
                    // temp classes
                    case class StencilEntry(var offset : MultiIndex, var weight : Expression) {}
                    case class Stencil(var entries : ListBuffer[StencilEntry] = new ListBuffer) extends Node {}
                    case class StencilConvolution(var stencil : Stencil, var field : Field) extends Expression with Expandable {
                      override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n";

                      def expand(collector : StackCollector) : Expression = {
                        stencil.entries.map(e =>
                          e.weight * (new FieldAccess("curFragment.", field, 0, new MultiIndex(DefaultLoopMultiIndex(), e.offset, _ + _))). /*FIXME*/ expand(new StackCollector))
                          .toArray[Expression].reduceLeft(_ + _)
                      }
                    }

                    // temp stencil
                    var stencil = new Stencil
                    for (i <- 0 until e1.length)
                      stencil.entries += StencilEntry(new MultiIndex(IdxKnowledge.StencilToidx(Knowledge.dimensionality, e1.length)(i).toArray), e1.entries(i)) // s"$id1[0].entries[$i]")

                    // find field
                    val field : Field = fieldCollection.getFieldByIdentifier("Solution", levstr.cpp.toInt).get

                    // temp conv
                    var conv = StencilConvolution(stencil, field)

                    return conv.expand(new StackCollector).cpp
                  }
                }
                case _ =>
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
      return new BinaryExpression(BinaryOperators.Division, new StringLiteral(s"${DomainKnowledge.datatype_L2.getOrElse("double")}(1)"), arglist(0).transform(scopeparas, modifier, "argument"))
    }

    if (fname.equals("diag")) {

      if (DomainKnowledge.use_gpu) {
        var curStencil = TreeManager.tree.Stencils(0)

        return new StringLiteral(s"${curStencil.entries(0)}") // DataClasses.generateStencilConvolutioncuda(1,args(0).toString_cpp,"", "")
      } else {
        // FIXME: this is only a quick hack necessary because the new stencil structure is not available here 
        var curStencil = TreeManager.tree.Stencils(0)
        return new StringLiteral(s"${curStencil.entries(0)}")

        //return new MemberFunctionCallExpression(args(0).cpp, fname, new ListBuffer[Expression])
      }
    }
    if (fname.equals("random"))
      return new StringLiteral("(rand()/static_cast<double>(RAND_MAX))*" + args(0).cpp) // TODO
    if (fname.equals("fasterReduce") && DomainKnowledge.use_gpu)
      return new StringLiteral("fasterReduce (Res[lev].begin(), solution[lev].x1_*solution[lev].x2_, f[lev].begin())") // TODO

    return new FunctionCallExpression(fname, args)

  }
}

case class AbstractLiteral(text : String) extends AbstractExpression {
  override def value(context : Context) = text
  override def toString = text
  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    return new StringLiteral(text)
  }
}

case class AbstractStringLiteral(text : String) extends AbstractExpression {
  override def value(context : Context) = text
  override def toString = text
  override def transform(scopeparas : ListBuffer[ParameterInfo], modifier : Option[String], scopetype : String) : Expression = {
    return new StringLiteral("\"" + text + "\"")
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
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id)) {
        // COMM_HACK
        return id match {
          case "solution" => (
            if ("statement" == scopetype || "expression" == scopetype) {
              //s"curFragment.solData[0][" ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp()
              val fieldCollection = StateManager.findFirst[FieldCollection]().get
              val field : Field = fieldCollection.getFieldByIdentifier("Solution", lev.transform(scopeparas, modifier, scopetype).cpp.toInt).get
              new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) /*TODO*/ .expand(new StackCollector)
            } else
              s"*curFragment.solData[0][FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]")
          case "Res" => (
            if ("statement" == scopetype || "expression" == scopetype) {
              //s"curFragment.resData[0][" ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp()
              val fieldCollection = StateManager.findFirst[FieldCollection]().get
              val field : Field = fieldCollection.getFieldByIdentifier("Residual", lev.transform(scopeparas, modifier, scopetype).cpp.toInt).get
              new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) /*TODO*/ .expand(new StackCollector)
            } else
              s"*curFragment.resData[0][FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]")
          case "f" => (
            if ("statement" == scopetype || "expression" == scopetype) {
              //s"curFragment.rhsData[0][" ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]->getDataRef" ~ DomainKnowledge.rule_idxArray_cpp()
              val fieldCollection = StateManager.findFirst[FieldCollection]().get
              val field : Field = fieldCollection.getFieldByIdentifier("RHS", lev.transform(scopeparas, modifier, scopetype).cpp.toInt).get
              new FieldAccess("curFragment.", field, 0, DefaultLoopMultiIndex()) /*TODO*/ .expand(new StackCollector)
            } else
              s"*curFragment.rhsData[0][FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ s"]")
          case _ => (
            if ("statement" == scopetype || "expression" == scopetype)
              id ~ "[FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ "]" ~ DomainKnowledge.rule_idxArray_cpp()
            else
              id ~ "[FIXME " ~ lev.transform(scopeparas, modifier, scopetype) ~ "]")
        }
      }

    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id)) {
        if ("statement" == scopetype || "expression" == scopetype)
          return id ~ "[0]" ~ "(Matrix (i0,i1))"
        else
          return id ~ "[0]"
      }

    for (e <- scopeparas) {
      if (e.name.equals(id))
        if (e.dtype.startsWith(TreeManager.tree.ExternalClasses.get("Array").get.name) || e.dtype.startsWith("Container")) {
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
    for (e <- TreeManager.tree.Fields)
      if (e.name.equals(id))
        return ListBuffer(id)
    for (e <- TreeManager.tree.Stencils)
      if (e.name.equals(id))
        return ListBuffer(id)
    return ListBuffer()
  }
}
