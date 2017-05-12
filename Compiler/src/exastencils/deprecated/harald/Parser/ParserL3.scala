package exastencils.deprecated.harald.Parser

import exastencils.deprecated.harald.dsl.DomainKnowledge
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable.ListBuffer
import exastencils.deprecated.harald.Discrete._
import exastencils.deprecated.harald.Abstract._
import exastencils.deprecated.harald.ast.TreeL2

class ParserL3 extends ExaParser {

  class ExaOption(val name : String, val value : Expr) {
  }
  class ExaOptionfloat(val name : String, val value : String) {
  }
  class ExaStringOption(val name : String, val value : String) {
  }

  def exastencilsL3 : Parser[Any] = multigridcomponents.? <~ newline.? ~> multigridparameter.? <~ newline.? ~> multigridparameterfloat.?

  def multigridparameter : Parser[Any] = "mgparameter" ~ "{" ~> { option.* ^^ { case opts => for (o <- opts) { set(DomainKnowledge, o.name, evaluate(o.value)) }; opts } } <~ "}"
  def multigridparameterfloat : Parser[Any] = "mgparameterfloat" ~ "{" ~> { optionfloat.* ^^ { case opts => for (o <- opts) { set(DomainKnowledge, o.name, o.value.toDouble) }; opts } } <~ "}"
  def multigridcomponents : Parser[Any] = "mgcomponents" ~ "{" ~> { stringoption.* ^^ { case opts => for (o <- opts) { set(DomainKnowledge, o.name, o.value) }; opts } } <~ "}"

  def option : Parser[ExaOption] = { ident ~ "=" ~ expr <~ newline.? } ^^ { case k ~ eq ~ v => ; new ExaOption(k, v) }
  def optionfloat : Parser[ExaOptionfloat] = { ident ~ "=" ~ floatingPointNumber <~ newline.? } ^^ { case k ~ eq ~ v => ; new ExaOptionfloat(k, v) }
  def stringoption : Parser[ExaStringOption] = { ident ~ "=" ~ (smoother_type | interpolation_type | restriction_type | coarse_type) <~ newline.? } ^^ { case k ~ eq ~ v => ; new ExaStringOption(k, v) }

  def field_datatype : Parser[String] = "real" | "natural"
  def field_location : Parser[String] = "nodes" | "cells"

  def smoother_type : Parser[String] = ident | "GS" | "Jac"
  def interpolation_type : Parser[String] = ident | "Linear"
  def restriction_type : Parser[String] = ident | "FW"
  def coarse_type : Parser[String] = ident | "CG" | "Direct" | "AMG"

}

class ParserL3a(tree : TreeL2) extends StandardTokenParsers {
  lexical.reserved += ("VEC", "MATRIX", "SET", "inverse", "ITERATION", "diag", "SIZE", "transp", "lc", "l", "lf", "order", "coarsestlevel", "if", "then", "RESTRMATRIX", "of")
  lexical.delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "+=", "|", "[", "]", "<", ">", ".", "->", "^", ",", "~", "==", "!=")

  def newline : Parser[Any] = "\n" | "\r\n"
  def levelident : Parser[String] = "l" | "lc" | "lf" | "coarsestlevel"

  def parse(input : String) = {

    val tokens = new lexical.Scanner(input)
    //if (DomainKnowledge.debugmode)
    println(tokens.toString)
    val result = phrase(exastencilsL3a)(tokens)
    //if (DomainKnowledge.debugmode)
    println(result.toString)
    /*
    result match {
      case Success(tree, _) => new Interpreter(tree).run()

      case e: NoSuccess => {
        println(e)
      }
    }

  if (DomainKnowledge.debugmode) {
    println(result.toString)
    for (e <- tree.exaClasses)
      println(e)
  }
   */
    println(result.toString)
  }

  class ExaStringOption(val name : String, val value : String) {
  }

  def exastencilsL3a = vectors.* <~ newline.? ~> matrices.* <~ newline.? ~> restr.* <~ newline.? ~> sets.* <~ newline.? ~> iterations.*
  //  def exastencilsL3a = domain.* <~ newline.? ~> functiontypes.* <~ newline.? ~> operatortypes.* <~ newline.? ~> constants.* <~ newline.? ~> opdefines.* <~ newline.? ~> funcdefines.* <~ newline.? ~> equations.* <~ newline.? ~> accuracy.? <~ newline.? ~> generate.?

  def vectors : Parser[Any] = ("VEC" ~> ident) ~ ("SIZE" ~> ident) ^^ {
    case id ~ id2 =>
      for (f <- DomainKnowledge.discr_functions)
        if (f.name.equals(id2))
          DomainKnowledge.discr_functions += new DiscrFunction(id, f.functype, f.evalpoints)
  }
  def matrices : Parser[Any] = ("MATRIX" ~> ident) ~ ("SIZE" ~> ident) ^^ {
    case id ~ id2 =>
      for (f <- DomainKnowledge.discr_operators)
        if (f.name.equals(id2))
          DomainKnowledge.discr_operators += new DiscrOperator(id, f.optype)
  }
  def restr : Parser[Any] = ("RESTRMATRIX" ~> ident) ~ "of" ~ ident ~ "=" ~ numericLit ^^ {
    case id ~ a ~ id2 ~ b ~ id3 =>
      for (f <- DomainKnowledge.discr_functions)
        if (f.name.equals(id2))
          tree.exaOperators += new AbstractStencil(id, f.functype.domaindimension, f.functype.codomain, "1", "1", "TOCOARSE", id3, f.evalpoints)
  }
  def sets : Parser[Any] = ("SET" ~> ident) ~ ("=" ~> subsets) ^^ {
    case id ~ id2 =>
      DomainKnowledge.discr_sets += new DiscrSet(id, id2)
  }
  def iterations : Parser[Any] = ("ITERATION" ~> ident) ~ (":" ~> discreq) ~ ("order" ~> ident) ^^ {
    case id ~ id2 ~ ord =>
      DomainKnowledge.discr_iterations += new DiscrIteration(id, id2, ord, List())
  } |
    ("ITERATION" ~> ident) ~ ":" ~ discrexpr ^^ {
      case id ~ a ~ id3 =>
        DomainKnowledge.discr_iterations += new DiscrIteration(id, id3, "", List())
    } |
    ("ITERATION" ~> ident) ~ ":" ~ discrcase.* ^^ {
      case id ~ a ~ id3 =>
        DomainKnowledge.discr_iterations += new DiscrIteration(id, new DiscrExpression(), "", id3)
    }

  def discrcase : Parser[Tuple2[DiscrExpression, DiscrExpression]] = "if" ~ discrexpr ~ "then" ~ discrexpr ^^ {
    case a ~ e1 ~ b ~ e2 =>
      new Tuple2(e1, e2)
  }

  def discreq : Parser[DiscrExpression] = discrexpr ~ "=" ~ discrexpr ^^ {
    case e1 ~ a ~ e2 =>
      new DiscrBinOp("=", e1, e2)
  }

  def subsets : Parser[List[Tuple2[DiscrPoint, DiscrPoint]]] = item.* ^^ { case i => i }

  def item : Parser[Tuple2[DiscrPoint, DiscrPoint]] = interval ~ interval ^^ { case i1 ~ i2 => Tuple2[DiscrPoint, DiscrPoint](i1, i2) }
  def interval : Parser[DiscrPoint] = ("[" ~> numericLit) ~ ("," ~> numericLit) ~ ("," ~> numericLit) ~ "]" ^^ { case b ~ e ~ f ~ h => DiscrPoint(List[Int](b.toInt, e.toInt, f.toInt)) } |
    ("[" ~> numericLit) ~ ("," ~> numericLit) ~ "]" ^^ { case b ~ e ~ h => DiscrPoint(List[Int](b.toInt, e.toInt)) }

  def discrexpr : Parser[DiscrExpression] =
    (term ~ "+" ~ term) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("+", lhs, rhs) } |
      (term ~ "^" ~ term) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("^", lhs, rhs) } |
      (levelident ~ "==" ~ numericLit) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("==", DiscrLiteral(lhs, ""), DiscrLiteral(rhs, "")) } |
      (levelident ~ "!=" ~ numericLit) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("!=", DiscrLiteral(lhs, ""), DiscrLiteral(rhs, "")) } |
      (term ~ "~" ~ term) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("~", lhs, rhs) } |
      (term ~ "-" ~ term) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("-", lhs, rhs) } |
      ("-" ~ term) ^^ { case op ~ rhs => DiscrUnaryOp("-", rhs) } |
      "transp" ~ term ^^ { case a ~ e => DiscrTranspose(e) } |
      "diag" ~ term ^^ { case a ~ e => DiscrDiag(e) } |
      "inverse" ~ term ^^ { case a ~ e => DiscrInverse(e) } |
      term

  def term : Parser[DiscrExpression] =
    (factor ~ "*" ~ factor) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("*", lhs, rhs) } |
      (factor ~ "/" ~ factor) ^^ { case lhs ~ op ~ rhs => DiscrBinOp("/", lhs, rhs) } |
      factor

  def factor : Parser[DiscrExpression] = ("(" ~> discrexpr <~ ")"
    | ident ~ "[" ~ levelident ~ "]" ^^ {
      case id ~ a ~ obj ~ b => DiscrLiteral(id, obj)
    }
    | "(" ~ numericLit ~ "." ~ numericLit ~ ")" ^^ { case a ~ n1 ~ b ~ n2 ~ c => DiscrLiteral(n1.toString + "." + n2.toString, "") }
    | ident ^^ {
      case id => DiscrLiteral(id, "")
    }
    | numericLit ^^ { case n => DiscrLiteral(n, "") })

}

