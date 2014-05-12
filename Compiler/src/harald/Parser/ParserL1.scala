package harald.Parser

import harald.dsl.DomainKnowledge
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable.ListBuffer
import harald.Continuous._
import scala.util.matching.Regex

class ParserL1 extends ExaParser {

  // DSL level 1 parser
  def exastencilsL1 : Parser[Any] = domain.? <~ newline.? ~> function.* <~ newline.? ~> unknown.* <~ newline.? ~> operator.* <~ newline.? ~> pdebc.? <~ newline.? ~> system.? <~ newline.? ~> accuracy.? <~ newline.? ~> generate.?
  //	def exastencilsL1: Parser[Any] = domain.? <~ newline.? ~> function.* <~ newline.? ~> unknown.* <~ newline.? ~> operator.* <~ newline.? ~> pde.? <~ newline.? ~> pdebc.? <~ newline.? ~> system.? <~ newline.? ~> accuracy.? <~ newline.? ~> generate.?

  def domain : Parser[Any] = ("Domain" ~> ident) ~ ("=" ~> domain_type) ^^ { case n ~ t => DomainKnowledge.domain_L1 = Some(Tuple2(n, t)) }
  def function : Parser[Any] = ("Function" ~> ident) ~ ("=" ~> expr) ^^ { case fname ~ fexpr => DomainKnowledge.function_L1 += new Tuple2(fname, evaluate(fexpr)) }
  def unknown : Parser[Any] = ("Unknown" ~> ident) ~ ("=" ~> expr) ^^ { case n ~ fexpr => DomainKnowledge.unknown_L1 += new Tuple2(n, evaluate(fexpr)) }
  def operator : Parser[Any] = ("Operator" ~> ident) ~ ("=" ~> ident.+) ^^ { case n ~ op => DomainKnowledge.operator_L1 += new Tuple2(n, op) }

  /*	def pde: Parser[Any] = ("PDE" ~> ident) ~ ("{" ~> ident) ~ ("(" ~> ident) ~ (")" ~ "=" ~> ident <~ "}") ^^ { case n1~n2~n3~n4 => DomainKnowledge.pde_L1 = Some(n1 + " " + n2 + " " + n3 + " " + n4)} | 
	                       "FUNCTIONAL" ~ ident ~ "{" ~ ident ~ "+" ~ ident ~ "}" ^^ { case a~name~b~dt~c~reg~d => DomainKnowledge.functional_L1 = Some( name); 
	                                                                                                               DomainKnowledge.dataterm_L1 = Some(dt);
	                                                                                                               DomainKnowledge.regularizer_L1 = Some(reg);}
*/
  def pdebc : Parser[Any] = ("PDEBC" ~> ident) ~ ("{" ~> ident) ~ ("=" ~> ident <~ "}") ^^ { case n ~ rhs ~ lhs => DomainKnowledge.pdebc_L1 = Some(Tuple2(rhs, lhs)) }
  def system : Parser[Any] = "System" ~ "=" ~ expr ^^ { case a ~ b ~ e => DomainKnowledge.system_L1 = Some(evaluate(e)) }
  def accuracy : Parser[Any] = "Accuracy" ~ "=" ~ expr ^^ { case a ~ b ~ e => DomainKnowledge.accuracy_L1 = Some(evaluate(e)) }
  def generate : Parser[Any] = "Generate" ~ "=" ~ expr ^^ { case a ~ b ~ e => DomainKnowledge.generate_L1 = Some(evaluate(e)) }

  def domain_type : Parser[String] = "UnitSquare" | "Square" | "UnitCube"
  //	def operator_type: Parser[String] = "Laplacian"

}

class ParserL1a extends StandardTokenParsers {
  lexical.reserved += ("EQ", "FT", "OT", "R", "C", "in", "T", "Z", "CO", "transp", "OP", "integral", "FU", "Domain", "Accuracy", "Generate", "UnitSquare", "UnitCube", "Function", "Unknown", "Operator", "x")
  //  lexical.reserved += ("Domain", "Function", "Unknown", "Operator", "PDE", "PDEBC") 
  lexical.delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "+=", "|", "[", "]", "<", ">", ".", "->", "^", ",")

  def newline : Parser[Any] = "\n" | "\r\n"

  def parse(input : String) = {

    val tokens = new lexical.Scanner(input)
    //if (DomainKnowledge.debugmode)
    println(tokens.toString)
    val result = phrase(exastencilsL1a)(tokens)
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

  def exastencilsL1a = domain.* <~ newline.? ~> functiontypes.* <~ newline.? ~> operatortypes.* <~ newline.? ~> constants.* <~ newline.? ~> opdefines.* <~ newline.? ~> funcdefines.* <~ newline.? ~> equations.* <~ newline.? ~> accuracy.? <~ newline.? ~> generate.?
  // def exastencilsL1a = domain.? <~ newline.? ~> function.* <~ newline.? ~> unknown.* <~ newline.? ~> operator.* <~ newline.? ~> functiontypes.* <~ newline.? ~> operatortypes.* <~ newline.? ~> constants.* <~ newline.? ~> opdefines.* <~ newline.? ~> funcdefines.* <~ newline.? ~> equations.* <~ newline.? ~> accuracy.? <~ newline.? ~> generate.?
  //	def exastencilsL4 =  definitions.* <~ newline.? ~> multigridfunctions.* <~ newline.? ~> dataclasses.*

  //  	def domain: Parser[Any] = ("Domain" ~> ident) ~ ("=" ~> domain_type) ^^ { case n~t => DomainKnowledge.domain_L1 = Some(Tuple2(n,t))}
  def domain : Parser[Any] = ("Domain" ~> ident) ~ ("=" ~> subdomain) ^^ { case n ~ t => DomainKnowledge.domain_L1 = Some(Tuple2(n, "")); DomainKnowledge.cont_domains += new ContDomain(n, List(t)) }
  def subdomain : Parser[ContSubDomain] = interval ~ "x" ~ interval ~ "x" ~ interval ^^ { case n1 ~ a ~ n2 ~ b ~ n3 => ContSubDomain(List(n1, n2, n3)) } |
    interval ~ "x" ~ interval ^^ { case n1 ~ a ~ n2 => ContSubDomain(List(n1, n2)) } |
    interval ^^ { case n => ContSubDomain(List(n)) }

  def interval : Parser[ContInterval] = ("[" ~> numericLit) ~ ("," ~> numericLit) ~ "]" ^^ { case b ~ e ~ h => ContInterval(b.toInt, e.toInt, "") }

  /*
    def function: Parser[Any] = ("Function" ~> ident) ^^ { case fname => DomainKnowledge.function_L1 = ListBuffer(new Tuple2(fname,0))}
	def unknown: Parser[Any] = ("Unknown" ~> ident) ^^ { case n => DomainKnowledge.unknown_L1  = ListBuffer(new Tuple2(n, 0))}
	def operator: Parser[Any] = ("Operator" ~> ident) ~ ("=" ~> ident+)^^ { case n~op => DomainKnowledge.operator_L1 =  ListBuffer(new Tuple2(n,op))}

	def pde: Parser[Any] = ("PDE" ~> ident) ~ ("{" ~> ident) ~ ("(" ~> ident) ~ (")" ~ "=" ~> ident <~ "}") ^^ { case n1~n2~n3~n4 => DomainKnowledge.pde_L1 = Some(n1 + " " + n2 + " " + n3 + " " + n4)} | 
	                       "FUNCTIONAL" ~ ident ~ "{" ~ ident ~ "+" ~ ident ~ "}" ^^ { case a~name~b~dt~c~reg~d => DomainKnowledge.functional_L1 = Some( name); 
	                                                                                                               DomainKnowledge.dataterm_L1 = Some(dt);
	                                                                                                               DomainKnowledge.regularizer_L1 = Some(reg);}
	                        
	def pdebc: Parser[Any] = ("PDEBC" ~> ident) ~ ("{" ~> ident) ~ ("=" ~> ident <~ "}") ^^ { case n~rhs~lhs => DomainKnowledge.pdebc_L1 = Some(Tuple2(rhs,lhs))}
	def system: Parser[Any] = "System" ~ "=" ~ expr ^^ { case a~b~e => DomainKnowledge.system_L1 = Some(evaluate(e))}
		*/
  def accuracy : Parser[Any] = "Accuracy" ~ "=" ~ numericLit ^^ { case a ~ b ~ e => DomainKnowledge.accuracy_L1 = Some(e.toInt) }
  def generate : Parser[Any] = "Generate" ~ "=" ~ numericLit ^^ { case a ~ b ~ e => DomainKnowledge.generate_L1 = Some(e.toInt) }

  def set_type : Parser[String] = ident ~ "x" ~ "T" ^^ { case n ~ a ~ b => s"${ident} x T" } | ident | "R" | "C" | "Z"
  def domain_type : Parser[String] = "UnitSquare" | "UnitCube"

  //def definitions : Parser[Definition] = { "decl" ~ ident ~ ident ~ "=" ~ expr } ^^ {case a ~ typ ~ name ~ b ~ e => exaDefinitions.append(new Definition(name, typ, e)); Definition(name, typ, e) }
  def functiontypes : Parser[Any] = "FT" ~ ident ~ ":" ~ set_type ~ "^" ~ numericLit ~ "->" ~ set_type ~ "^" ~ numericLit ^^ {
    case a ~ name ~ b ~ dt1 ~ b1 ~ vec1 ~ c ~ dt ~ d ~ vec2 =>
      var strt = Tuple2(dt1, "")
      var dim = 2
      for (d <- DomainKnowledge.cont_domains)
        if (dt1.startsWith(d.name)) {
          dim = d.subdoms(0).intervals.size
          if (dt1.equals(d.name))
            strt = Tuple2(d.name, "")
          else
            strt = Tuple2(d.name, "T")
        }

      DomainKnowledge.cont_functions += new ContFunction(name, new ContFunctionType(strt, dim, vec1.toInt, dt, 1, vec2.toInt));
      ContFunction(name, new ContFunctionType(strt, DomainKnowledge.rule_dim, vec1.toInt, dt, 1, vec2.toInt))

  }

  def operatortypes : Parser[ContOperator] = "OT" ~ ident ~ ":" ~ "(" ~ set_type ~ "^" ~ numericLit ~ "->" ~ set_type ~ "^" ~ numericLit ~ ")" ~ "->" ~ "(" ~ set_type ~ "^" ~ numericLit ~ "->" ~ set_type ~ "^" ~ numericLit ~ ")" ^^ {
    case a ~ name ~ b ~ b1 ~ st1 ~ b2 ~ vec1 ~ c ~ dt ~ d ~ vec2 ~ e ~ e1 ~ e2 ~ st2 ~ b3 ~ vec3 ~ f ~ dt2 ~ g ~ vec4 ~ h =>
      var strt1 = Tuple2(st1, "")
      var dim1 = 2
      var strt2 = Tuple2(st2, "")
      var dim2 = 2
      for (d <- DomainKnowledge.cont_domains) {
        if (st1.startsWith(d.name)) {
          dim1 = d.subdoms(0).intervals.size
          if (st1.equals(d.name))
            strt1 = Tuple2(d.name, "")
          else
            strt1 = Tuple2(d.name, "T")
        }
        if (st2.startsWith(d.name)) {
          dim2 = d.subdoms(0).intervals.size
          if (st2.equals(d.name))
            strt2 = Tuple2(d.name, "")
          else
            strt2 = Tuple2(d.name, "T")
        }
      }

      DomainKnowledge.cont_operators += new ContOperator(name,
        new ContOperatorType(new ContFunctionType(strt1, dim1, vec1.toInt, dt, 1, vec2.toInt),
          new ContFunctionType(strt2, dim2, vec3.toInt, dt2, 1, vec4.toInt)));
      ContOperator(name,
        new ContOperatorType(new ContFunctionType(strt1, dim1, vec1.toInt, dt, 1, vec2.toInt),
          new ContFunctionType(strt2, dim2, vec3.toInt, dt2, 1, vec4.toInt)))
  }

  def constants : Parser[ContConstant] = "CO" ~ ident ~ ":" ~ set_type ~ "^" ~ numericLit ~ "=" ~ numericLit ^^ {
    case a ~ name ~ b ~ dt1 ~ b1 ~ vec1 ~ c ~ value =>
      DomainKnowledge.cont_consts += new ContConstant(name, new ContConstType(dt1, vec1.toInt), value.toDouble);
      ContConstant(name, new ContConstType(dt1, vec1.toInt), value.toDouble)
  }

  def equations : Parser[ContEquation] = "EQ" ~ ident ~ ":" ~ contexpr ~ "=" ~ contexpr ~ "in" ~ ident ^^ {
    case a ~ name ~ b ~ exp1 ~ c ~ exp2 ~ d ~ dom =>
      DomainKnowledge.cont_equations += new ContEquation(exp1, exp2, dom, name);
      ContEquation(exp1, exp2, dom, name)
  }

  def opdefines : Parser[ContEquation] = "OP" ~ ident ~ "<" ~ numericLit ~ ">" ~ "=" ~ contexpr ^^ {
    case a ~ nam ~ b ~ ent ~ c ~ d ~ exp1 =>
      var o1 : ContOperator = DomainKnowledge.cont_operators(0)

      for (o <- DomainKnowledge.cont_operators)
        if (o.name.equals(nam))
          o1 = o
      DomainKnowledge.cont_equations += new ContEquation(ContVariable(o1, ent.toInt), exp1, "", "OP");
      ContEquation(ContVariable(o1), exp1, "", "OP")
  }

  def funcdefines : Parser[ContEquation] = "FU" ~ ident ~ "<" ~ numericLit ~ ">" ~ "=" ~ contexpr ^^ {
    case a ~ nam ~ b ~ ent ~ c ~ d ~ exp1 =>
      var o1 : ContFunction = DomainKnowledge.cont_functions(0)

      for (o <- DomainKnowledge.cont_functions)
        if (o.name.equals(nam))
          o1 = o
      DomainKnowledge.cont_equations += new ContEquation(ContVariable(o1, ent.toInt), exp1, "", "FU");
      ContEquation(ContVariable(o1), exp1, "", "FU")
  }

  def contexpr : Parser[ContExpression] =
    (term ~ "+" ~ term) ^^ { case lhs ~ op ~ rhs => ContBinOp("+", lhs, rhs) } |
      (term ~ "-" ~ term) ^^ { case lhs ~ op ~ rhs => ContBinOp("-", lhs, rhs) } |
      ("-" ~ term) ^^ { case op ~ rhs => ContUnaryOp("-", rhs) } |
      "transp" ~ term ^^ { case a ~ e => ContTranspose(e) } |
      "integral" ~ ident ~ term ^^ { case a ~ om ~ e => ContIntegral(om, e) } |
      term

  def term : Parser[ContExpression] =
    (factor ~ "*" ~ factor) ^^ { case lhs ~ op ~ rhs => ContBinOp("*", lhs, rhs) } |
      (factor ~ "/" ~ factor) ^^ { case lhs ~ op ~ rhs => ContBinOp("/", lhs, rhs) } |
      factor

  def factor : Parser[ContExpression] = ("(" ~> contexpr <~ ")"
    | stringLit ^^ { case s => ContLiteral(s) }
    | ident ~ "[" ~ ident ~ "]" ^^ {
      case id ~ a ~ obj ~ b =>
        var found = false
        var o1 : ContOperator = DomainKnowledge.cont_operators(0)
        var f1 : ContFunction = DomainKnowledge.cont_functions(0)

        for (o <- DomainKnowledge.cont_operators)
          if (o.name.equals(id)) {
            for (f <- DomainKnowledge.cont_functions)
              if (f.name.equals(obj)) {
                found = true
                o1 = o
                f1 = f

              }
          }
        ContOpapply(o1, f1)

    }
    | ident ~ "{" ~ contexpr ~ "}" ^^ {
      case id ~ a ~ obj ~ b =>
        ContFuncapply(id, obj)
    }
    | ident ~ "<" ~ numericLit ~ ">" ^^ {
      case id ~ a ~ obj ~ b =>
        var o1 : ContObject = new ContObject()
        for (o <- DomainKnowledge.cont_operators)
          if (o.name.equals(id))
            o1 = o
        for (f <- DomainKnowledge.cont_functions)
          if (f.name.equals(id))
            o1 = f

        ContVariable(o1, obj.toInt)
    }
    | numericLit ^^ { case s => ContLiteral(s) }
    | "(" ~ numericLit ~ "." ~ numericLit ~ ")" ^^ { case a ~ n1 ~ b ~ n2 ~ c => var s : String = n1.toString + "." + n2.toString; ContLiteral(s) }
    | ident ~ "<" ~ numericLit ~ ">" ^^ {
      case id ~ a ~ ent ~ b =>
        var o1 : ContObject = new ContObject()
        for (o <- DomainKnowledge.cont_operators)
          if (o.name.equals(id))
            o1 = o
        for (f <- DomainKnowledge.cont_functions)
          if (f.name.equals(id))
            o1 = f

        ContVariable(o1, ent.toInt)
    }
    | ident ^^ {
      case id =>
        var o1 : ContObject = new ContObject()
        for (o <- DomainKnowledge.cont_operators)
          if (o.name.equals(id))
            o1 = o
        for (f <- DomainKnowledge.cont_functions)
          if (f.name.equals(id))
            o1 = f
        for (c <- DomainKnowledge.cont_consts)
          if (c.name.equals(id))
            o1 = c

        ContVariable(o1)
    })
}

