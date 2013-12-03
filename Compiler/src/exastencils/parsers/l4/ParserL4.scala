package exastencils.parsers.l4

import exastencils.parsers._
import exastencils.datastructures.l4._

class ParserL4 extends ExaParser {

  //lexical.reserved += ("Int")

  def parse(input : String) {
    val tokens = new lexical.Scanner(input)
    val result = phrase(program)(tokens)
    println(result.toString)
    //    result match {
    //      case Success(tree, _) => new Interpreter(tree).run()
    //
    //      case e: NoSuccess => {
    //        println(e)
    //      }
    //    }
    //
    //    println(result.toString)
    //    for (e <- tree.exaClasses)
    //      println(e)
  }

  lazy val program = statement.* ^^ { case x => println(x); x }

  lazy val statement = function |
    ident ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(Identifier(id), exp) }

  lazy val function : Parser[Any] = "def" ~> ident ~ "(" ~ functionArgumentList.? ~ ")" ~ ":" ~ datatype ~ "{" ~ statement.* ~ "}" // FIXME statements
  lazy val functionArgumentList = (functionArgument ~ ("," | newline)).* ~ functionArgument
  lazy val functionArgument = ((ident <~ ":") ~ datatype) ^^ { case id ~ t => println(f"found argument $id of type $t"); }
  lazy val functionCall = ident ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCall(id, args.getOrElse(List[Expression]())) }
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => ex :: exps } // = new list(exps, ex)

  lazy val expression : PackratParser[Expression] =
    (expression ~ ("+" | "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) } |
      term

  lazy val term : PackratParser[Expression] =
    (term ~ ("*" | "/" | "**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) } |
      factor

  lazy val factor : Parser[Expression] =
    "(" ~> expression <~ ")" |
      stringLit ^^ { case s => StringLiteral(s) } |
      ("+" | "-").? ~ numericLit ^^ { case s ~ n => if (s == Some("-")) NumericLiteral(-n.toDouble) else NumericLiteral(n.toDouble) } | // FIXME check and create integer/double
      booleanLit ^^ { case s => BooleanLiteral(s.toBoolean) } |
      functionCall |
      ident ^^ { case id => Identifier(id) }
      
  //
  //  class ExaStringOption(val name: String, val value: String) {
  //  }
  //
  //  def exastencilsL4 = multigridfunctions.* <~ newline.? ~> dataclasses.*
  //  //	def exastencilsL4 =  definitions.* <~ newline.? ~> multigridfunctions.* <~ newline.? ~> dataclasses.*
  //
  //  //def definitions : Parser[Definition] = { "decl" ~ ident ~ ident ~ "=" ~ expr } ^^ {case a ~ typ ~ name ~ b ~ e => exaDefinitions.append(new Definition(name, typ, e)); Definition(name, typ, e) }
  //
  //  def dataclasses: Parser[AbstractClass] = ("class" ~> ident.?) ~ (ident <~ "{") ~ (paramoption+) ~ (("public" ~> (multigridfunctions+)) <~ "}") ^^ {
  //    case templ ~ cname ~ memb ~ memf =>
  //      tree.exaClasses.append(new AbstractClass(cname, templ, memb, memf)); AbstractClass(cname, templ, memb, memf)
  //  }
  //
  //  def data_type: Parser[String] = "Double" | "Int" | "Array" | "MyArray" | "Complex" | ident
  //  def return_datatype: Parser[String] = "real" | "natural" | "Unit" | "Double" | "Complex"
  //  def operator_type: Parser[String] = "+" | "-" | "*" | "/"
  //  def modifier_type: Parser[String] = "ToFine" | "ToCoarse"
  //
  //  def paramoption: Parser[Param] = { ident ~ ":" ~ data_type } ^^ { case k ~ ":" ~ v => Param(k, v) }
  //
  //  def multigridfunctions: Parser[AbstractFunction] = ("def" ~> ident) ~ ident ~ ("(" ~> (paramoption*) <~ ")") ~ (":" ~> return_datatype) ~ ("{" ~> (stmt+) <~ "}") ^^ {
  //    case loc ~ name ~ args ~ ret ~ body =>
  //      tree.exaFunctions.append(new AbstractFunction(name, loc, ret, args, body)); AbstractFunction(name, loc, ret, args, body)
  //  }
  //  
  //  def stmt: Parser[AbstractStatement] = ("loop" ~ ident ~ "level" ~ ident ~ "order" ~ ident ~ "block" ~ numericLit ~ numericLit ~ (stmt+) ~ "next" ^^ {
  //    case a ~ where ~ b ~ clev ~ c ~ ord ~ d ~ bls ~ blst ~ stmts ~ e => AbstractLoop(where, clev, ord, bls, blst, stmts)
  //  }
  //    | ident ~ "=" ~ expr ~ modifier.? ^^ { case id ~ _ ~ e ~ m => AbstractLet(id, e, m) }
  //    | ident ~ "+=" ~ expr ~ modifier.? ^^ { case id ~ _ ~ e ~ m => AbstractPLet(id, e, m) }
  //    | ident ~ "(" ~ expr.* ~ ")" ^^ { case id ~ a ~ e ~ b => AbstractPCall(id, e) } 
  //    | "decl" ~ paramoption ~ "=" ~ expr ^^ {case a ~ para ~ b ~ e => AbstractDefinition(para, e) }
  //    | "return" ~ expr ^^ { case a ~ e => AbstractReturn(e) }
  //    | "repeat" ~ ident ~ expr ~ (stmt+) ~ "next" ^^ { case a ~ id ~ e ~ s ~ f => AbstractRepeat(e, s, id) }
  //    | "Reduction" ~ stmt ^^ { case a ~ s => AbstractReduction(s) }
  //    | "Communicate" ~ ident ~ ident ^^ { case a ~ f ~ loc => AbstractCommunication(f,loc) }
  //    | "if" ~ expr ~ "{" ~ (stmt+) ~ "}" ~ "else" ~ "{" ~ (stmt+) ~ "}" ^^ {
  //      case a ~ cond ~ b ~ ifstmts ~ c ~ d ~ e ~ elsestmts ~ f => AbstractIfElse(cond, ifstmts, elsestmts)
  //    })
  //
  //  def modifier = "|" ~ modifier_type ^^ { case a ~ m => m }
  //  def functioncall = ident ~ "(" ~ expr.* ~ ")" ^^ { case id ~ a ~ e ~ b => AbstractFCall(id, e) }
  //
  //  def expr: Parser[AbstractExpression] =
  //    (term ~ "+" ~ term) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp("+", lhs, rhs) } |
  //      (term ~ "-" ~ term) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp("-", lhs, rhs) } |
  //      functioncall |
  //      term
  //
  //  def term: Parser[AbstractExpression] =
  //    (factor ~ "*" ~ factor) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp("*", lhs, rhs) } |
  //      (factor ~ "/" ~ factor) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp("/", lhs, rhs) } |
  //      factor
  //
  //  def factor: Parser[AbstractExpression] = ("(" ~> expr <~ ")"
  //    | stringLit ^^ { case s => AbstractStringLiteral(s) }
  //    | ident ~ "[" ~ factor ~ "]" ^^ { case id ~ a ~ t ~ b => AbstractVariable(id, t) }
  //    | numericLit ^^ { case s => AbstractLiteral(s) }
  //    | ident ^^ { case id => AbstractVariable(id, new AbstractLiteral("")) })

}
