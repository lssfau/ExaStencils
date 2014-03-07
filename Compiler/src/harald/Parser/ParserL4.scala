package harald.Parser

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import harald.dsl._
import harald.Abstract._
import harald.ast.TreeL2

class ParserL4(tree : TreeL2) extends StandardTokenParsers {
  override val lexical = new exastencils.parsers.ExaLexer()
  
  lexical.reserved += ("let", "loop", "next", "level", "def", "Int", "Array", "Unit", "Double", "return", "ToFine", "ToCoarse", "if", "else", "repeat", "Reduction", "class", "block", "public", "Complex", "order", "Communicate","decl")
  //  lexical.reserved += ("Domain", "Function", "Unknown", "Operator", "PDE", "PDEBC") 
  lexical.delimiters += ("=", "(", ")", "{", "}", ":", "+", "-", "*", "/", "+=", "|", "[", "]")

  def newline: Parser[Any] = "\n" | "\r\n"

  def parse(input: String) {

    val tokens = new lexical.Scanner(input)
    println(tokens.toString)
    val result = phrase(exastencilsL4)(tokens)
    println(result.toString)
    result match {
      case Success(tree, _) => new Interpreter(tree).run()

      case e: NoSuccess => {
        println(e)
      }
    }

    println(result.toString)
    for (e <- tree.exaClasses)
      println(e)
  }

  class ExaStringOption(val name: String, val value: String) {
  }

  def exastencilsL4 = multigridfunctions.* <~ newline.? ~> dataclasses.*
  //	def exastencilsL4 =  definitions.* <~ newline.? ~> multigridfunctions.* <~ newline.? ~> dataclasses.*

  //def definitions : Parser[Definition] = { "decl" ~ ident ~ ident ~ "=" ~ expr } ^^ {case a ~ typ ~ name ~ b ~ e => exaDefinitions.append(new Definition(name, typ, e)); Definition(name, typ, e) }

  def dataclasses: Parser[AbstractClass] = "class" ~ ident.? ~ ident ~ "{" ~ (paramoption.+) ~ "public" ~ (multigridfunctions.+) ~ "}" ^^ {
    case a ~ templ ~ cname ~ b ~ memb ~ c ~ memf ~ d =>
      tree.exaClasses.append(new AbstractClass(cname, templ, memb, memf)); AbstractClass(cname, templ, memb, memf)
  }

  def data_type: Parser[String] = "Double" | "Int" | "Array" | "Container" | "MyArray" | "Complex" | ident
  def return_datatype: Parser[String] = "real" | "natural" | "Unit" | "Double" | "Complex"
  def operator_type: Parser[String] = "+" | "-" | "*" | "/"
  def modifier_type: Parser[String] = "ToFine" | "ToCoarse"

  def paramoption: Parser[Param] = { ident ~ ":" ~ data_type } ^^ { case k ~ eq ~ v => Param(k, v) }

  def multigridfunctions: Parser[AbstractFunction] = "def" ~ ident ~ ident ~ "(" ~ (paramoption.*) ~ ")" ~ ":" ~ return_datatype ~ "{" ~ (stmt.+) ~ "}" ^^ {
    case a ~ loc ~ name ~ b ~ args ~ c ~ d ~ ret ~ e ~ body ~ f =>
      tree.exaFunctions.append(new AbstractFunction(name, loc, ret, args, body)); AbstractFunction(name, loc, ret, args, body)
  }
  
  def stmt: Parser[AbstractStatement] = ("loop" ~ ident ~ "level" ~ ident ~ "order" ~ ident ~ "block" ~ numericLit ~ numericLit ~ (stmt.+) ~ "next" ^^ {
    case a ~ where ~ b ~ clev ~ c ~ ord ~ d ~ bls ~ blst ~ stmts ~ e => AbstractLoop(where, clev, ord, bls, blst, stmts)
  }
    | ident ~ "=" ~ expr ~ modifier.? ^^ { case id ~ _ ~ e ~ m => AbstractLet(id, e, m) }
    | ident ~ "+=" ~ expr ~ modifier.? ^^ { case id ~ _ ~ e ~ m => AbstractPLet(id, e, m) }
    | ident ~ "(" ~ expr.* ~ ")" ^^ { case id ~ a ~ e ~ b => AbstractPCall(id, e) } 
    | "decl" ~ paramoption ~ "=" ~ expr ^^ {case a ~ para ~ b ~ e => AbstractDefinition(para, e) }
    | "return" ~ expr ^^ { case a ~ e => AbstractReturn(e) }
    | "repeat" ~ ident ~ expr ~ (stmt.+) ~ "next" ^^ { case a ~ id ~ e ~ s ~ f => AbstractRepeat(e, s, id) }
    | "Reduction" ~ stmt ^^ { case a ~ s => AbstractReduction(s) }
    | "Communicate" ~ ident ~ ident ^^ { case a ~ f ~ loc => AbstractCommunication(f,loc) }
    | "if" ~ expr ~ "{" ~ (stmt.+) ~ "}" ~ "else" ~ "{" ~ (stmt.+) ~ "}" ^^ {
      case a ~ cond ~ b ~ ifstmts ~ c ~ d ~ e ~ elsestmts ~ f => AbstractIfElse(cond, ifstmts, elsestmts)
    })

  def modifier = "|" ~ modifier_type ^^ { case a ~ m => m }
  def functioncall = ident ~ "(" ~ expr.* ~ ")" ^^ { case id ~ a ~ e ~ b => AbstractFCall(id, e) }

  def expr: Parser[AbstractExpression] =
    (term ~ "+" ~ term) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp(exastencils.datastructures.ir.BinaryOperators.Addition, lhs, rhs) } |
      (term ~ "-" ~ term) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp(exastencils.datastructures.ir.BinaryOperators.Subtraction, lhs, rhs) } |
      functioncall |
      term

  def term: Parser[AbstractExpression] =
    (factor ~ "*" ~ factor) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp(exastencils.datastructures.ir.BinaryOperators.Multiplication, lhs, rhs) } |
      (factor ~ "/" ~ factor) ^^ { case lhs ~ op ~ rhs => AbstractBinaryOp(exastencils.datastructures.ir.BinaryOperators.Division, lhs, rhs) } |
      factor

  def factor: Parser[AbstractExpression] = ("(" ~> expr <~ ")"
    | stringLit ^^ { case s => AbstractStringLiteral(s) }
    | ident ~ "[" ~ factor ~ "]" ^^ { case id ~ a ~ t ~ b => AbstractVariable(id, t) }
    | numericLit ^^ { case s => AbstractLiteral(s) }
    | ident ^^ { case id => AbstractVariable(id, new AbstractLiteral("")) })

}
