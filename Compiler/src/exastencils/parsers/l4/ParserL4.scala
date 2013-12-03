package exastencils.parsers.l4

import exastencils.parsers._
import exastencils.datastructures._
import exastencils.datastructures.l4._

class ParserL4 extends ExaParser with scala.util.parsing.combinator.PackratParsers {

  //lexical.reserved += ("Int")

  def parseAll = phrase(program)
  
  var scan = new lexical.Scanner("hallo")

  def parse(input : String) {
    val tokens = new lexical.Scanner(input)
    scan = tokens
    val result = phrase(program)(tokens)
    println(result.toString)
    println(".................")
    println(".................")
    result.get.foreach(f => println(f.getAnnotations.foreach(a => println(a))))//{ if(f.isInstanceOf[Annotatable]) println(f.asInstanceOf[Annotatable].getAnnotations) })
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

  lazy val program = function.*

  lazy val statement : Parser[Statement] = function |
    loop |
    substatement

  lazy val substatement : Parser[Statement] = ident ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(Identifier(id), exp) }

  lazy val function = ("def" ~> ident) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> datatype) ~ ("{" ~> (statement.* <~ "}"))^^
  { case id ~ args ~ t ~ stmts => var x = FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts); x.add(new Annotation("location", scan.pos)); x }
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = ((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(id, t) }
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

  lazy val loop = ("loop" ~ "over" ~> ("domain" | "inner" | "boundary")) ~ ("level" ~> ident) ~
    ("order" ~> ident).? ~ ("blocksize" ~> (numericLit ~ numericLit ~ numericLit)).? ~
    substatement.+ <~ "next" ^^
    { case area ~ level ~ order ~ blocksize ~ stmts => LoopOverDomainStatement(area, level, order, blocksize, stmts) }
}
