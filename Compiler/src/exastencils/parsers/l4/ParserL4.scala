package exastencils.parsers.l4

import exastencils.parsers._
import exastencils.datastructures._
import exastencils.datastructures.l4._

class ParserL4 extends ExaParser with scala.util.parsing.combinator.PackratParsers {
  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Node = {
    import scala.util.parsing.input._
    import scala.collection.immutable.PagedSeq

    val lines = io.Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner)
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
    phrase(program)(tokens) match {
      case Success(e, _) => e
      case Error(msg, _) => throw new Exception("parse error: " + msg)
      case Failure(msg, parser) => {
        var sb = new StringBuilder
        sb.append(s"Parse failure at position ${parser.pos}: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")

        throw new Exception(sb.toString)
      }
    }
  }

  val IntRegEx = """[+-]?(\d+)""".r
  val DoubleRegEx = """[+-]?\d+(\.\d*)?""".r
  protected def isInt(str : String) : Boolean = {
    val x = IntRegEx.findFirstIn(str)
    x.getOrElse(" ") == str
  }

  var annos = new scala.collection.mutable.ListBuffer[Annotation]

  //###########################################################

  lazy val annotation = ("@" ~> ident) ~ ("(" ~> ((ident | numericLit | stringLit | booleanLit) <~ ")")).? ^^
    { case n ~ v => annos += new Annotation(n, v) }

  //###########################################################

  lazy val program = locationize(function.* ^^ { case stmts => Root(stmts) })

  lazy val function = locationize(("def" ~> ident) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(id, t) })
  lazy val functionCall = locationize(ident ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => ex :: exps } // = new list(exps, ex)

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[Statement] = (
    variableDeclaration
    ||| repeatUntil
    ||| loopOver
    ||| assignment
    ||| locationize(functionCall ^^ { case f => FunctionCallStatement(f.name, f.arguments) })
    ||| conditional)

  lazy val variableDeclaration = (
    locationize(("var" ~> ident) <~ (":" ~ "Domain") ^^ { case id => DomainDeclarationStatement(id) })
    ||| locationize(("var" ~> ident) ~ (":" ~> datatype) ~ ("=" ~> expression).? ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(id, dt, exp) }))

  lazy val repeatUntil = locationize(
    (("repeat" ~ "until") ~> comparison) ~ (("{" ~> statement.+) <~ "}") ^^ { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val loopOver = locationize(("loop" ~ "over" ~> loopOverArea) ~
    ("levels" ~> numericLit ^^ { case x => x.toInt }).? ~
    ("order" ~> ident).? ~
    ("blocksize" ~> ((numericLit ~ numericLit ~ numericLit ^^ { case x ~ y ~ z => new LoopOverDomainStatement.Blocksize3D(x.toInt, y.toInt, z.toInt) }) | (numericLit ~ numericLit ^^ { case x ~ y => new LoopOverDomainStatement.Blocksize2D(x.toInt, y.toInt) }))).? ~
    ("{" ~> statement.+) <~ "}" ^^
    {
      case area ~ levels ~ order ~ blocksize ~ stmts => LoopOverDomainStatement(area, levels, order, blocksize, stmts)
    })
  lazy val loopOverArea = "domain" | "inner" | "boundary"

  lazy val assignment = locationize(ident ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(id, exp) })

  lazy val conditional = locationize(("if" ~> booleanexpression) ~ ("{" ~> statement.+ <~ "}") ^^ { case exp ~ stmts => ConditionalStatement(exp, stmts) })

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val expression = binaryexpression | booleanexpression

  lazy val binaryexpression : PackratParser[Expression] = (
    locationize((expression ~ ("+" | "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    | term)

  lazy val term : PackratParser[Expression] = (
    locationize((term ~ ("*" | "/" | "**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    | factor)

  lazy val factor : Parser[Expression] = (
    "(" ~> expression <~ ")"
    | locationize(stringLit ^^ { case s => StringLiteral(s) })
    | locationize(numericLit ^^ {
      case n => if (isInt(n)) NumericLiteral(n.toInt) else NumericLiteral(n.toDouble)
    })
    | locationize(booleanLit ^^ { case s => BooleanLiteral(s.toBoolean) })
    | locationize(functionCall)
    | locationize(ident ^^ { case id => Identifier(id) }))

  lazy val booleanexpression : PackratParser[BooleanExpression] = (
    locationize(booleanexpression ~ ("||" | "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    | locationize(expression ~ ("||" | "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    | locationize(booleanexpression ~ ("||" | "&&") ~ expression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    | comparison)

  lazy val comparison : PackratParser[BooleanExpression] =
    locationize((expression ~ ("<" | "<=" | ">" | ">=" | "==" | "!=") ~ expression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })

}
