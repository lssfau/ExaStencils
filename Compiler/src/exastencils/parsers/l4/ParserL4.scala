package exastencils.parsers.l4

import exastencils.parsers._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.collection.mutable.ListBuffer

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

  var annos = new scala.collection.mutable.ListBuffer[Annotation]

  //###########################################################

  lazy val annotation = ("@" ~> ident) ~ ("(" ~> ((ident | numericLit | stringLit | booleanLit) <~ ")")).? ^^
    { case n ~ v => annos += new Annotation(n, v) }

  //###########################################################

  lazy val program = locationize(field.* ~ function.* ^^ { case f ~ s => Root(f, s) })

  lazy val level = (
    singlelevel
    ||| ("@" ~ "(" ~> levelsub) ~ ")" ^^ { case l ~ _ => l })

  lazy val levelsub : Parser[LevelSpecification] = (
    levelsubsub
    ||| locationize((levelsubsub <~ ",").* ~ levelsubsub ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x }))

  lazy val levelsubsub : Parser[LevelSpecification] = (
    locationize("current" ^^ { case _ => CurrentLevelSpecification() })
    ||| locationize("coarser" ^^ { case _ => CoarserLevelSpecification() })
    ||| locationize("finer" ^^ { case _ => FinerLevelSpecification() })
    ||| locationize(numericLit ^^ { case l => SingleLevelSpecification(l.toInt) })
    ||| locationize(numericLit ~ "to" ~ numericLit ^^ { case b ~ _ ~ e => RangeLevelSpecification(b.toInt, e.toInt) }))

  lazy val singlelevel = locationize("@" ~> numericLit ^^ { case l => SingleLevelSpecification(l.toInt) })

  // ######################################
  // ##### Functions
  // ######################################

  lazy val function = locationize(("def" ~> leveledidentifier) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = locationize(((leveledidentifier <~ ":") ~ datatype) ^^ { case id ~ t => Variable(id, t) })
  lazy val functionCall = locationize(leveledidentifier ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => ex :: exps } // = new list(exps, ex)

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[Statement] = (
    variableDeclaration
    ||| repeatUp
    ||| repeatUntil
    ||| reduction
    ||| loopOver
    ||| assignment
    ||| operatorassignment
    ||| locationize(functionCall ^^ { case f => FunctionCallStatement(f.identifier, f.arguments) })
    ||| conditional)

  lazy val variableDeclaration = (
    locationize(("var" ~> ident) <~ (":" ~ "Domain") ^^ { case id => DomainDeclarationStatement(id) })
    ||| locationize(("var" ~> ident) ~ (":" ~> datatype) ~ ("=" ~> expression).? ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(id, dt, exp) }))

  lazy val repeatUp = locationize(("repeat" ~ "up") ~> numericLit ~ ("{" ~> statement.+ <~ "}") ^^ { case n ~ s => RepeatUpStatement(n.toInt, s) })

  lazy val repeatUntil = locationize(
    (("repeat" ~ "until") ~> comparison) ~ (("{" ~> statement.+) <~ "}") ^^ { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val reduction = locationize(("Reduction" ~ "{") ~> statement <~ "}" ^^ { case s => ReductionStatement(s) })

  lazy val loopOver = locationize(("loop" ~ "over" ~> loopOverArea) ~
    ("level" ~> level).? ~
    ("order" ~> loverOverOrder).? ~
    ("blocksize" ~> index).? ~
    ("{" ~> statement.+) <~ "}" ^^
    {
      case area ~ level ~ order ~ blocksize ~ stmts => LoopOverDomainStatement(area, level, order, blocksize, stmts)
    })
  lazy val loopOverArea = "domain" | "inner" | "boundary"
  lazy val loverOverOrder = "lexical" | "redblack"

  lazy val assignment = locationize(leveledidentifier ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(id, exp) })

  lazy val operatorassignment = locationize(leveledidentifier ~ operatorassignmentoperator ~ expression ^^ {
    case id ~ op ~ exp => AssignmentStatement(id, BinaryExpression(op, id, exp))
  })

  lazy val operatorassignmentoperator = ("+=" ^^ { case _ => "+" }
    ||| "-=" ^^ { case _ => "-" }
    ||| "*=" ^^ { case _ => "*" }
    ||| "/=" ^^ { case _ => "/" })

  lazy val conditional = locationize(("if" ~> booleanexpression) ~ ("{" ~> statement.+ <~ "}") ^^ { case exp ~ stmts => ConditionalStatement(exp, stmts) })

  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> index <~ ",") ~ (datatype <~ ">") ~ level.? ~ ("(" ~> tempOptions <~ ")").?
    ^^ { case id ~ i ~ t ~ l ~ topts => var f = FieldDeclarationStatement(id, t, i, l); topts.getOrElse(List()).foreach(f.set(_)); f })

  lazy val index : PackratParser[Index] = (
    locationize("[" ~ numericLit ~ "," ~ numericLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => Index2D(n1.toInt, n2.toInt) })
    ||| locationize("[" ~ numericLit ~ "," ~ numericLit ~ "," ~ numericLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => Index3D(n1.toInt, n2.toInt, n3.toInt) }))

  lazy val tempOptions : PackratParser[List[TempOption]] = (tempOption <~ ",").* ~ tempOption ^^ { case t1 ~ t2 => t1 :+ t2 }
  lazy val tempOption = (locationize(ident ~ "=" ~ ident ^^ { case a ~ _ ~ b => TempOption(a, b) })
    ||| locationize(ident ~ "=" ~ numericLit ^^ { case a ~ _ ~ b => TempOption(a, b) })
    ||| locationize(ident ~ "=" ~ booleanLit ^^ { case a ~ _ ~ b => TempOption(a, b) }))

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val leveledidentifier = locationize(ident ~ level.? ^^ { case id ~ level => Identifier(id, level) })

  lazy val expression = binaryexpression | booleanexpression

  lazy val binaryexpression : PackratParser[Expression] = (
    locationize((expression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term)

  lazy val term : PackratParser[Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| factor)

  lazy val factor : Parser[Expression] = (
    "(" ~> expression <~ ")"
    ||| locationize(stringLit ^^ { case s => StringConstant(s) })
    ||| locationize("-".? ~ numericLit ^^ {
      case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble)
    })
    ||| locationize(booleanLit ^^ { case s => BooleanConstant(s.toBoolean) })
    ||| locationize(functionCall)
    ||| leveledidentifier)

  lazy val booleanexpression : PackratParser[BooleanExpression] = (
    locationize(booleanexpression ~ ("||" ||| "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| locationize(expression ~ ("||" ||| "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| locationize(booleanexpression ~ ("||" ||| "&&") ~ expression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| comparison)

  lazy val comparison : PackratParser[BooleanExpression] =
    locationize((expression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ expression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })

}
