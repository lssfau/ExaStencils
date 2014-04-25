package exastencils.parsers.l4

import scala.collection.mutable.ListBuffer
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

  var annos = new scala.collection.mutable.ListBuffer[Annotation]

  //###########################################################

  lazy val annotation = ("@" ~> ident) ~ ("(" ~> ((ident | numericLit | stringLit | booleanLit) <~ ")")).? ^^
    { case n ~ v => annos += new Annotation(n, v) }

  //###########################################################

  lazy val program = locationize(field.* ~ stencil.* ~ iterationSet.* ~ function.* ^^ { case f ~ s ~ i ~ ss => Root(f, s, i, ss) })

  //###########################################################

  lazy val level = (
    singlelevel
    ||| ("@" ~ "(" ~> (levelsub ||| levelnegation ||| levellist)) ~ ")" ^^ { case l ~ _ => l })

  lazy val levelnegation : Parser[LevelSpecification] = (
    locationize((("not " ~ "(") ~> (levelsub ||| levellist)) ~ ")" ^^ { case l ~ _ => new NegatedLevelSpecification(l) }))

  lazy val levellist : Parser[LevelSpecification] = (
    locationize((levelsub <~ ",").* ~ levelsub ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x }))

  lazy val levelsub : Parser[LevelSpecification] = (
    locationize("current" ^^ { case _ => CurrentLevelSpecification() })
    ||| locationize("coarser" ^^ { case _ => CoarserLevelSpecification() })
    ||| locationize("finer" ^^ { case _ => FinerLevelSpecification() })
    ||| locationize("coarsest" ^^ { case _ => CoarsestLevelSpecification() })
    ||| locationize("finest" ^^ { case _ => FinestLevelSpecification() })
    ||| locationize(numericLit ^^ { case l => SingleLevelSpecification(l.toInt) })
    ||| locationize(numericLit ~ "to" ~ numericLit ^^ { case b ~ _ ~ e => RangeLevelSpecification(b.toInt, e.toInt) }))

  lazy val singlelevel = locationize("@" ~> numericLit ^^ { case l => SingleLevelSpecification(l.toInt) })

  // ######################################
  // ##### Functions
  // ######################################

  lazy val function = locationize(("def" ~> leveledidentifier) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(UnresolvedIdentifier(id, None), t) })
  lazy val functionCall = locationize(leveledidentifier ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => exps :+ ex }

  lazy val diagFunctionCall = locationize("diag" ~ "(" ~ leveledidentifier ~ ")" ^^ { case _ ~ "(" ~ what ~ ")" => FunctionCallExpression(BasicIdentifier("diag"), List[Expression](what)) })
  lazy val toFinerFunctionCall = locationize("ToFiner" ~ "(" ~ binaryexpression ~ ")" ^^ { case _ ~ "(" ~ stencilconv ~ ")" => FunctionCallExpression(BasicIdentifier("ToFiner"), List[Expression](stencilconv)) })
  lazy val toCoarserFunctionCall = locationize("ToCoarser" ~ "(" ~ binaryexpression ~ ")" ^^ { case _ ~ "(" ~ stencilconv ~ ")" => FunctionCallExpression(BasicIdentifier("ToCoarser"), List[Expression](stencilconv)) })

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[Statement] = (
    variableDeclaration
    ||| repeatUp
    ||| repeatUntil
    ||| loopOver
    ||| assignment
    ||| operatorassignment
    ||| locationize(functionCall ^^ { case f => FunctionCallStatement(f.identifier, f.arguments) })
    ||| conditional
    ||| communicateStatement)

  lazy val domainDeclaration = (locationize(("var" ~> ident) <~ (":" ~ "Domain") ^^ { case id => DomainDeclarationStatement(id) }))
  lazy val variableDeclaration = (locationize(("var" ~> ident) ~ (":" ~> datatype) ~ ("=" ~> expression).? ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(BasicIdentifier(id), dt, exp) }))

  lazy val iterationSet = locationize(("Set" ~> leveledidentifier) ~ index ~ ("-" ~> index) ~ ("steps" ~> index).?
    ^^ { case id ~ begin ~ end ~ inc => IterationSetDeclarationStatement(id, begin, end, inc) })

  lazy val repeatUp = locationize(("repeat" ~ "up") ~> numericLit ~ ("{" ~> statement.+ <~ "}") ^^ { case n ~ s => RepeatUpStatement(n.toInt, s) })

  lazy val repeatUntil = locationize(
    (("repeat" ~ "until") ~> comparison) ~ (("{" ~> statement.+) <~ "}") ^^ { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val reduction = locationize((("reduction" ~ "(") ~> ("+" ||| "*")) ~ (":" ~> leveledidentifier <~ ")") ^^ { case op ~ s => ReductionStatement(op, s) })

  lazy val loopOver = locationize(("loop" ~ "over" ~> ident) ~
    ("on" ~> leveledfieldidentifier) ~ // FIXME: this is all but robust / a user could break this easily
    ("with" ~> reduction).? ~ // FIXME: support other additional commands
    ("{" ~> statement.+) <~ "}" ^^
    {
      case area ~ field ~ red ~ stmts => LoopOverDomainStatement(area, field, stmts, red)
    })

  lazy val assignment = locationize(leveledidentifier ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(id, exp) })

  lazy val operatorassignment = locationize(leveledidentifier ~ operatorassignmentoperator ~ expression ^^ {
    case id ~ op ~ exp => AssignmentStatement(id, BinaryExpression(op, id, exp))
  })

  lazy val operatorassignmentoperator = ("+=" ^^ { case _ => "+" }
    ||| "-=" ^^ { case _ => "-" }
    ||| "*=" ^^ { case _ => "*" }
    ||| "/=" ^^ { case _ => "/" })

  lazy val conditional = locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ^^ { case exp ~ stmts => ConditionalStatement(exp, stmts) })

  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> index <~ ",") ~ (datatype <~ ">") ~ level.? ~ ("(" ~> tempOptions <~ ")").?
    ^^ { case id ~ i ~ t ~ l ~ topts => var f = FieldDeclarationStatement(id, t, i, l); topts.getOrElse(List()).foreach(f.set(_)); f })

  lazy val index : PackratParser[Index] = (
    // FIXME: this has to be done properly
    locationize("[" ~ integerLit ~ "," ~ integerLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => Index2D(n1, n2) })
    ||| locationize("[" ~ integerLit ~ "," ~ integerLit ~ "," ~ integerLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => Index3D(n1, n2, n3) }))

  lazy val expressionIndex : PackratParser[ExpressionIndex] = (
    // FIXME: this has to be done properly
    locationize("[" ~ expression ~ "," ~ expression ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => ExpressionIndex2D(n1, n2) })
    ||| locationize("[" ~ expression ~ "," ~ expression ~ "," ~ expression ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => ExpressionIndex3D(n1, n2, n3) }))

  lazy val tempOptions : PackratParser[List[TempOption]] = (tempOption <~ ",").* ~ tempOption ^^ { case t1 ~ t2 => t1 :+ t2 }
  lazy val tempOption = (locationize(ident ~ "=" ~ ident ^^ { case a ~ _ ~ b => TempOption(a, b) })
    ||| locationize(ident ~ "=" ~ numericLit ^^ { case a ~ _ ~ b => TempOption(a, b) })
    ||| locationize(ident ~ "=" ~ booleanLit ^^ { case a ~ _ ~ b => TempOption(a, b) }))

  lazy val stencilentry = ((expressionIndex ~ ("=>" ~> factor)) ^^ { case offset ~ weight => StencilEntry(offset, weight) })

  lazy val stencil = locationize(("Stencil" ~> ident) ~ level.? ~ ("{" ~> stencilentry.+ <~ "}")
    ^^ { case id ~ level ~ entries => StencilDeclarationStatement(id, entries, level) })

  lazy val communicateStatement = locationize("communicate" ~> leveledidentifier ^^ { case field => CommunicateStatement(field) })

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val leveledidentifier = locationize(ident ~ level.? ^^ { case id ~ level => UnresolvedIdentifier(id, level) })
  lazy val leveledfieldidentifier = locationize(ident ~ level ^^ { case id ~ level => FieldIdentifier(id, level) })

  lazy val expression = binaryexpression | booleanexpression

  lazy val binaryexpression : PackratParser[Expression] = (
    locationize((expression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term)

  lazy val term : PackratParser[Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "**" ||| "%") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| factor)

  lazy val factor : Parser[Expression] = (
    "(" ~> expression <~ ")"
    ||| locationize(stringLit ^^ { case s => StringConstant(s) })
    ||| locationize("-".? ~ numericLit ^^ {
      case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble)
    })
    ||| locationize(booleanLit ^^ { case s => BooleanConstant(s.toBoolean) })
    ||| locationize(functionCall)
    ||| locationize(diagFunctionCall)
    ||| locationize(toFinerFunctionCall)
    ||| locationize(toCoarserFunctionCall)
    ||| leveledidentifier)

  lazy val booleanexpression : PackratParser[BooleanExpression] = (
    locationize(booleanexpression ~ ("||" ||| "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| locationize(expression ~ ("||" ||| "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| locationize(booleanexpression ~ ("||" ||| "&&") ~ expression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| comparison)

  lazy val comparison : PackratParser[BooleanExpression] =
    locationize((expression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ expression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })

}
