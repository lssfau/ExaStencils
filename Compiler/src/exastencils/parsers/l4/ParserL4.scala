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

  lazy val program = locationize(domain.* ~ layout.* ~ field.* ~ externalField.* ~ stencil.* ~ iterationSet.* ~ function.* ^^
    { case d ~ l ~ f ~ ef ~ s ~ i ~ ss => Root(d, l, f, ef, s, i, ss) })

  //###########################################################

  // ######################################
  // ##### Level Specifications
  // ######################################

  lazy val level = (
    locationize("@" ~> integerLit ^^ { case l => SingleLevelSpecification(l) })
    ||| locationize("@" ~> levelsingle ^^ { case l => l })
    ||| ("@" ~ "(" ~> (levelsingle ||| levelnegation ||| levellist ||| levelrange ||| levelrelative)) ~ ")" ^^ { case l ~ _ => l })

  lazy val levelnegation = (
    locationize((("not" ~ "(") ~> (levelsingle ||| levellist ||| levelrange ||| levelrelative)) ~ ")" ^^ { case l ~ _ => new NegatedLevelSpecification(l) }))

  lazy val levellist = (
    locationize((levelsingle <~ ",").* ~ levelsingle ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x })
    ||| locationize((levelsingle <~ "and").* ~ levelsingle ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x }))

  lazy val levelrange = (
    locationize((levelsingle ||| "(" ~> levelrelative <~ ")") ~ "to" ~ (levelsingle ||| "(" ~> levelrelative <~ ")") ^^ { case b ~ _ ~ e => RangeLevelSpecification(b, e) }))

  lazy val levelrelative = (
    locationize(levelsingle ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => RelativeLevelSpecification(op, l, i) }))

  lazy val levelsingle = (
    locationize("current" ^^ { case _ => CurrentLevelSpecification() })
    ||| locationize("all" ^^ { case _ => AllLevelsSpecification() })
    ||| locationize("coarser" ^^ { case _ => CoarserLevelSpecification() })
    ||| locationize("finer" ^^ { case _ => FinerLevelSpecification() })
    ||| locationize("coarsest" ^^ { case _ => CoarsestLevelSpecification() })
    ||| locationize("finest" ^^ { case _ => FinestLevelSpecification() })
    ||| locationize(integerLit ^^ { case l => SingleLevelSpecification(l) }))

  // ######################################
  // ##### Functions
  // ######################################

  lazy val function = locationize(("def" ~> identifierWithOptionalLevel) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(UnresolvedIdentifier(id, None), t) })
  lazy val functionCall = locationize(identifierWithOptionalLevel ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => exps :+ ex }

  lazy val diagFunctionCall = locationize("diag" ~ "(" ~ identifierWithOptionalLevel ~ ")" ^^ { case _ ~ "(" ~ what ~ ")" => FunctionCallExpression(BasicIdentifier("diag"), List[Expression](what)) })
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

  lazy val domain = (locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => DomainDeclarationStatement(id, l, u) }))

  lazy val variableDeclaration = (locationize(("var" ~> ident) ~ (":" ~> datatype) ~ ("=" ~> expression).? ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(BasicIdentifier(id), dt, exp) }))

  lazy val iterationSet = locationize(("Set" ~> identifierWithOptionalLevel) ~ expressionIndex ~ ("-" ~> expressionIndex) ~ ("steps" ~> expressionIndex).?
    ^^ { case id ~ begin ~ end ~ inc => IterationSetDeclarationStatement(id, begin, end, inc) })

  lazy val repeatUp = locationize(("repeat" ~ "up") ~> numericLit ~ ("{" ~> statement.+ <~ "}") ^^ { case n ~ s => RepeatUpStatement(n.toInt, s) })

  lazy val repeatUntil = locationize(
    (("repeat" ~ "until") ~> comparison) ~ (("{" ~> statement.+) <~ "}") ^^ { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val loopOver = locationize(("loop" ~ "over" ~> ident) ~
    ("on" ~> identifierWithObligatoryLevel) ~ // FIXME: this is all but robust / a user could break this easily
    ("with" ~> loopOverWithClause).? ~
    ("{" ~> statement.+) <~ "}" ^^
    {
      case area ~ field ~ red ~ stmts => LoopOverDomainStatement(area, field, stmts, red)
    })

  lazy val loopOverWithClause = locationize((("reduction" ~ "(") ~> ("+" ||| "*")) ~ (":" ~> identifierWithOptionalLevel <~ ")") ^^ { case op ~ s => ReductionStatement(op, s) })

  lazy val assignment = locationize(identifierWithOptionalLevel ~ "=" ~ expression ^^ { case id ~ "=" ~ exp => AssignmentStatement(id, exp) })

  lazy val operatorassignment = locationize(identifierWithOptionalLevel ~ operatorassignmentoperator ~ expression ^^ {
    case id ~ op ~ exp => AssignmentStatement(id, BinaryExpression(op, id, exp))
  })

  lazy val operatorassignmentoperator = ("+=" ^^ { case _ => "+" }
    ||| "-=" ^^ { case _ => "-" }
    ||| "*=" ^^ { case _ => "*" }
    ||| "/=" ^^ { case _ => "/" })

  lazy val conditional = locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ^^ { case exp ~ stmts => ConditionalStatement(exp, stmts) })

  // ######################################
  // ##### Fields & Layouts
  // ######################################

  lazy val layout = locationize(("Layout" ~> ident) ~ ("{" ~> (layoutOption).+ <~ "}") ^^ { case id ~ opts => var x = LayoutDeclarationStatement(id); x.set(opts); x })
  lazy val layoutOption = locationize((ident <~ "=") ~ index ~ ("with" ~ "communication").? ^^ { case id ~ idx ~ comm => LayoutOption(id, idx, Some(comm.isDefined)) })

  lazy val field = locationize(("Field" ~> ident) ~ "<" ~ datatype ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ level.?
    ^^ {
      case id ~ _ ~ dt ~ layout ~ boundary ~ _ ~ level => FieldDeclarationStatement(id, dt, layout, boundary, level)
    })
  lazy val fieldBoundary = expression ^^ { case x => Some(x) } ||| "None" ^^ { case x => None }

  lazy val index : PackratParser[Index] = (
    locationize("[" ~ integerLit ~ "," ~ integerLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => Index2D(n1, n2) })
    ||| locationize("[" ~ integerLit ~ "," ~ integerLit ~ "," ~ integerLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => Index3D(n1, n2, n3) }))

  lazy val realIndex : PackratParser[RealIndex] = (
    locationize("[" ~ realLit ~ "," ~ realLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => RealIndex2D(n1, n2) })
    ||| locationize("[" ~ realLit ~ "," ~ realLit ~ "," ~ realLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => RealIndex3D(n1, n2, n3) }))

  lazy val expressionIndex : PackratParser[ExpressionIndex] = (
    locationize("[" ~ expression ~ "," ~ expression ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => ExpressionIndex2D(n1, n2) })
    ||| locationize("[" ~ expression ~ "," ~ expression ~ "," ~ expression ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => ExpressionIndex3D(n1, n2, n3) }))

  lazy val stencilentry = ((expressionIndex ~ ("=>" ~> factor)) ^^ { case offset ~ weight => StencilEntry(offset, weight) })

  lazy val stencil = locationize(("Stencil" ~> ident) ~ level.? ~ ("{" ~> stencilentry.+ <~ "}")
    ^^ { case id ~ level ~ entries => StencilDeclarationStatement(id, entries, level) }) // FIXME replace ident by identifierWith*

  lazy val communicateStatement = locationize("communicate" ~> identifierWithOptionalLevel ^^ { case field => CommunicateStatement(field) })

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val identifierWithOptionalLevel = locationize(ident ~ level.? ^^ { case id ~ level => UnresolvedIdentifier(id, level) })
  lazy val identifierWithObligatoryLevel = locationize(ident ~ level ^^ { case id ~ level => FieldIdentifier(id, level) })

  lazy val expression : PackratParser[Expression] = binaryexpression | booleanexpression

  lazy val binaryexpression : PackratParser[Expression] = (
    locationize((expression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term)

  lazy val term : PackratParser[Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "**" ||| "%") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
    ||| locationize(stringLit ^^ { case s => StringConstant(s) })
    ||| locationize("-".? ~ numericLit ^^ {
      case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble)
    })
    ||| locationize(booleanLit ^^ { case s => BooleanConstant(s.toBoolean) })
    ||| locationize(functionCall)
    ||| locationize(diagFunctionCall)
    ||| locationize(toFinerFunctionCall)
    ||| locationize(toCoarserFunctionCall)
    ||| identifierWithOptionalLevel)

  lazy val booleanexpression : PackratParser[BooleanExpression] = (
    locationize(booleanexpression ~ ("||" ||| "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| comparison)

  lazy val comparison : PackratParser[BooleanExpression] =
    locationize((expression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ expression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })

  // ######################################
  // ##### "External" Definitions
  // ######################################

  lazy val externalField = locationize(
    (("external" ~ "Field") ~> ident) ~ ("<" ~> ident <~ ">") ~ "=>" ~ identifierWithOptionalLevel ^^ {
      case extid ~ layout ~ _ ~ intid => ExternalFieldDeclarationStatement(extid, intid, layout)
    })

}
