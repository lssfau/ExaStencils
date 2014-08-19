package exastencils.parsers.l4

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers._

class ParserL4 extends ExaParser with scala.util.parsing.combinator.PackratParsers {
  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Node = {
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

  lazy val program = definition.* ^^ { case d => Root(d) }

  lazy val definition = domain ||| layout ||| field ||| stencilField ||| externalField ||| stencil ||| iterationSet ||| globals ||| function

  //###########################################################

  lazy val identifierWithOptionalLevel = locationize(ident ~ level.?
    ^^ { case id ~ level => if (level.isDefined) LeveledIdentifier(id, level.get) else BasicIdentifier(id) })

  // ######################################
  // ##### Level Specifications
  // ######################################

  lazy val level = (
    locationize("@" ~> integerLit ^^ { case l => SingleLevelSpecification(l) })
    ||| locationize("@" ~> (levelsingle ||| levelall) ^^ { case l => l })
    ||| locationize("@" ~ "(" ~> (levelsingle ||| levelall ||| levelnegation ||| levellist ||| levelrange ||| levelrelative)) ~ ")" ^^ { case l ~ _ => l })

  lazy val levelnegation = (
    locationize((("not" ~ "(") ~> (levelsingle ||| levellist ||| levelrange ||| levelrelative)) ~ ")" ^^ { case l ~ _ => new NegatedLevelSpecification(l) }))

  lazy val levellist = (
    locationize((levelsingle <~ ",").* ~ levelsingle ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x })
    ||| locationize((levelsingle <~ "and").* ~ levelsingle ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x }))

  lazy val levelrange = (
    locationize((levelsingle ||| "(" ~> levelrelative <~ ")") ~ "to" ~ (levelsingle ||| "(" ~> levelrelative <~ ")") ^^ { case b ~ _ ~ e => RangeLevelSpecification(b, e) }))

  lazy val levelrelative = (
    locationize(levelsingle ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => RelativeLevelSpecification(op, l, i) }))

  lazy val levelall = locationize("all" ^^ { case _ => AllLevelsSpecification() })

  lazy val levelsingle = (
    locationize("current" ^^ { case _ => CurrentLevelSpecification() })
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
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(BasicIdentifier(id), t) })
  lazy val functionCall = locationize((flatAccess ||| leveledAccess) ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (expression <~ ("," | newline)).* ~ expression ^^ { case exps ~ ex => exps :+ ex }

  lazy val diagFunctionCall = locationize("diag" ~ "(" ~ binaryexpression ~ ")" ^^ { case _ ~ "(" ~ expr ~ ")" => FunctionCallExpression(BasicAccess("diag"), List[Expression](expr)) })
  lazy val toFinerFunctionCall = locationize("ToFiner" ~ "(" ~ binaryexpression ~ ")" ^^ { case _ ~ "(" ~ expr ~ ")" => FunctionCallExpression(BasicAccess("ToFiner"), List[Expression](expr)) })
  lazy val toCoarserFunctionCall = locationize("ToCoarser" ~ "(" ~ binaryexpression ~ ")" ^^ { case _ ~ "(" ~ expr ~ ")" => FunctionCallExpression(BasicAccess("ToCoarser"), List[Expression](expr)) })

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[Statement] = (
    variableDeclaration
    ||| repeatUp
    ||| repeatUntil
    ||| loopOver
    ||| loopOverFragments
    ||| assignment
    ||| operatorassignment
    ||| locationize(functionCall ^^ { case f => FunctionCallStatement(f) })
    ||| conditional
    ||| communicateStatement)

  lazy val variableDeclaration = (locationize(("var" ~> ident) ~ (":" ~> datatype) ~ ("=" ~> expression).? ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(BasicIdentifier(id), dt, exp) }))

  lazy val repeatUp = locationize(("repeat" ~ "up") ~> numericLit ~ ("{" ~> statement.+ <~ "}") ^^ { case n ~ s => RepeatUpStatement(n.toInt, s) })
  lazy val repeatUntil = locationize(
    (("repeat" ~ "until") ~> comparison) ~ (("{" ~> statement.+) <~ "}") ^^ { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val loopOverFragments = locationize(("loop" ~ "over" ~ "fragments") ~ ("with" ~> loopOverWithClause).? ~ ("{" ~> statement.+ <~ "}") ^^
    { case _ ~ red ~ stmts => LoopOverFragmentsStatement(stmts, red) })
  lazy val loopOver = locationize(("loop" ~ "over" ~> ident) ~ ("on" ~> fieldLikeAccess) ~ ("with" ~> loopOverWithClause).? ~ ("{" ~> statement.+ <~ "}") ^^
    { case area ~ field ~ red ~ stmts => LoopOverPointsStatement(area, field.resolveToFieldAccess, stmts, red) })
  lazy val loopOverWithClause = locationize((("reduction" ~ "(") ~> ("+" ||| "*")) ~ (":" ~> ident <~ ")") ^^ { case op ~ s => ReductionStatement(op, s) })

  lazy val assignment = locationize((flatAccess ||| fieldLikeAccess) ~ "=" ~ expression ^^ { case id ~ op ~ exp => AssignmentStatement(id, exp, op) })
  lazy val operatorassignment = locationize((flatAccess ||| fieldLikeAccess) ~ ("+=" ||| "-=" ||| "*=" ||| "/=") ~ expression
    ^^ { case id ~ op ~ exp => AssignmentStatement(id, exp, op) })

  lazy val conditional = locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ^^ { case exp ~ stmts => ConditionalStatement(exp, stmts) })

  lazy val communicateStatement = locationize((("begin" ||| "finish").? <~ "communicate") ~ fieldLikeAccess ^^ { case op ~ access => CommunicateStatement(access.resolveToFieldAccess, op.getOrElse("both")) })

  // ######################################
  // ##### Globals
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> variableDeclaration.* <~ "}") ^^ { case entries => GlobalDeclarationStatement(entries) })

  // ######################################
  // ##### Object Declarations
  // ######################################

  lazy val domain = (locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => DomainDeclarationStatement(id, l, u) }))

  lazy val iterationSet = locationize(("Set" ~> identifierWithOptionalLevel) ~ expressionIndex ~ ("-" ~> expressionIndex).? ~ ("steps" ~> expressionIndex).? ~ ("with" ~> booleanexpression).?
    ^^ { case id ~ begin ~ end ~ inc ~ cond => IterationSetDeclarationStatement(id, begin, end, inc, cond) })

  lazy val layout = locationize(("Layout" ~> ident) ~ ("{" ~> layoutOptions <~ "}") ^^ { case id ~ opts => var x = LayoutDeclarationStatement(id); x.set(opts); x })
  lazy val layoutOptions = (
    (layoutOption <~ ",").* ~ layoutOption ^^ { case opts ~ opt => opts.::(opt) }
    ||| layoutOption.+)
  lazy val layoutOption = locationize((ident <~ "=") ~ index ~ ("with" ~ "communication").? ^^ { case id ~ idx ~ comm => LayoutOption(id, idx, Some(comm.isDefined)) })

  lazy val field = locationize(("Field" ~> ident) ~ "<" ~ datatype ~ ("," ~> ident) ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ ("[" ~> integerLit <~ "]").? ~ level.?
    ^^ { case id ~ _ ~ dt ~ domain ~ layout ~ boundary ~ _ ~ slots ~ level => FieldDeclarationStatement(id, dt, domain, layout, boundary, level, slots.getOrElse(1).toInt) })
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

  lazy val stencil = locationize(("Stencil" ~> ident) ~ level.? ~ ("{" ~> stencilEntries <~ "}")
    ^^ { case id ~ level ~ entries => StencilDeclarationStatement(id, entries, level) })
  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
    ||| stencilEntry.+)
  lazy val stencilEntry = ((expressionIndex ~ ("=>" ~> factor)) ^^ { case offset ~ weight => StencilEntry(offset, weight) })

  lazy val stencilField = locationize(("StencilField" ~> ident) ~ ("<" ~> ident <~ "=>") ~ (ident <~ ">") ~ level.?
    ^^ { case sf ~ f ~ s ~ l => StencilFieldDeclarationStatement(sf, f, s, l) })

  // ######################################
  // ##### "External" Definitions
  // ######################################

  lazy val externalField = locationize((("external" ~ "Field") ~> ident) ~ ("<" ~> ident <~ ">") ~ "=>" ~ fieldLikeAccess
    ^^ { case extid ~ layout ~ _ ~ intid => ExternalFieldDeclarationStatement(extid, intid.resolveToFieldAccess, layout) })

  // ######################################
  // ##### Object Access
  // ######################################

  lazy val slotAccess = locationize("[" ~> binaryexpression <~ "]" ^^ { case s => s })

  lazy val levelAccess = (
    locationize("@" ~> levelsingle ^^ { case l => l })
    ||| locationize("@" ~ "(" ~> levelsingle <~ ")" ^^ { case l => l }))

  lazy val flatAccess = locationize(ident
    ^^ { case id => UnresolvedAccess(id, None, None, None) })
  lazy val leveledAccess = locationize(ident ~ levelAccess
    ^^ { case id ~ level => UnresolvedAccess(id, Some(level), None, None) })
  lazy val fieldLikeAccess = locationize(ident ~ slotAccess.? ~ levelAccess ~ ("[" ~> integerLit <~ "]").?
    ^^ { case id ~ slot ~ level ~ arrayIndex => UnresolvedAccess(id, Some(level), slot, arrayIndex) })
  lazy val stencilLikeAccess = locationize(ident ~ levelAccess
    ^^ { case id ~ level => UnresolvedAccess(id, Some(level), None, None) })
  lazy val stencilFieldLikeAccess = locationize(ident ~ slotAccess.? ~ levelAccess.?
    ^^ { case id ~ slot ~ level => UnresolvedAccess(id, level, slot, None) })

  lazy val genericAccess = locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("[" ~> integerLit <~ "]").?
    ^^ { case id ~ slot ~ level ~ arrayIndex => UnresolvedAccess(id, level, slot, arrayIndex) })

  // ######################################
  // ##### Expressions
  // ######################################

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
    ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble) })
    ||| locationize(booleanLit ^^ { case s => BooleanConstant(s.toBoolean) })
    ||| locationize(functionCall)
    ||| locationize(diagFunctionCall)
    ||| locationize(toFinerFunctionCall)
    ||| locationize(toCoarserFunctionCall)
    ||| locationize(fieldLikeAccess)
    ||| locationize(stencilLikeAccess)
    ||| locationize(stencilFieldLikeAccess))

  lazy val booleanexpression : PackratParser[BooleanExpression] = (
    locationize(booleanexpression ~ ("||" ||| "&&") ~ booleanexpression ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| comparison)

  lazy val comparison : PackratParser[BooleanExpression] =
    locationize((expression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ expression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
}
