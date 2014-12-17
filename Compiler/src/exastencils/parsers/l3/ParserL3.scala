package exastencils.parsers.l3

import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader

import exastencils.datastructures._
import exastencils.datastructures.l3._
import exastencils.parsers._

class ParserL3 extends ExaParser with scala.util.parsing.combinator.PackratParsers {
  override val lexical : ExaLexer = new LexerL3()

  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Root = {
    val lines = io.Source.fromFile(filename).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    parseTokens(scanner).asInstanceOf[Root]
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

  // ######################################
  // ##### Datatypes
  // ######################################

  lazy val datatype : Parser[ScType] = (
    simpleDatatype
    ||| numericDatatype
    ||| algorithmicDatatype)

  lazy val simpleDatatype : Parser[ScType] = (
    "String" ^^ { case x => new StringDatatype }
    ||| numericSimpleDatatype)

  lazy val numericDatatype : Parser[ScType] = (
    ("Complex" ~ "[") ~> numericSimpleDatatype <~ "]" ^^ { case x => new ComplexDatatype(x) }
    ||| numericSimpleDatatype)

  lazy val numericSimpleDatatype : Parser[ScType] = (
    "Integer" ^^ { case x => new IntegerDatatype }
    ||| "Real" ^^ { case x => new RealDatatype })

  lazy val returnDatatype = ("Unit" ^^ { case x => new UnitDatatype }
    ||| datatype)

  lazy val algorithmicDatatype = ("Stencil" ^^ { case x => new StencilDatatype }
    ||| "Field" ^^ { case x => new FieldDatatype })

  lazy val program = definition.* ^^ { case d => Root(d) }

  lazy val definition = function ||| instantiation

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

  lazy val function = locationize((("Func" ||| "Function") ~> ident) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}")) ^^
    { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => arg :: args }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(id, t) })
  lazy val functionCall = locationize(ident ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })
  lazy val functionCallArgumentList = (binaryexpression <~ ("," | newline)).* ~ binaryexpression ^^ { case exps ~ ex => exps :+ ex }

  // ######################################
  // ##### Instantiations
  // ######################################

  lazy val instantiation = locationize(((("Inst" ||| "Instantiate") ~ ("Func" ||| "Function")) ~> ident) ~ (("with" ~ "(") ~> functionCallArgumentList) ~ ")" ~ level ^^ { case id ~ args ~ _ ~ l => FunctionInstantiationStatement(id, args, l) })

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[Statement] = (
    variableDeclaration
    //    ||| valueDeclaration
    //    ||| repeatNTimes
    //    ||| repeatUntil
    //    ||| loopOver
    //    ||| loopOverFragments
    ||| assignment
    //    ||| operatorassignment
    ||| locationize(functionCall ^^ { case f => FunctionCallStatement(f) }) //    ||| conditional
    //    ||| applyBCsStatement
    )

  lazy val variableDeclaration = (locationize((("Var" ||| "Variable") ~> ident) ~ (":" ~> datatype) ~ ("=" ~> binaryexpression).?
    ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(id, dt, exp) }))

  //  lazy val valueDeclaration = (locationize((("Val" ||| "Value") ~> identifierWithOptionalLevel) ~ (":" ~> datatype) ~ ("=" ~> binaryexpression)
  //    ^^ { case id ~ dt ~ exp => ValueDeclarationStatement(id, dt, exp) }))
  //
  //  lazy val repeatNTimes = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ ("with" ~> "contraction").? ~ ("{" ~> statement.+ <~ "}") ^^
  //    { case n ~ i ~ c ~ s => RepeatUpStatement(n.toInt, i, c.isDefined, s) })
  //  lazy val repeatUntil = locationize((("repeat" ~ "until") ~> simpleComparison) ~ (("{" ~> statement.+) <~ "}") ^^
  //    { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val assignment = locationize(ident ~ "=" ~ binaryexpression ^^ { case id ~ op ~ exp => AssignmentStatement(id, exp, op) })

  //  lazy val conditional = locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ (("else" ~ "{") ~> statement.+ <~ "}").?
  //    ^^ { case exp ~ stmts ~ elsestmts => ConditionalStatement(exp, stmts, elsestmts.getOrElse(List())) })

  // ######################################
  // ##### Object Declarations
  // ######################################
  //
  //  lazy val domain = (locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => DomainDeclarationStatement(id, l, u) }))
  //
  //  lazy val iterationSet = locationize(("Set" ~> identifierWithOptionalLevel) ~ expressionIndex ~ ("-" ~> expressionIndex).? ~ ("steps" ~> expressionIndex).? ~ ("with" ~> booleanexpression).?
  //    ^^ { case id ~ begin ~ end ~ inc ~ cond => IterationSetDeclarationStatement(id, begin, end, inc, cond) })
  //
  //  lazy val layout = locationize(("Layout" ~> ident) ~ ("<" ~> datatype <~ ">") ~ level.? ~ ("{" ~> layoutOptions <~ "}")
  //    ^^ { case id ~ dt ~ level ~ opts => var x = LayoutDeclarationStatement(LeveledIdentifier(id, level.getOrElse(new AllLevelsSpecification)), dt); x.set(opts); x })
  //  lazy val layoutOptions = (
  //    (layoutOption <~ ",").* ~ layoutOption ^^ { case opts ~ opt => opts.::(opt) }
  //    ||| layoutOption.+)
  //  lazy val layoutOption = locationize((ident <~ "=") ~ index ~ ("with" ~ "communication").? ^^ { case id ~ idx ~ comm => LayoutOption(id, idx, Some(comm.isDefined)) })
  //
  //  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> ident) ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ ("[" ~> integerLit <~ "]").? ~ level.?
  //    ^^ { case id ~ domain ~ layout ~ boundary ~ _ ~ slots ~ level => FieldDeclarationStatement(LeveledIdentifier(id, level.getOrElse(new AllLevelsSpecification)), domain, layout, boundary, slots.getOrElse(1).toInt) })
  //  lazy val fieldBoundary = binaryexpression ^^ { case x => Some(x) } ||| "None" ^^ { case x => None }
  //
  //  lazy val index : PackratParser[Index] = (
  //    locationize("[" ~ integerLit ~ "," ~ integerLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => Index2D(n1, n2) })
  //    ||| locationize("[" ~ integerLit ~ "," ~ integerLit ~ "," ~ integerLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => Index3D(n1, n2, n3) }))
  //
  //  lazy val realIndex : PackratParser[RealIndex] = (
  //    locationize("[" ~ realLit ~ "," ~ realLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => RealIndex2D(n1, n2) })
  //    ||| locationize("[" ~ realLit ~ "," ~ realLit ~ "," ~ realLit ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => RealIndex3D(n1, n2, n3) }))
  //
  //  lazy val expressionIndex : PackratParser[ExpressionIndex] = (
  //    locationize("[" ~ binaryexpression ~ "," ~ binaryexpression ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ => ExpressionIndex2D(n1, n2) })
  //    ||| locationize("[" ~ binaryexpression ~ "," ~ binaryexpression ~ "," ~ binaryexpression ~ "]" ^^ { case _ ~ n1 ~ _ ~ n2 ~ _ ~ n3 ~ _ => ExpressionIndex3D(n1, n2, n3) }))
  //
  //  lazy val stencil = locationize(("Stencil" ~> identifierWithOptionalLevel) ~ ("{" ~> stencilEntries <~ "}")
  //    ^^ { case id ~ entries => StencilDeclarationStatement(id, entries) })
  //  lazy val stencilEntries = (
  //    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
  //    ||| stencilEntry.+)
  //  lazy val stencilEntry = ((expressionIndex ~ ("=>" ~> factor)) ^^ { case offset ~ weight => StencilEntry(offset, weight) })
  //
  //  lazy val stencilField = locationize((("StencilField" ~> ident) ~ ("<" ~> ident <~ "=>") ~ (ident <~ ">") ~ level.?)
  //    ^^ { case id ~ f ~ s ~ level => StencilFieldDeclarationStatement(LeveledIdentifier(id, level.getOrElse(new AllLevelsSpecification)), f, s) })
  //

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val binaryexpression : PackratParser[Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term)

  lazy val term : PackratParser[Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%") ~ term2) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term2)

  lazy val term2 : PackratParser[Expression] = (
    locationize((term2 ~ ("**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
    ||| locationize(stringLit ^^ { case s => StringConstant(s) })
    ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble) })
    ||| functionCall
    ||| ident ^^ { case id => IdentifierExpression(id) })

  //  lazy val booleanexpression : PackratParser[Expression] = (
  //    locationize(("!" ~> booleanexpression1) ^^ { case ex => UnaryBooleanExpression("!", ex) })
  //    ||| booleanexpression1)
  //
  //  lazy val booleanexpression1 : PackratParser[Expression] = (
  //    locationize((booleanexpression1 ~ "&&" ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
  //    ||| booleanexpression2)
  //
  //  lazy val booleanexpression2 : PackratParser[Expression] = (
  //    locationize((booleanexpression2 ~ "||" ~ booleanexpression3) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
  //    ||| booleanexpression3)
  //
  //  lazy val booleanexpression3 : PackratParser[Expression] = (
  //    "(" ~> booleanexpression <~ ")"
  //    ||| locationize(booleanLit ^^ { case s => BooleanConstant(s.toBoolean) })
  //    ||| comparison
  //    ||| functionCall
  //    ||| genericAccess)
  //
  //  // simpleComparison is to be used in statements such as Repeat Until
  //  lazy val simpleComparison : PackratParser[BooleanExpression] =
  //    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
  //
  //  lazy val comparison : PackratParser[BooleanExpression] =
  //    locationize(((binaryexpression ||| booleanexpression) ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ (binaryexpression ||| booleanexpression)) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
}
