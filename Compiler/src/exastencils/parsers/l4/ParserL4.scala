package exastencils.parsers.l4

import scala.collection.immutable.PagedSeq
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.PagedSeqReader

import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.parsers._

class ParserL4 extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = new LexerL4()

  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  def parseFile(filename : String) : Node = {
    println(filename)
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

  //###########################################################

  lazy val program = (domain ||| layout ||| field ||| stencilField ||| externalField ||| stencil ||| globals ||| function ||| functionTemplate ||| functionInstantiation).+ ^^ { case d => Root(d) }

  //###########################################################

  lazy val identifierWithOptionalLevel = locationize(ident ~ level.?
    ^^ { case id ~ level => if (level.isDefined) LeveledIdentifier(id, level.get) else BasicIdentifier(id) })

  // ######################################
  // ##### Level Specifications
  // ######################################

  lazy val level = (
    locationize("@" ~> (levelsingle ||| levelall) ^^ { case l => l })
    ||| locationize("@" ~ "(" ~> levellist ~ ")" ^^ { case l ~ _ => l }))

  lazy val levellist = (
    locationize(((levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation) <~ ("," ||| "and")).* ~ (levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation) ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x }))

  lazy val levelsublist = (
    locationize(((levelsingle ||| levelrange ||| levelrelative) <~ ("," ||| "and")).* ~ (levelsingle ||| levelrange ||| levelrelative) ^^ { case a ~ b => var x = new ListLevelSpecification(); a.foreach(x.add(_)); x.add(b); x }))

  lazy val levelnegation = (
    locationize(("not" ~ "(") ~> levelsublist <~ ")") ^^ { case l => new NegatedLevelSpecification(l) })

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
  // ##### Datatypes
  // ######################################

  lazy val datatype : Parser[Datatype] = (
    simpleDatatype
    ||| algorithmicDatatype
    ||| "Array" ~ ("<" ~> datatype <~ ">") ~ ("<" ~> integerLit <~ ">") ^^ { case _ ~ x ~ s => new ArrayDatatype(x, s) })

  lazy val simpleDatatype : Parser[Datatype] = (
    "String" ^^ { case _ => new StringDatatype }
    ||| ("Boolean" ||| "Bool") ^^ { case _ => new BooleanDatatype }
    ||| numericDatatype)

  lazy val algorithmicDatatype : Parser[Datatype] = (
    ("Complex" ~ "<") ~> numericDatatype <~ ">" ^^ { case x => new ComplexDatatype(x) }
    ||| "Vector" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => new VectorDatatype(x, s, true) }
    ||| ("ColumnVector" ||| "CVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => new VectorDatatype(x, s, false) }
    ||| numericDatatype ~ ("<" ~> integerLit <~ ">") ^^ { case x ~ s => new VectorDatatype(x, s, true) }
    ||| "Matrix" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ m ~ n => new MatrixDatatype(x, m, n) }
    ||| numericDatatype ~ ("<" ~> integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case x ~ m ~ n => new MatrixDatatype(x, m, n) }
    ||| numericDatatype)

  lazy val numericDatatype : Parser[Datatype] = (
    ("Integer" ||| "Int") ^^ { case x => new IntegerDatatype }
    ||| "Real" ^^ { case x => new RealDatatype })

  lazy val returnDatatype = ("Unit" ^^ { case x => new UnitDatatype }
    ||| datatype)

  // ######################################
  // ##### Functions
  // ######################################

  lazy val function = locationize((("Func" ||| "Function") ~> identifierWithOptionalLevel) ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case id ~ args ~ t ~ stmts => FunctionStatement(id, t, args.getOrElse(List[Variable]()), stmts) })
  lazy val functionArgumentList = /*locationize*/ ((functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => args :+ arg })
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => Variable(BasicIdentifier(id), t) })

  lazy val functionCallArgumentList = /*locationize*/ (((binaryexpression ||| booleanexpression) <~ ("," | newline)).* ~ (binaryexpression ||| booleanexpression) ^^ { case exps ~ ex => exps :+ ex })
  lazy val functionCall = locationize((flatAccess ||| leveledAccess) ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => FunctionCallExpression(id, args.getOrElse(List[Expression]())) })

  lazy val functionTemplateArgList = /*locationize*/ ((ident <~ ("," | newline)).* ~ ident ^^ { case args ~ arg => args :+ arg })
  lazy val functionInstArgList = /*locationize*/ ((functionInstArgument <~ ("," | newline)).* ~ functionInstArgument ^^ { case args ~ arg => args :+ arg })
  lazy val functionInstArgument = (binaryexpression ||| booleanexpression)
  lazy val functionTemplate = locationize((("FuncTemplate" ||| "FunctionTemplate") ~> ident) ~ ("<" ~> functionTemplateArgList.? <~ ">") ~ ("(" ~> (functionArgumentList.?) <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case id ~ templateArgs ~ functionArgs ~ retType ~ stmts => FunctionTemplateStatement(id, templateArgs.getOrElse(List()), functionArgs.getOrElse(List()), retType, stmts) })
  lazy val functionInstantiation = locationize(((("Inst" ||| "Instantiate") ~> ident) ~ ("<" ~> functionInstArgList.? <~ ">") ~ ("as" ~> identifierWithOptionalLevel))
    ^^ { case template ~ args ~ target => FunctionInstantiationStatement(template, args.getOrElse(List()), target) })

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[Statement] = (
    variableDeclaration
    ||| valueDeclaration
    ||| repeatNTimes
    ||| repeatUntil
    ||| loopOver
    ||| loopOverFragments
    ||| assignment
    ||| operatorassignment
    ||| locationize(functionCall ^^ { case f => FunctionCallStatement(f) })
    ||| conditional
    ||| applyBCsStatement
    ||| communicateStatement
    ||| returnStatement
    ||| advanceStatement
    ||| leveledScope)

  lazy val statementInsideRepeat = statement ||| breakStatement

  lazy val variableDeclaration = (locationize((("Var" ||| "Variable") ~> identifierWithOptionalLevel) ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ dt ~ exp => VariableDeclarationStatement(id, dt, exp) }))

  lazy val valueDeclaration = (locationize((("Val" ||| "Value") ~> identifierWithOptionalLevel) ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ dt ~ exp => ValueDeclarationStatement(id, dt, exp) }))

  lazy val repeatNTimes = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ contractionClause.? ~ ("{" ~> statementInsideRepeat.+ <~ "}") ^^
    { case n ~ i ~ c ~ s => RepeatTimesStatement(n.toInt, i, c, s) })
  lazy val contractionClause = locationize("with" ~ "contraction" ~> index ~ ("," ~> index).? ^^ { case l ~ r => new ContractionSpecification(l, r) })

  lazy val repeatUntil = locationize((("repeat" ~ "until") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.+) <~ "}") ^^
    { case c ~ s => RepeatUntilStatement(c, s) })

  lazy val breakStatement = locationize("break" ^^ { case _ => BreakStatement() })

  lazy val loopOverFragments = locationize(("loop" ~ "over" ~ "fragments") ~ ("with" ~> reductionClause).? ~ ("{" ~> statement.+ <~ "}") ^^
    { case _ ~ red ~ stmts => LoopOverFragmentsStatement(stmts, red) })
  lazy val loopOver = locationize(("loop" ~ "over" ~> genericAccess) ~ //fieldAccess
    ("only" ~> regionSpecification).? ~
    ("sequentially").? ~ // FIXME: seq HACK
    ("where" ~> booleanexpression).? ~
    ("starting" ~> expressionIndex).? ~
    ("ending" ~> expressionIndex).? ~
    ("stepping" ~> expressionIndex).? ~
    ("with" ~> reductionClause).? ~
    precomm.* ~
    postcomm.* ~
    ("{" ~> statement.+ <~ "}") ^^ {
      case field ~ region ~ seq ~ cond ~ startOff ~ endOff ~ inc ~ red ~ prec ~ postc ~ stmts =>
        LoopOverPointsStatement(field, region, seq.isDefined, cond, startOff, endOff, inc, stmts, red, prec, postc)
    })
  lazy val reductionClause = locationize((("reduction" ~ "(") ~> (ident ||| "+" ||| "*")) ~ (":" ~> ident <~ ")") ^^ { case op ~ s => ReductionStatement(op, s) })
  lazy val regionSpecification = locationize((("ghost" ||| "dup" ||| "inner") ~ index ~ ("on" <~ "boundary").?) ^^ { case region ~ dir ~ bc => RegionSpecification(region, dir, bc.isDefined) })

  lazy val assignment = locationize(genericAccess ~ "=" ~ (binaryexpression ||| booleanexpression) ^^ { case id ~ op ~ exp => AssignmentStatement(id, exp, op) })
  lazy val operatorassignment = locationize(genericAccess ~ ("+=" ||| "-=" ||| "*=" ||| "/=") ~ binaryexpression
    ^^ { case id ~ op ~ exp => AssignmentStatement(id, exp, op) })

  lazy val conditional : PackratParser[ConditionalStatement] = (
    locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ (("else" ~ "{") ~> statement.+ <~ "}").?
      ^^ { case exp ~ stmts ~ elsestmts => ConditionalStatement(exp, stmts, elsestmts.getOrElse(List())) })
    ||| locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ ("else" ~> conditional)
      ^^ { case exp ~ stmts ~ elsecond => ConditionalStatement(exp, stmts, List[Statement](elsecond)) }))

  lazy val applyBCsStatement = locationize(("apply" ~ "bc" ~ "to") ~> genericAccess //fieldAccess
    ^^ { case field => ApplyBCsStatement(field) })
  lazy val communicateStatement = locationize((("begin" ||| "finish").? <~ ("communicate" ||| "communicating")) ~ communicateTarget.* ~ (("of").? ~> genericAccess) //fieldAccess
    ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => CommunicateStatement(field, op.getOrElse("both"), targets, cond) })
  lazy val communicateTarget = locationize(("all" ||| "dup" ||| "ghost") ~ index.? ~ ("to" ~> index).? // inclucive indices
    ^^ { case target ~ start ~ end => CommunicateTarget(target, start, end) })
  lazy val precomm = locationize("precomm" ~> ("begin" ||| "finish").? ~ communicateTarget.* ~ (("of").? ~> genericAccess) ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => CommunicateStatement(field, op.getOrElse("both"), targets, cond) })
  lazy val postcomm = locationize("postcomm" ~> ("begin" ||| "finish").? ~ communicateTarget.* ~ (("of").? ~> genericAccess) ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => CommunicateStatement(field, op.getOrElse("both"), targets, cond) })

  lazy val returnStatement = locationize("return" ~> (binaryexpression ||| booleanexpression).? ^^ { case exp => ReturnStatement(exp) })

  lazy val leveledScope = locationize((level <~ "{") ~ (statement.+ <~ "}") ^^ { case l ~ s => LeveledScopeStatement(l, s) })

  // ######################################
  // ##### Globals
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> globalEntry.* <~ "}") ^^ { case entries => new GlobalDeclarationStatement(entries) })
  lazy val globalEntry : PackratParser[Statement] = locationize(valueDeclaration ||| variableDeclaration)

  // ######################################
  // ##### Object Declarations
  // ######################################

  lazy val domain = (
    locationize(("Domain" ~> "fromFile" ~> ("(" ~> stringLit <~ ")")) ^^ { case file => { DomainDeclarationStatement(file, null, null) } })
    ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => DomainDeclarationStatement(id, l, u) })
    ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l1 ~ u1 ~ l2 ~ u2 ~ l3 ~ u3 => DomainDeclarationStatement(id, List(l1, l2, l3), List(u1, u2, u3)) })
    ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l1 ~ u1 ~ l2 ~ u2 ~ l3 ~ u3 ~ l4 ~ u4 ~ l5 ~ u5 => DomainDeclarationStatement(id, List(l1, l2, l3, l4, l5), List(u1, u2, u3, u4, u5)) }))

  lazy val discretization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ { case d => d })
  lazy val layout = locationize(("Layout" ~> ident) ~ ("<" ~> datatype <~ ",") ~ (discretization <~ ">") ~ level.? ~ ("{" ~> layoutOptions <~ "}")
    ^^ { case id ~ dt ~ disc ~ level ~ opts => var x = LayoutDeclarationStatement(LeveledIdentifier(id, level.getOrElse(new AllLevelsSpecification)), dt, disc.toLowerCase); x.set(opts); x })
  lazy val layoutOptions = (
    (layoutOption <~ ",").* ~ layoutOption ^^ { case opts ~ opt => opts.::(opt) }
    ||| layoutOption.*)
  lazy val layoutOption = locationize((("ghostLayers" ||| "duplicateLayers" ||| "innerPoints") <~ "=") ~ index ~ ("with" ~ "communication").?
    ^^ { case id ~ idx ~ comm => LayoutOption(id, idx, Some(comm.isDefined)) })

  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> ident) ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ ("[" ~> integerLit <~ "]").? ~ level.?
    ^^ { case id ~ domain ~ layout ~ boundary ~ _ ~ slots ~ level => FieldDeclarationStatement(LeveledIdentifier(id, level.getOrElse(new AllLevelsSpecification)), domain, layout, boundary, slots.getOrElse(1).toInt) })
  lazy val fieldBoundary = binaryexpression ^^ { case x => Some(x) } ||| "None" ^^ { case x => None }

  lazy val index : PackratParser[Index] = (
    index1d
    ||| index2d
    ||| index3d)

  lazy val index1d = locationize("[" ~> integerLit <~ "]" ^^ { case n1 => Index1D(n1) })
  lazy val index2d = locationize(("[" ~> integerLit <~ ",") ~ (integerLit <~ "]") ^^ { case n1 ~ n2 => Index2D(n1, n2) })
  lazy val index3d = locationize(("[" ~> integerLit <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ "]") ^^ { case n1 ~ n2 ~ n3 => Index3D(n1, n2, n3) })

  lazy val realIndex : PackratParser[RealIndex] = (
    locationize("[" ~> realLit <~ "]" ^^ { case n1 => RealIndex1D(n1) })
    ||| locationize(("[" ~> realLit <~ ",") ~ (realLit <~ "]") ^^ { case n1 ~ n2 => RealIndex2D(n1, n2) })
    ||| locationize(("[" ~> realLit <~ ",") ~ (realLit <~ ",") ~ (realLit <~ "]") ^^ { case n1 ~ n2 ~ n3 => RealIndex3D(n1, n2, n3) }))

  lazy val expressionIndex : PackratParser[ExpressionIndex] = (
    locationize("[" ~> binaryexpression <~ "]" ^^ { case n1 => ExpressionIndex1D(n1) })
    ||| locationize(("[" ~> binaryexpression <~ ",") ~ (binaryexpression <~ "]") ^^ { case n1 ~ n2 => ExpressionIndex2D(n1, n2) })
    ||| locationize(("[" ~> binaryexpression <~ ",") ~ (binaryexpression <~ ",") ~ (binaryexpression <~ "]") ^^ { case n1 ~ n2 ~ n3 => ExpressionIndex3D(n1, n2, n3) }))

  lazy val stencil = locationize(("Stencil" ~> identifierWithOptionalLevel) ~ ("{" ~> stencilEntries <~ "}")
    ^^ { case id ~ entries => StencilDeclarationStatement(id, entries) })
  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
    ||| stencilEntry.+)
  lazy val stencilEntry = ((expressionIndex ~ ("=>" ~> (binaryexpression ||| matrixExpression))) ^^ { case offset ~ weight => StencilEntry(offset, weight) })

  lazy val stencilField = locationize((("StencilField" ~> ident) ~ ("<" ~> ident <~ "=>") ~ (ident <~ ">") ~ level.?)
    ^^ { case id ~ f ~ s ~ level => StencilFieldDeclarationStatement(LeveledIdentifier(id, level.getOrElse(new AllLevelsSpecification)), f, s) })

  // ######################################
  // ##### "External" Definitions
  // ######################################

  lazy val externalField = locationize((("external" ~ "Field") ~> ident) ~ ("<" ~> ident <~ ">") ~ ("=>" ~> fieldAccess)
    ^^ { case extid ~ layout ~ field => ExternalFieldDeclarationStatement(extid, field, layout) })

  // ######################################
  // ##### Object Access
  // ######################################

  lazy val slotAccess = locationize(
    "$" ~> slotModifier ^^ { case s => s }
      ||| "[" ~> slotModifier <~ "]" ^^ { case s => s })

  lazy val slotModifier = locationize("active" ^^ { case _ => SlotModifier.Active() }
    ||| "activeSlot" ^^ { case _ => SlotModifier.Active() }
    ||| "currentSlot" ^^ { case _ => SlotModifier.Active() }
    ||| "next" ^^ { case _ => SlotModifier.Next() }
    ||| "nextSlot" ^^ { case _ => SlotModifier.Next() }
    ||| "previous" ^^ { case _ => SlotModifier.Previous() }
    ||| "previousSlot" ^^ { case _ => SlotModifier.Previous() }
    ||| integerLit ^^ { case i => SlotModifier.Constant(i) })

  lazy val advanceStatement = locationize("advance" ~> leveledAccess ^^ { case a => AdvanceStatement(a) })

  lazy val levelAccess = (
    locationize("@" ~> levelsingle ^^ { case l => l })
    ||| locationize("@" ~ "(" ~> levelsingle <~ ")" ^^ { case l => l }))

  lazy val componentIndex = locationize(index1d ^^ { case x => new ComponentIndex1d(x) }
    ||| (("[" ~> integerLit.?) <~ ":") ~ (integerLit.? <~ "]") ^^ { case a ~ b => ComponentIndex1d(a, b) }
    ||| index2d ^^ { case x => new ComponentIndex2d(x) }
    ||| ("[" ~> integerLit.?) ~ (":" ~> integerLit.? <~ ",") ~ (integerLit.? <~ ":") ~ (integerLit.? <~ "]") ^^ { case a ~ b ~ c ~ d => ComponentIndex2d(ComponentIndex1d(a, b), ComponentIndex1d(a, b)) })

  lazy val fieldAccess = locationize(ident ~ slotAccess.? ~ levelAccess ~ componentIndex.?
    ^^ { case id ~ slot ~ level ~ cIndex => FieldAccess(id, level, slot.getOrElse(SlotModifier.Active()), cIndex) })

  lazy val flatAccess = locationize(ident
    ^^ { case id => UnresolvedAccess(id, None, None, None, None, None) })
  lazy val leveledAccess = locationize(ident ~ levelAccess
    ^^ { case id ~ level => UnresolvedAccess(id, None, Some(level), None, None, None) })

  lazy val genericAccess = (
    locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> expressionIndex).? ~ componentIndex.?
      ^^ { case id ~ slot ~ level ~ offset ~ cIndex => UnresolvedAccess(id, slot, level, offset, cIndex, None) })
    ||| locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> expressionIndex).? ~ (":" ~> expressionIndex).?
      ^^ { case id ~ slot ~ level ~ offset ~ dirAccess => UnresolvedAccess(id, slot, level, offset, None, dirAccess) }))

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val binaryexpression : PackratParser[Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-" ||| ".+" ||| ".-") ~ term) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term)

  lazy val term : PackratParser[Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%" ||| ".*" ||| "./" ||| ".%") ~ term2) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| term2)

  lazy val term2 : PackratParser[Expression] = (
    locationize((term2 ~ ("**" ||| "^" ||| ".**") ~ factor) ^^ { case lhs ~ op ~ rhs => BinaryExpression(op, lhs, rhs) })
    ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
    ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { case exp => UnaryExpression("-", exp) }
    ||| rowVectorExpression
    ||| columnVectorExpression
    ||| matrixExpression
    ||| locationize(stringLit ^^ { case s => StringConstant(s) })
    ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) IntegerConstant((s.getOrElse("") + n).toInt) else FloatConstant((s.getOrElse("") + n).toDouble) })
    ||| locationize("-" ~> functionCall ^^ { case x => UnaryExpression("-", x) })
    ||| functionCall
    ||| locationize("-" ~> genericAccess ^^ { case x => UnaryExpression("-", x) })
    ||| genericAccess
    ||| locationize(booleanLit ^^ { case s => BooleanConstant(s) }))

  lazy val rowVectorExpression = locationize("{" ~> (binaryexpression <~ ",").+ ~ (binaryexpression <~ "}") ^^ { case x ~ y => VectorExpression(None, x :+ y, true) })

  lazy val columnVectorExpression = locationize(rowVectorExpression <~ "T" ^^ { case x => VectorExpression(None, x.expressions, false) })

  lazy val matrixExpression = locationize("{" ~> (rowVectorExpression <~ ",").+ ~ (rowVectorExpression <~ "}") ^^ { case x ~ y => MatrixExpression(None, x :+ y) })

  lazy val booleanexpression : PackratParser[Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
    ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ { case ex => UnaryBooleanExpression("!", ex) })
    ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[Expression] = (
    "(" ~> booleanexpression <~ ")"
    ||| comparison
    ||| binaryexpression)

  lazy val comparison : PackratParser[BooleanExpression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => BooleanExpression(op, ex1, ex2) })
}
