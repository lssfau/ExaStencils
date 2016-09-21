package exastencils.parsers.l4

import scala.collection.immutable.PagedSeq
import scala.collection.mutable.{ Node => _, _ }
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.PagedSeqReader

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.domain.l4.L4_DomainDecl
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldDecl
import exastencils.parsers._
import exastencils.solver.l4._
import exastencils.stencil.l4._

class ParserL4 extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = new LexerL4()

  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : Node = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    prevDirs.push(file.getAbsoluteFile().getParentFile())
    val ret = parseTokens(scanner)
    prevDirs.pop()
    return ret
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
    phrase(program)(tokens) match {
      case Success(e, _)        => e
      case Error(msg, _)        => throw new Exception("parse error: " + msg)
      case Failure(msg, parser) => {
        val sb = new StringBuilder
        sb.append(s"Parse failure at position ${ parser.pos }: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
      }
    }
  }

  //###########################################################

  lazy val program = ((import_ ||| domain ||| layout ||| field ||| stencilField ||| externalField ||| stencil ||| globals ||| function ||| functionTemplate ||| functionInstantiation).+
    ^^ { case d => Root()(d) })

  lazy val import_ = "import" ~> stringLit ^^ { case path => parseFile(path).asInstanceOf[Root] }

  //###########################################################

  lazy val identifierWithOptionalLevel = locationize(ident ~ level.?
    ^^ { case id ~ level => if (level.isDefined) LeveledIdentifier(id, level.get) else BasicIdentifier(id) })

  // ######################################
  // ##### Level Specifications
  // ######################################

  lazy val level = (
    locationize("@" ~> (levelsingle ||| levelall) ^^ { l => l })
      ||| locationize("@" ~ "(" ~> levellist <~ ")" ^^ { l => l }))

  lazy val levellist = locationize(((levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation) <~ ("," ||| "and")).* ~ (levelall ||| levelsingle ||| levelrange ||| levelrelative ||| levelnegation)
    ^^ { case a ~ b => L4_LevelList(a :+ b) })

  lazy val levelsublist = locationize(((levelsingle ||| levelrange ||| levelrelative) <~ ("," ||| "and")).* ~ (levelsingle ||| levelrange ||| levelrelative)
    ^^ { case a ~ b => var x = L4_LevelList(); a.foreach(x.add(_)); x.add(b); x })

  lazy val levelnegation = locationize(("not" ~ "(") ~> levelsublist <~ ")"
    ^^ { L4_NegatedLevelList })

  lazy val levelrange = locationize(((levelsingle ||| "(" ~> levelrelative <~ ")") <~ "to") ~ (levelsingle ||| "(" ~> levelrelative <~ ")")
    ^^ { case b ~ e => L4_LevelRange(b, e) })

  lazy val levelrelative = locationize(levelsingle ~ ("+" ||| "-") ~ integerLit
    ^^ { case l ~ op ~ i => L4_RelativeLevel(op, l, i) })

  lazy val levelall = locationize("all" ^^ { _ => L4_AllLevels })

  lazy val levelsingle = (
    locationize("current" ^^ { _ => L4_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L4_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L4_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L4_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L4_FinestLevel })
      ||| locationize(integerLit ^^ { l => L4_SingleLevel(l) }))

  // ######################################
  // ##### Datatypes
  // ######################################

  lazy val datatype : Parser[L4_Datatype] = (
    simpleDatatype
      ||| algorithmicDatatype
      ||| "Array" ~ ("<" ~> datatype <~ ">") ~ ("<" ~> integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_ArrayDatatype(x, s) })

  lazy val simpleDatatype : Parser[L4_Datatype] = (
    "String" ^^ { _ => L4_StringDatatype }
      ||| ("Boolean" ||| "Bool") ^^ { _ => L4_BooleanDatatype }
      ||| numericDatatype)

  lazy val algorithmicDatatype : Parser[L4_Datatype] = (
    ("Complex" ~ "<") ~> numericDatatype <~ ">" ^^ { x => L4_ComplexDatatype(x) }
      ||| "Vector" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s, None) }
      ||| ("ColumnVector" ||| "CVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s, Some(false)) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ">") ^^ { case x ~ s => L4_VectorDatatype(x, s, Some(true)) }
      ||| "Matrix" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ m ~ n => L4_MatrixDatatype(x, m, n) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ",") ~ (integerLit <~ ">") ^^ { case x ~ m ~ n => L4_MatrixDatatype(x, m, n) }
      ||| numericDatatype)

  lazy val numericDatatype : Parser[L4_Datatype] = (
    ("Integer" ||| "Int") ^^ { _ => L4_IntegerDatatype }
      ||| "Real" ^^ { _ => L4_RealDatatype })

  lazy val returnDatatype = ("Unit" ^^ { _ => L4_UnitDatatype }
    ||| datatype)

  // ######################################
  // ##### Functions
  // ######################################

  lazy val function = locationize("noinline".? ~ (("Func" ||| "Function") ~> identifierWithOptionalLevel) ~ ("(" ~> functionArgumentList.? <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case inline ~ id ~ args ~ t ~ stmts => L4_Function(id, t, args.getOrElse(List[L4_FunctionArgument]()), stmts, inline.isEmpty) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => args :+ arg }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => L4_FunctionArgument(id, t) })

  lazy val functionCallArgumentList = /*locationize*/ ((binaryexpression ||| booleanexpression) <~ ("," | newline)).* ~ (binaryexpression ||| booleanexpression) ^^ { case exps ~ ex => exps :+ ex }
  lazy val functionCall = locationize((flatAccess ||| leveledAccess) ~ "(" ~ functionCallArgumentList.? ~ ")" ^^ { case id ~ "(" ~ args ~ ")" => L4_FunctionCall(id, args.getOrElse(List()).to[ListBuffer]) })

  lazy val functionTemplateArgList = /*locationize*/ (ident <~ ("," | newline)).* ~ ident ^^ { case args ~ arg => args :+ arg }
  lazy val functionInstArgList = /*locationize*/ (functionInstArgument <~ ("," | newline)).* ~ functionInstArgument ^^ { case args ~ arg => args :+ arg }
  lazy val functionInstArgument = binaryexpression ||| booleanexpression
  lazy val functionTemplate = locationize((("FuncTemplate" ||| "FunctionTemplate") ~> ident) ~ ("<" ~> functionTemplateArgList.? <~ ">") ~ ("(" ~> functionArgumentList.? <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case id ~ templateArgs ~ functionArgs ~ retType ~ stmts => L4_FunctionTemplate(id, templateArgs.getOrElse(List()), functionArgs.getOrElse(List()), retType, stmts) })
  lazy val functionInstantiation = locationize(((("Inst" ||| "Instantiate") ~> ident) ~ ("<" ~> functionInstArgList.? <~ ">") ~ ("as" ~> identifierWithOptionalLevel))
    ^^ { case template ~ args ~ target => L4_FunctionInstantiation(template, args.getOrElse(List()), target) })

  // ######################################
  // ##### Statements
  // ######################################

  lazy val statement : Parser[L4_Statement] = (
    variableDeclaration
      ||| valueDeclaration
      ||| repeatNTimes
      ||| contractionLoop
      ||| repeatUntil
      ||| repeatWhile
      ||| loopOver
      ||| loopOverFragments
      ||| assignment
      ||| operatorassignment
      ||| locationize(functionCall ^^ { L4_ExpressionStatement(_) })
      ||| conditional
      ||| applyBCsStatement
      ||| communicateStatement
      ||| returnStatement
      ||| advanceStatement
      ||| leveledScope
      ||| solveLocallyStatement
      ||| colorWithStatement)

  lazy val statementInsideRepeat = statement ||| breakStatement

  lazy val variableDeclaration = (locationize((("Var" ||| "Variable") ~> identifierWithOptionalLevel) ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ dt ~ exp => L4_VariableDeclaration(id, dt, exp) }))

  lazy val valueDeclaration = (locationize((("Val" ||| "Value") ~> identifierWithOptionalLevel) ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ dt ~ exp => L4_ValueDeclaration(id, dt, exp) }))

  lazy val repeatNTimes = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ ("{" ~> statementInsideRepeat.+ <~ "}") ^^ { case n ~ i ~ s => L4_ForLoop(n.toInt, i, s) })
  lazy val contractionLoop = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ contractionClause ~ ("{" ~> statementInsideRepeat.+ <~ "}") ^^ { case n ~ i ~ c ~ s => L4_ContractingLoop(n.toInt, i, c, s) })
  lazy val contractionClause = locationize("with" ~ "contraction" ~> index ~ ("," ~> index).? ^^ { case l ~ r => L4_ContractionSpecification(l, r) })

  lazy val repeatUntil = locationize((("repeat" ~ "until") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.+) <~ "}") ^^ { case c ~ s => L4_UntilLoop(c, s.to[ListBuffer]) })
  lazy val repeatWhile = locationize((("repeat" ~ "while") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.+) <~ "}") ^^ { case c ~ s => L4_WhileLoop(c, s.to[ListBuffer]) })

  lazy val breakStatement = locationize("break" ^^ { case _ => L4_Break() })

  lazy val loopOverFragments = locationize(("loop" ~ "over" ~ "fragments") ~ ("with" ~> reductionClause).? ~ ("{" ~> statement.+ <~ "}") ^^ { case _ ~ red ~ stmts => L4_LoopOverFragments(stmts, red) })
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
      L4_LoopOverField(field, region, seq.isDefined, cond, startOff, endOff, inc, stmts, red, prec, postc)
  })
  lazy val reductionClause = locationize((("reduction" ~ "(") ~> (ident ||| "+" ||| "*")) ~ (":" ~> ident <~ ")") ^^ { case op ~ s => L4_Reduction(op, s) })
  lazy val regionSpecification = locationize((("ghost" ||| "dup" ||| "inner") ~ index ~ ("on" <~ "boundary").?) ^^ { case region ~ dir ~ bc => L4_RegionSpecification(region, dir, bc.isDefined) })

  lazy val assignment = locationize(genericAccess ~ "=" ~ (binaryexpression ||| booleanexpression) ^^ { case id ~ op ~ exp => L4_Assignment(id, exp, op) })
  lazy val operatorassignment = locationize(genericAccess ~ ("+=" ||| "-=" ||| "*=" ||| "/=") ~ binaryexpression
    ^^ { case id ~ op ~ exp => L4_Assignment(id, exp, op) })

  lazy val conditional : PackratParser[L4_IfCondition] = (
    locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ (("else" ~ "{") ~> statement.+ <~ "}").?
      ^^ { case exp ~ stmts ~ elsestmts => L4_IfCondition(exp, stmts.to[ListBuffer], elsestmts.getOrElse(List()).to[ListBuffer]) })
      ||| locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ ("else" ~> conditional)
      ^^ { case exp ~ stmts ~ elsecond => L4_IfCondition(exp, stmts.to[ListBuffer], ListBuffer[L4_Statement](elsecond)) }))

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

  lazy val returnStatement = locationize("return" ~> (binaryexpression ||| booleanexpression).? ^^ { case exp => L4_Return(exp) })

  lazy val leveledScope = locationize((level <~ "{") ~ (statement.+ <~ "}") ^^ { case l ~ s => LeveledScopeStatement(l, s) })

  lazy val equationExpression = locationize((binaryexpression <~ "==") ~ binaryexpression ^^ { case lhs ~ rhs => L4_Equation(lhs, rhs) })
  lazy val solveLocallyComponent = /*locationize*/ ((genericAccess <~ "=>") ~ equationExpression ^^ { case f ~ eq => (f, eq) })
  lazy val solveLocallyStatement = locationize(("solve" ~ "locally" ~ "{") ~> solveLocallyComponent.* <~ "}"
    ^^ { case stmts => L4_LocalSolve(stmts.map(_._1), stmts.map(_._2)) })

  lazy val colorWithStatement = locationize(("color" ~ "with" ~ "{") ~> (booleanexpression <~ ",").+ ~ loopOver <~ "}"
    ^^ { case colors ~ loop => L4_ColorLoops(colors, loop) })

  // ######################################
  // ##### Globals
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> globalEntry.* <~ "}") ^^ { L4_GlobalSection(_) })
  lazy val globalEntry : PackratParser[L4_Statement] = locationize(valueDeclaration ||| variableDeclaration)

  // ######################################
  // ##### Object Declarations
  // ######################################

  lazy val domain = (
    locationize(("Domain" ~> "fromFile" ~> ("(" ~> stringLit <~ ")")) ^^ { case file => { L4_DomainDecl(file, null, null) } })
      ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l ~ u => L4_DomainDecl(id, l, u) })
      ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l1 ~ u1 ~ l2 ~ u2 ~ l3 ~ u3 => L4_DomainDecl(id, List(l1, l2, l3), List(u1, u2, u3)) })
      ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l1 ~ u1 ~ l2 ~ u2 ~ l3 ~ u3 ~ l4 ~ u4 ~ l5 ~ u5 => L4_DomainDecl(id, List(l1, l2, l3, l4, l5), List(u1, u2, u3, u4, u5)) }))

  lazy val discretization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ { case d => d })
  lazy val layout = locationize(("Layout" ~> ident) ~ ("<" ~> datatype <~ ",") ~ (discretization <~ ">") ~ level.? ~ ("{" ~> layoutOptions <~ "}")
    ^^ { case id ~ dt ~ disc ~ level ~ opts => L4_FieldLayoutDecl(LeveledIdentifier(id, level.getOrElse(L4_AllLevels)), dt, disc.toLowerCase, opts) })
  lazy val layoutOptions = (
    (layoutOption <~ ",").* ~ layoutOption ^^ { case opts ~ opt => opts.::(opt) }
      ||| layoutOption.*)
  lazy val layoutOption = locationize((ident <~ "=") ~ index ~ ("with" ~ "communication").?
    ^^ { case id ~ idx ~ comm => L4_FieldLayoutOption(id, idx, comm.isDefined) })

  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> ident) ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ ("[" ~> integerLit <~ "]").? ~ level.?
    ^^ { case id ~ domain ~ layout ~ boundary ~ _ ~ slots ~ level => L4_FieldDecl(LeveledIdentifier(id, level.getOrElse(L4_AllLevels)), domain, layout, boundary, slots.getOrElse(1).toInt) })
  lazy val fieldBoundary = binaryexpression ^^ { case x => Some(x) } ||| "None" ^^ { case x => None }

  lazy val index : PackratParser[L4_ConstIndex] = (
    index1d
      ||| index2d
      ||| index3d)

  lazy val index1d = locationize("[" ~> integerLit <~ "]" ^^ { case n1 => L4_ConstIndex(n1) })
  lazy val index2d = locationize(("[" ~> integerLit <~ ",") ~ (integerLit <~ "]") ^^ { case n1 ~ n2 => L4_ConstIndex(n1, n2) })
  lazy val index3d = locationize(("[" ~> integerLit <~ ",") ~ (integerLit <~ ",") ~ (integerLit <~ "]") ^^ { case n1 ~ n2 ~ n3 => L4_ConstIndex(n1, n2, n3) })

  lazy val realIndex : PackratParser[ConstVec] = (
    locationize("[" ~> realLit <~ "]" ^^ { case n1 => ConstVec1D(n1) })
      ||| locationize(("[" ~> realLit <~ ",") ~ (realLit <~ "]") ^^ { case n1 ~ n2 => ConstVec2D(n1, n2) })
      ||| locationize(("[" ~> realLit <~ ",") ~ (realLit <~ ",") ~ (realLit <~ "]") ^^ { case n1 ~ n2 ~ n3 => ConstVec3D(n1, n2, n3) }))

  lazy val expressionIndex : PackratParser[L4_ExpressionIndex] = (
    locationize("[" ~> binaryexpression <~ "]" ^^ { case n1 => L4_ExpressionIndex(n1) })
      ||| locationize(("[" ~> binaryexpression <~ ",") ~ (binaryexpression <~ "]") ^^ { case n1 ~ n2 => L4_ExpressionIndex(n1, n2) })
      ||| locationize(("[" ~> binaryexpression <~ ",") ~ (binaryexpression <~ ",") ~ (binaryexpression <~ "]") ^^ { case n1 ~ n2 ~ n3 => L4_ExpressionIndex(n1, n2, n3) }))

  lazy val stencil = locationize(("Stencil" ~> identifierWithOptionalLevel) ~ ("{" ~> stencilEntries <~ "}")
    ^^ { case id ~ entries => L4_StencilDecl(id, entries) })
  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilEntry.+)
  lazy val stencilEntry = expressionIndex ~ ("=>" ~> (binaryexpression ||| matrixExpression)) ^^ { case offset ~ weight => L4_StencilEntry(offset, weight) }

  lazy val stencilField = locationize((("StencilField" ~> ident) ~ ("<" ~> ident <~ "=>") ~ (ident <~ ">") ~ level.?)
    ^^ { case id ~ f ~ s ~ level => L4_StencilFieldDecl(LeveledIdentifier(id, level.getOrElse(L4_AllLevels)), f, s) })

  // ######################################
  // ##### "External" Definitions
  // ######################################

  lazy val externalField = locationize((("external" ~ "Field") ~> ident) ~ ("<" ~> ident <~ ">") ~ ("=>" ~> genericAccess)
    ^^ { case extid ~ layout ~ field => L4_ExternalFieldDecl(extid, layout, field) })

  // ######################################
  // ##### Object Access
  // ######################################

  lazy val componentAccess = index1d ||| index2d

  lazy val slotAccess = locationize(
    "$" ~> slotModifier ^^ { case s => s }
      ||| "[" ~> slotModifier <~ "]" ^^ { case s => s })

  lazy val slotModifier = locationize("active" ^^ { case _ => L4_ActiveSlot }
    ||| "activeSlot" ^^ { case _ => L4_ActiveSlot }
    ||| "currentSlot" ^^ { case _ => L4_ActiveSlot }
    ||| "next" ^^ { case _ => L4_NextSlot }
    ||| "nextSlot" ^^ { case _ => L4_NextSlot }
    ||| "previous" ^^ { case _ => L4_PreviousSlot }
    ||| "previousSlot" ^^ { case _ => L4_PreviousSlot }
    ||| integerLit ^^ { case i => L4_ConstantSlot(i) })

  lazy val advanceStatement = locationize("advance" ~> leveledAccess ^^ { case a => L4_AdvanceSlot(a) })

  lazy val levelAccess = (
    locationize("@" ~> levelsingle ^^ { case l => l })
      ||| locationize("@" ~ "(" ~> levelsingle <~ ")" ^^ { case l => l }))

  lazy val flatAccess = locationize(ident
    ^^ { case id => UnresolvedAccess(id, None, None, None, None, None) })
  lazy val leveledAccess = locationize(ident ~ levelAccess
    ^^ { case id ~ level => UnresolvedAccess(id, None, Some(level), None, None, None) })

  lazy val genericAccess = (
    locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> expressionIndex).? ~ ("[" ~> integerLit <~ "]").?
      ^^ { case id ~ slot ~ level ~ offset ~ arrayIndex => UnresolvedAccess(id, slot, level, offset, arrayIndex, None) })
      ||| locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> expressionIndex).? ~ (":" ~> expressionIndex).?
      ^^ { case id ~ slot ~ level ~ offset ~ dirAccess => UnresolvedAccess(id, slot, level, offset, None, dirAccess) }))

  // ######################################
  // ##### Expressions
  // ######################################

  lazy val binaryexpression : PackratParser[L4_Expression] = (
    locationize((binaryexpression ~ ("+" ||| "-" ||| ".+" ||| ".-") ~ term) ^^ { case lhs ~ op ~ rhs => L4_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term)

  lazy val term : PackratParser[L4_Expression] = (
    locationize((term ~ ("*" ||| "/" ||| "%" ||| ".*" ||| "./" ||| ".%") ~ term2) ^^ { case lhs ~ op ~ rhs => L4_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| term2)

  lazy val term2 : PackratParser[L4_Expression] = (
    locationize((term2 ~ ("**" ||| "^" ||| ".**") ~ factor) ^^ { case lhs ~ op ~ rhs => L4_BinaryOperators.createExpression(op, lhs, rhs) })
      ||| factor)

  lazy val factor = (
    "(" ~> binaryexpression <~ ")"
      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { L4_NegativeExpression(_) }
      ||| rowVectorExpression
      ||| columnVectorExpression
      ||| matrixExpression
      ||| locationize(stringLit ^^ { case s => L4_StringConstant(s) })
      ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) L4_IntegerConstant((s.getOrElse("") + n).toInt) else L4_RealConstant((s.getOrElse("") + n).toDouble) })
      ||| locationize("-" ~> functionCall ^^ { L4_NegativeExpression(_) })
      ||| functionCall
      ||| locationize("-" ~> genericAccess ^^ { L4_NegativeExpression(_) })
      ||| genericAccess
      ||| locationize(booleanLit ^^ { case s => L4_BooleanConstant(s) }))

  lazy val rowVectorExpression = locationize("{" ~> (binaryexpression <~ ",").+ ~ (binaryexpression <~ "}") ^^ { case x ~ y => VectorExpression(None, x :+ y, None) })

  lazy val columnVectorExpression = locationize(rowVectorExpression <~ "T" ^^ { case x => VectorExpression(None, x.expressions, Some(false)) })

  lazy val matrixExpression = locationize("{" ~> (rowVectorExpression <~ ",").+ ~ (rowVectorExpression <~ "}") ^^ { case x ~ y => MatrixExpression(None, x :+ y) })

  lazy val booleanexpression : PackratParser[L4_Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[L4_Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[L4_Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ { case ex => L4_UnaryOperators.createExpression("!", ex) })
      ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[L4_Expression] = (
    "(" ~> booleanexpression <~ ")"
      ||| comparison
      ||| binaryexpression)

  lazy val comparison : PackratParser[L4_Expression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
}
