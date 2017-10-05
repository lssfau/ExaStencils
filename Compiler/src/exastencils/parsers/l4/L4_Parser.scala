package exastencils.parsers.l4

import scala.collection.immutable.PagedSeq
import scala.collection.mutable._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.PagedSeqReader

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.boundary.l4._
import exastencils.communication.l4._
import exastencils.datastructures._
import exastencils.deprecated.l4._
import exastencils.domain.l4._
import exastencils.field.l4._
import exastencils.interfacing.l4.L4_ExternalFieldDecl
import exastencils.operator.l4._
import exastencils.parsers._
import exastencils.solver.l4._

/// L4_Parser

object L4_Parser extends ExaParser with PackratParsers {
  override val lexical : ExaLexer = new L4_Lexer()

  def parse(s : String) : Node = {
    parseTokens(new lexical.Scanner(s))
  }

  private val prevDirs = new Stack[java.io.File]().push(null)
  def parseFile(filename : String) : L4_Root = {
    val file = new java.io.File(prevDirs.top, filename)
    val lines = scala.io.Source.fromFile(file).getLines
    val reader = new PagedSeqReader(PagedSeq.fromLines(lines))
    val scanner = new lexical.Scanner(reader)

    prevDirs.push(file.getAbsoluteFile.getParentFile)
    val ret = parseTokens(scanner)
    prevDirs.pop()
    ret.asInstanceOf[L4_Root]
  }

  protected def parseTokens(tokens : lexical.Scanner) : Node = {
    phrase(program)(tokens) match {
      case Success(e, _)        => e
      case Error(msg, _)        => throw new Exception("parse error: " + msg)
      case Failure(msg, parser) =>
        val sb = new StringBuilder
        sb.append(s"Parse failure at position ${ parser.pos }: $msg\n")
        sb.append(parser.pos.longString)
        sb.append("\n")
        throw new Exception(sb.toString)
    }
  }

  //###########################################################

  lazy val program = (
    import_
      ||| domain
      ||| layout
      ||| field
      ||| stencilField
      ||| externalField
      ||| stencilDeclaration
      ||| stencilFromDefault
      ||| globals
      ||| function
      ||| functionTemplate
      ||| functionInstantiation
    ).* ^^ { L4_Root(_) }

  lazy val import_ = "import" ~> stringLit ^^ { parseFile }

  //###########################################################

  lazy val identifierWithOptDeclLevel = locationize(ident ~ levelDecl.?
    ^^ { case id ~ level => if (level.isDefined) L4_LeveledIdentifier(id, level.get) else L4_BasicIdentifier(id) })

  // ######################################
  // ##### Level Specifications
  // ######################################

  lazy val levelDecl = locationize("@" ~> (
    directDeclLevel ||| ("(" ~> relativeDeclLevel <~ ")")
      ||| allLevels ||| ("(" ~> (levelDeclRange ||| levelDeclList ||| levelDeclNegList) <~ ")")) ^^ { l => l })

  lazy val levelAccess = locationize("@" ~> (directAccessLevel ||| ("(" ~> relativeAccessLevel <~ ")")))

  lazy val levelDeclGroup = (
    levelDeclRange
      ||| levelDeclList
      ||| allLevels
      ||| singleDeclLevel
    )

  lazy val allLevels = locationize("all" ^^ { _ => L4_AllLevels })

  lazy val levelDeclRange = locationize((singleDeclLevel <~ "to") ~ singleDeclLevel ^^ { case b ~ e => L4_LevelRange(b, e) })

  lazy val levelDeclList : Parser[L4_DeclarationLevelSpecification] = (
    locationize((singleDeclLevel <~ ("," ||| "and")).+ ~ singleDeclLevel ^^ { case a ~ b => L4_LevelList(a :+ b) })
      ||| locationize("(" ~> levelDeclList <~ ")") ^^ { l => l })

  lazy val levelDeclNegList : Parser[L4_LevelList] = (
    locationize((levelDeclGroup <~ ("but" ||| "not")) ~ levelDeclGroup ^^ { case in ~ out => L4_LevelList(List(in, L4_NegatedLevelList(out))) })
      ||| locationize("(" ~> levelDeclNegList <~ ")") ^^ { l => l })

  lazy val singleAccessLevel : Parser[L4_AccessLevelSpecification] = (
    directAccessLevel
      ||| relativeAccessLevel
      ||| locationize("(" ~> singleAccessLevel <~ ")") ^^ { l => l })

  lazy val singleDeclLevel : Parser[L4_DeclarationLevelSpecification] = (
    directDeclLevel
      ||| relativeDeclLevel
      ||| locationize("(" ~> singleDeclLevel <~ ")") ^^ { l => l })

  lazy val relativeAccessLevel = locationize(directAccessLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L4_RelativeLevel(l, op, i) })
  lazy val relativeDeclLevel = locationize(directDeclLevel ~ ("+" ||| "-") ~ integerLit ^^ { case l ~ op ~ i => L4_RelativeLevel(l, op, i) })

  lazy val directAccessLevel : Parser[L4_AccessLevelSpecification] = (
    locationize("current" ^^ { _ => L4_CurrentLevel })
      ||| locationize("coarser" ^^ { _ => L4_CoarserLevel })
      ||| locationize("finer" ^^ { _ => L4_FinerLevel })
      ||| locationize("coarsest" ^^ { _ => L4_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L4_FinestLevel })
      ||| locationize(integerLit ^^ { l => L4_SingleLevel(l) })
      ||| locationize("(" ~> directAccessLevel <~ ")" ^^ { l => l }))

  lazy val directDeclLevel : Parser[L4_DeclarationLevelSpecification] = (
    locationize("coarsest" ^^ { _ => L4_CoarsestLevel })
      ||| locationize("finest" ^^ { _ => L4_FinestLevel })
      ||| locationize(integerLit ^^ { l => L4_SingleLevel(l) })
      ||| locationize("(" ~> directDeclLevel <~ ")" ^^ { l => l }))

  // ######################################
  // ##### Datatypes
  // ######################################

  lazy val datatype : Parser[L4_Datatype] = simpleDatatype ||| algorithmicDatatype

  lazy val simpleDatatype : Parser[L4_Datatype] = (
    "String" ^^ { _ => L4_StringDatatype }
      ||| ("Boolean" ||| "Bool") ^^ { _ => L4_BooleanDatatype }
      ||| numericDatatype)

  lazy val algorithmicDatatype : Parser[L4_Datatype] = (
    ("Complex" ~ "<") ~> numericDatatype <~ ">" ^^ { x => L4_ComplexDatatype(x) }
      ||| "Vector" ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s) }
      ||| ("RowVector" ||| "RVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s, true) }
      ||| ("ColumnVector" ||| "CVector") ~ ("<" ~> numericDatatype <~ ",") ~ (integerLit <~ ">") ^^ { case _ ~ x ~ s => L4_VectorDatatype(x, s, false) }
      ||| numericDatatype ~ ("<" ~> integerLit <~ ">") ^^ { case x ~ s => L4_VectorDatatype(x, s) }
      ||| "Vec2" ^^ { _ => L4_VectorDatatype(L4_RealDatatype, 2) }
      ||| "Vec3" ^^ { _ => L4_VectorDatatype(L4_RealDatatype, 3) }
      ||| "Vec4" ^^ { _ => L4_VectorDatatype(L4_RealDatatype, 4) }
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

  lazy val function = locationize("noinline".? ~ (("Func" ||| "Function") ~> ident) ~ levelDecl.? ~ ("(" ~> functionArgumentList.? <~ ")").? ~ (":" ~> returnDatatype).? ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case inline ~ id ~ level ~ args ~ t ~ stmts => L4_FunctionDecl(id, level, t, args, stmts, inline.isEmpty) })
  lazy val functionArgumentList = (functionArgument <~ ("," | newline)).* ~ functionArgument ^^ { case args ~ arg => args :+ arg }
  lazy val functionArgument = locationize(((ident <~ ":") ~ datatype) ^^ { case id ~ t => L4_Function.Argument(id, t) })

  lazy val functionReference = locationize(ident ~ levelAccess.? ~ ("@" ~> constIndex).? ^^ { case id ~ level ~ offset => L4_UnresolvedFunctionReference(id, level, offset) })
  lazy val functionCallArgumentList = /*locationize*/ ((binaryexpression ||| booleanexpression) <~ ("," | newline)).* ~ (binaryexpression ||| booleanexpression) ^^ { case exps ~ ex => exps :+ ex }
  lazy val functionCall = locationize(functionReference ~ ("(" ~> functionCallArgumentList.? <~ ")")
    ^^ { case id ~ args => L4_FunctionCall(id, args.getOrElse(List()).to[ListBuffer]) })

  lazy val functionTemplateArgList = /*locationize*/ (ident <~ ("," | newline)).* ~ ident ^^ { case args ~ arg => args :+ arg }
  lazy val functionInstArgList = /*locationize*/ (functionInstArgument <~ ("," | newline)).* ~ functionInstArgument ^^ { case args ~ arg => args :+ arg }
  lazy val functionInstArgument = binaryexpression ||| booleanexpression
  lazy val functionTemplate = locationize((("FuncTemplate" ||| "FunctionTemplate") ~> ident) ~ ("<" ~> functionTemplateArgList.? <~ ">") ~ ("(" ~> functionArgumentList.? <~ ")") ~ (":" ~> returnDatatype) ~ ("{" ~> (statement.* <~ "}"))
    ^^ { case id ~ templateArgs ~ functionArgs ~ retType ~ stmts => L4_FunctionTemplate(id, retType, templateArgs.getOrElse(List()), functionArgs.getOrElse(List()), stmts) })
  lazy val functionInstantiation = locationize(((("Inst" ||| "Instantiate") ~> ident) ~ ("<" ~> functionInstArgList.? <~ ">") ~ ("as" ~> ident) ~ levelDecl.?)
    ^^ { case template ~ args ~ target ~ targetLvl => L4_FunctionInstantiation(template, args.getOrElse(List()), target, targetLvl) })

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
      ||| levelScope
      ||| solveLocallyStatement
      ||| colorWithStatement)

  lazy val statementInsideRepeat = statement ||| breakStatement

  lazy val variableDeclaration = locationize((("Var" ||| "Variable") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression)).?
    ^^ { case id ~ levels ~ dt ~ exp => L4_VariableDeclaration(id, levels, dt, exp, false) })

  lazy val valueDeclaration = locationize((("Val" ||| "Value") ~> ident) ~ levelDecl.? ~ (":" ~> datatype) ~ ("=" ~> (binaryexpression ||| booleanexpression))
    ^^ { case id ~ levels ~ dt ~ exp => L4_VariableDeclaration(id, levels, dt, Some(exp), true) })

  lazy val repeatNTimes = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ ("{" ~> statementInsideRepeat.+ <~ "}") ^^ { case n ~ i ~ s => L4_ForLoop(n.toInt, i, s) })
  lazy val contractionLoop = locationize(("repeat" ~> numericLit <~ "times") ~ ("count" ~> (flatAccess ||| leveledAccess)).? ~ contractionClause ~ ("{" ~> statementInsideRepeat.+ <~ "}") ^^ { case n ~ i ~ c ~ s => L4_ContractingLoop(n.toInt, i, c, s) })
  lazy val contractionClause = locationize("with" ~ "contraction" ~> constIndex ~ ("," ~> constIndex).? ^^ { case l ~ r => L4_ContractionSpecification(l, r) })

  lazy val repeatUntil = locationize((("repeat" ~ "until") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.+) <~ "}") ^^ { case c ~ s => L4_UntilLoop(c, s.to[ListBuffer]) })
  lazy val repeatWhile = locationize((("repeat" ~ "while") ~> booleanexpression) ~ (("{" ~> statementInsideRepeat.+) <~ "}") ^^ { case c ~ s => L4_WhileLoop(c, s.to[ListBuffer]) })

  lazy val breakStatement = locationize("break" ^^ (_ => L4_Break()))

  lazy val loopOverFragments = locationize(("loop" ~ "over" ~ "fragments") ~ ("with" ~> reductionClause).? ~ ("{" ~> statement.+ <~ "}") ^^ { case _ ~ red ~ stmts => L4_LoopOverFragments(stmts, red) })
  lazy val loopOver = locationize(("loop" ~ "over" ~> genericAccess) ~ //fieldAccess
    ("only" ~> regionSpecification).? ~
    "sequentially".? ~ // FIXME: seq HACK
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
  lazy val regionSpecification = locationize((("ghost" ||| "dup" ||| "inner") ~ constIndex ~ ("on" <~ "boundary").?) ^^ { case region ~ dir ~ bc => L4_RegionSpecification(region, dir, bc.isDefined) })

  lazy val assignment = locationize(genericAccess ~ "=" ~ (binaryexpression ||| booleanexpression) ^^ { case id ~ op ~ exp => L4_Assignment(id, exp, op) })
  lazy val operatorassignment = locationize(genericAccess ~ ("+=" ||| "-=" ||| "*=" ||| "/=") ~ binaryexpression
    ^^ { case id ~ op ~ exp => L4_Assignment(id, exp, op) })

  lazy val conditional : PackratParser[L4_IfCondition] = (
    locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ (("else" ~ "{") ~> statement.+ <~ "}").?
      ^^ { case exp ~ stmts ~ elsestmts => L4_IfCondition(exp, stmts.to[ListBuffer], elsestmts.getOrElse(List()).to[ListBuffer]) })
      ||| locationize(("if" ~ "(" ~> booleanexpression <~ ")") ~ ("{" ~> statement.+ <~ "}") ~ ("else" ~> conditional)
      ^^ { case exp ~ stmts ~ elsecond => L4_IfCondition(exp, stmts.to[ListBuffer], ListBuffer[L4_Statement](elsecond)) }))

  lazy val applyBCsStatement = locationize(("apply" ~ "bc" ~ "to") ~> genericAccess //fieldAccess
    ^^ (field => L4_ApplyBC(field)))
  lazy val communicateStatement = locationize((("begin" ||| "finish").? <~ ("communicate" ||| "communicating")) ~ communicateTarget.* ~ ("of".? ~> genericAccess) //fieldAccess
    ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => L4_Communicate(field, op.getOrElse("both"), targets, cond) })
  lazy val communicateTarget = locationize(("all" ||| "dup" ||| "ghost") ~ constIndex.? ~ ("to" ~> constIndex).? // inclusive indices
    ^^ { case target ~ start ~ end => L4_CommunicateTarget(target, start, end) })
  lazy val precomm = locationize("precomm" ~> ("begin" ||| "finish").? ~ communicateTarget.* ~ ("of".? ~> genericAccess) ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => L4_Communicate(field, op.getOrElse("both"), targets, cond) })
  lazy val postcomm = locationize("postcomm" ~> ("begin" ||| "finish").? ~ communicateTarget.* ~ ("of".? ~> genericAccess) ~ ("where" ~> booleanexpression).?
    ^^ { case op ~ targets ~ field ~ cond => L4_Communicate(field, op.getOrElse("both"), targets, cond) })

  lazy val returnStatement = locationize("return" ~> (binaryexpression ||| booleanexpression).? ^^ (exp => L4_Return(exp)))

  lazy val levelScope = locationize(((levelDecl ||| levelAccess) <~ "{") ~ (statement.+ <~ "}") ^^ { case l ~ s => L4_LevelScope(l, s) })

  lazy val equationExpression = locationize((binaryexpression <~ "==") ~ binaryexpression ^^ { case lhs ~ rhs => L4_Equation(lhs, rhs) })
  lazy val solveLocallyComponent = /*locationize*/ (genericAccess <~ "=>") ~ equationExpression ^^ { case f ~ eq => (f, eq) }
  lazy val solveLocallyStatement = locationize((("solve" ~ "locally") ~> ("with" ~> "jacobi").? ~ ("relax" ~> binaryexpression).? <~ "{") ~ solveLocallyComponent.* <~ "}"
    ^^ { case jac ~ relax ~ stmts => L4_LocalSolve(stmts.map(_._1), stmts.map(_._2), jac.isDefined, relax) })

  lazy val colorWithStatement = locationize(("color" ~ "with" ~ "{") ~> (booleanexpression <~ ",").+ ~ statement.* <~ "}"
    ^^ { case colors ~ stmts => L4_ColorLoops(colors, stmts) })

  // ######################################
  // ##### Globals
  // ######################################

  lazy val globals = locationize(("Globals" ~> "{" ~> globalEntry.* <~ "}") ^^ { L4_GlobalSection(_) })
  lazy val globalEntry : PackratParser[L4_VariableDeclaration] = locationize(valueDeclaration ||| variableDeclaration)

  // ######################################
  // ##### Object Declarations
  // ######################################

  lazy val domain = (
    locationize(("Domain" ~> "fromFile" ~> ("(" ~> stringLit <~ ")")) ^^ (file => L4_HACK_DomainDecl(file, null, null)))
      ||| locationize(("Domain" ~> ident) ~ ("<" ~> expressionIndex <~ "to") ~ (expressionIndex <~ ">") ^^ { case id ~ l ~ u => L4_DomainFromAABBDecl(id, l, u) })
      ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l1 ~ u1 ~ l2 ~ u2 ~ l3 ~ u3 => L4_HACK_DomainDecl(id, List(l1, l2, l3), List(u1, u2, u3)) })
      ||| locationize(("Domain" ~> ident) ~ ("<" ~> realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ",") ~ (realIndex <~ "to") ~ (realIndex <~ ">") ^^ { case id ~ l1 ~ u1 ~ l2 ~ u2 ~ l3 ~ u3 ~ l4 ~ u4 ~ l5 ~ u5 => L4_HACK_DomainDecl(id, List(l1, l2, l3, l4, l5), List(u1, u2, u3, u4, u5)) }))

  lazy val layout = locationize(("Layout" ~> ident) ~ ("<" ~> datatype <~ ",") ~ (localization <~ ">") ~ levelDecl.? ~ ("{" ~> layoutOptions <~ "}")
    ^^ { case id ~ dt ~ disc ~ level ~ opts => L4_FieldLayoutDecl(id, level, dt, disc.toLowerCase, opts) })
  lazy val layoutOptions = (
    (layoutOption <~ ",").* ~ layoutOption ^^ { case opts ~ opt => opts.::(opt) }
      ||| layoutOption.*)
  lazy val layoutOption = locationize((ident <~ "=") ~ constIndex ~ ("with" ~ "communication").?
    ^^ { case id ~ idx ~ comm => L4_FieldLayoutOption(id, idx, comm.isDefined) })

  lazy val field = locationize(("Field" ~> ident) ~ ("<" ~> ident) ~ ("," ~> ident) ~ ("," ~> fieldBoundary) ~ ">" ~ ("[" ~> integerLit <~ "]").? ~ levelDecl.?
    ^^ { case id ~ domain ~ layout ~ boundary ~ _ ~ slots ~ level => L4_BaseFieldDecl(id, level, domain, layout, boundary, slots.getOrElse(1).toInt) })
  lazy val fieldBoundary = (
    "Neumann" ~> ("(" ~> integerLit <~ ")").? ^^ { L4_NeumannBC(_) }
      ||| "None" ^^ { _ => L4_NoBC }
      ||| binaryexpression ^^ (L4_DirichletBC(_))
    )

  lazy val realIndex : PackratParser[L4_ConstVec] = (
    locationize("[" ~> realLit <~ "]" ^^ { n1 => L4_ConstVec1D(n1) })
      ||| locationize(("[" ~> realLit <~ ",") ~ (realLit <~ "]") ^^ { case n1 ~ n2 => L4_ConstVec2D(n1, n2) })
      ||| locationize(("[" ~> realLit <~ ",") ~ (realLit <~ ",") ~ (realLit <~ "]") ^^ { case n1 ~ n2 ~ n3 => L4_ConstVec3D(n1, n2, n3) }))

  lazy val rangeIndex1d = locationize(("[" ~> binaryexpression.? <~ ":") ~ (binaryexpression.? <~ "]") ^^ { case x ~ y => L4_RangeIndex(L4_Range(x, y)) })
  lazy val rangeIndex2d = locationize("[" ~> binaryexpression.? ~ ":" ~ binaryexpression.? ~ "," ~ binaryexpression.? ~ ":" ~ binaryexpression.? <~ "]" ^^ {
    case a ~ _ ~ b ~ _ ~ x ~ _ ~ y => L4_RangeIndex(Array(L4_Range(a, b), L4_Range(x, y)))
  })

  lazy val stencilField = locationize((("StencilField" ~> ident) ~ ("<" ~> ident <~ "=>") ~ (ident <~ ">") ~ levelDecl.?)
    ^^ { case id ~ f ~ s ~ level => L4_StencilFieldDecl(id, level, s, f) })

  // ######################################
  // ##### "External" Definitions
  // ######################################

  lazy val externalField = locationize((("external" ~ "Field") ~> ident) ~ ("<" ~> ident <~ ">") ~ ("=>" ~> genericAccess)
    ^^ { case extid ~ layout ~ field => L4_ExternalFieldDecl(extid, layout, field) })

  // ######################################
  // ##### Object Access
  // ######################################

  lazy val slotAccess = locationize(
    "$" ~> slotModifier ^^ (s => s)
      ||| "[" ~> slotModifier <~ "]" ^^ (s => s))

  lazy val slotModifier = locationize("active" ^^ (_ => L4_ActiveSlot)
    ||| "activeSlot" ^^ (_ => L4_ActiveSlot)
    ||| "currentSlot" ^^ (_ => L4_ActiveSlot)
    ||| "next" ^^ (_ => L4_NextSlot)
    ||| "nextSlot" ^^ (_ => L4_NextSlot)
    ||| "previous" ^^ (_ => L4_PreviousSlot)
    ||| "previousSlot" ^^ (_ => L4_PreviousSlot)
    ||| integerLit ^^ (i => L4_ConstantSlot(i)))

  lazy val advanceStatement = locationize("advance" ~> genericAccess ^^ (a => L4_AdvanceSlot(a)))

  lazy val flatAccess = locationize(ident
    ^^ (id => L4_UnresolvedAccess(id)))
  lazy val leveledAccess = locationize(ident ~ levelAccess
    ^^ { case id ~ level => L4_UnresolvedAccess(id, Some(level)) })

  lazy val genericAccess = (
    locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> constIndex).? ~ ("[" ~> integerLit <~ "]").?
      ^^ { case id ~ slot ~ level ~ offset ~ arrayIndex => L4_UnresolvedAccess(id, level, slot, offset, None, arrayIndex) })
      ||| locationize(ident ~ slotAccess.? ~ levelAccess.? ~ ("@" ~> constIndex).? ~ (":" ~> constIndex).?
      ^^ { case id ~ slot ~ level ~ offset ~ dirAccess => L4_UnresolvedAccess(id, level, slot, offset, dirAccess, None) })) // component acccess mit spitzen klammern

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
      ||| ("-" ~ "(") ~> binaryexpression <~ ")" ^^ { L4_Negative }
      ||| rowVectorExpression
      ||| columnVectorExpression
      ||| matrixExpression
      ||| locationize("-" ~> matrixExpression ^^ { L4_Negative(_) })
      ||| locationize(stringLit ^^ (s => L4_StringConstant(s)))
      ||| locationize("-".? ~ numericLit ^^ { case s ~ n => if (isInt(s.getOrElse("") + n)) L4_IntegerConstant((s.getOrElse("") + n).toInt) else L4_RealConstant((s.getOrElse("") + n).toDouble) })
      ||| locationize("-" ~> functionCall ^^ { L4_Negative(_) })
      ||| functionCall
      ||| locationize("-" ~> genericAccess ^^ { L4_Negative(_) })
      ||| genericAccess
      ||| fieldIteratorAccess
      ||| locationize(booleanLit ^^ (s => L4_BooleanConstant(s))))

  lazy val rowVectorExpression = locationize("{" ~> (binaryexpression <~ ",").* ~ (binaryexpression <~ "}") ^^ { case x ~ y => L4_VectorExpression(None, x :+ y, true) }
    ||| "[" ~> binaryexpression.+ <~ "]" ^^ { case x => L4_VectorExpression(None, x, true) }) // Alternative version

  lazy val columnVectorExpression = locationize(rowVectorExpression <~ "T" ^^ (x => L4_VectorExpression(None, x.expressions, false)) |||
    "[" ~> (binaryexpression <~ ";").* ~ binaryexpression <~ "]" ^^ { case x ~ y => L4_VectorExpression(None, x :+ y, false) })

  lazy val matrixExpression = locationize("{" ~> (rowVectorExpression <~ ",").* ~ (rowVectorExpression <~ "}") ^^ { case x ~ y => L4_MatrixExpression(None, (x :+ y).map(_.expressions.toList)) } |||
    ("[" ~> (binaryexpression.+ <~ ";").*) ~ (binaryexpression.+ <~ "]") ^^ { case x ~ y => L4_MatrixExpression(None, x :+ y) })

  lazy val booleanexpression : PackratParser[L4_Expression] = (
    locationize((booleanexpression ~ ("||" ||| "or") ~ booleanexpression1) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression1)

  lazy val booleanexpression1 : PackratParser[L4_Expression] = (
    locationize((booleanexpression1 ~ ("&&" ||| "and") ~ booleanexpression2) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })
      ||| booleanexpression2)

  lazy val booleanexpression2 : PackratParser[L4_Expression] = (
    locationize(("!" ~> booleanexpression3) ^^ (ex => L4_UnaryOperators.createExpression("!", ex)))
      ||| booleanexpression3)

  lazy val booleanexpression3 : PackratParser[L4_Expression] = (
    "(" ~> booleanexpression <~ ")"
      ||| comparison
      ||| binaryexpression)

  lazy val comparison : PackratParser[L4_Expression] = //(
    locationize((binaryexpression ~ ("<" ||| "<=" ||| ">" ||| ">=" ||| "==" ||| "!=") ~ binaryexpression) ^^ { case ex1 ~ op ~ ex2 => L4_BinaryOperators.createExpression(op, ex1, ex2) })

  // #############################################################################
  // #################################### BASE ###################################
  // #############################################################################

  // ######################################
  // ##### L4_Index
  // ######################################

  lazy val index = expressionIndex ||| constIndex

  lazy val expressionIndex = locationize("[" ~> binaryexpression ~ ("," ~> binaryexpression).* <~ "]" ^^ { case b ~ l => L4_ExpressionIndex((List(b) ++ l).toArray) })
  lazy val constIndex = locationize("[" ~> integerLit ~ ("," ~> integerLit).* <~ "]" ^^ { case b ~ l => L4_ConstIndex((List(b) ++ l).toArray) })

  // #############################################################################
  // ################################## BASE_EXT #################################
  // #############################################################################

  // ######################################
  // ##### L4_FieldIteratorAccess
  // ######################################

  lazy val fieldIteratorAccess = (
    locationize(("i0" | "i1" | "i2") ^^ { id => L4_FieldIteratorAccess(id) })
      ||| locationize(("x" | "y" | "z") ^^ { id => L4_FieldIteratorAccess(id) })
    )

  // #############################################################################
  // #################################### FIELD ##################################
  // #############################################################################

  // ######################################
  // ##### L4_FieldDecl
  // ######################################

  lazy val localization = ("Node" ||| "node" ||| "Cell" ||| "cell"
    ||| "Face_x" ||| "face_x" ||| "Face_y" ||| "face_y" ||| "Face_z" ||| "face_z"
    ||| "Edge_Node" ||| "edge_node" ||| "Edge_Cell" ||| "edge_cell"
    ^^ (l => l))

  // #############################################################################
  // ################################## OPERATOR #################################
  // #############################################################################

  // ######################################
  // ##### L4_StencilDecl
  // ######################################

  lazy val stencilDeclaration = (
    locationize(("Stencil" ~> ident) ~ levelDecl.? ~ ("{" ~> stencilEntries <~ "}")
      ^^ { case id ~ levels ~ entries => L4_BaseStencilDecl(id, levels, entries) })
      ||| locationize(("Stencil" ~> ident) ~ levelDecl.? ~ ("from" ~> binaryexpression)
      ^^ { case id ~ levels ~ expr => L4_StencilFromExpression(id, levels, expr) }))

  lazy val stencilEntries = (
    (stencilEntry <~ ",").+ ~ stencilEntry ^^ { case entries ~ entry => entries.::(entry) }
      ||| stencilEntry.+)

  lazy val stencilEntry = (
    locationize((constIndex ~ ("=>" ~> binaryexpression)) ^^ { case offset ~ coeff => L4_StencilOffsetEntry(offset, coeff) })
      ||| locationize(((expressionIndex <~ "from") ~ expressionIndex ~ ("with" ~> binaryexpression)) ^^ { case row ~ col ~ coeff => L4_StencilMappingEntry(row, col, coeff) }))

  lazy val stencilFromDefault = (
    locationize(("Stencil" ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "restriction" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L4_DefaultRestrictionStencil(id, level, local, interpolation) })
      ||| locationize(("Stencil" ~> ident) ~ levelDecl.? ~ (("from" ~ "default" ~ "prolongation" ~ "on") ~> localization) ~ ("with" ~> stringLit)
      ^^ { case id ~ level ~ local ~ interpolation => L4_DefaultProlongationStencil(id, level, local, interpolation) }))
}
