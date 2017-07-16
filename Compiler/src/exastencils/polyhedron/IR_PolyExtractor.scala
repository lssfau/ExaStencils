package exastencils.polyhedron

import scala.collection.mutable.{ ArrayBuffer, ArrayStack, HashSet, ListBuffer, Set, StringBuilder }

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.logger._
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.util.ir.IR_MathFunctions

/** Object for all "static" attributes */
object IR_PolyExtractor {

  /** constants for read/write annotations */
  private final object Access extends Enumeration {
    type Access = Value
    final val ANNOT : String = "PolyAcc"
    final val READ, WRITE, UPDATE = Value

    exastencils.core.Duplicate.registerImmutable(this.getClass)
  }

  /** annotation id used to indicate that this subtree should be skipped */
  private final val SKIP_ANNOT : String = "PolySkip"

  /** set of all functions that are allowed in a scop (these must not have side effects) */
  private final val allowedFunctions = Set[String]("abs", "fabs") ++= IR_MathFunctions.signatures.keys

  /** set of symbolic constants that must not be modeled as read accesses (these must be constant inside a scop) */
  private final val symbolicConstants = HashSet[String]()

  /** Register the name of a side-effect free function, that is safe to be used inside a scop. */
  def registerSideeffectFree(functionName : String) : Unit = {
    allowedFunctions.add(functionName)
  }

  /** Register the name of a symbolic constant, that is not modified inside a scop. */
  def registerSymbolicConstant(constName : String) : Unit = {
    symbolicConstants.add(constName)
  }

  private def extractConstraints(expr : IR_Expression, constraints : StringBuilder, formatString : Boolean, paramExprs : Set[IR_Expression],
      lParConstr : StringBuilder = null, gParConstr : StringBuilder = null, vars : Set[String] = null) : Boolean = {

    var bool : Boolean = false

    expr match {

      case _ if paramExprs.nonEmpty && paramExprs.contains(expr) =>
        val islStr : String = ScopNameMapping.expr2id(expr)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case _ : IR_VariableAccess | _ : IR_ArrayAccess =>
        val islStr : String = ScopNameMapping.expr2id(expr)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      // a StringConstant is only allowed, if the value it represents was used correctly before (as a VariableAccess, for example)
      case str : IR_StringLiteral if ScopNameMapping.id2expr(str.value).isDefined =>
        val e = ScopNameMapping.id2expr(str.value).get
        val islStr : String = ScopNameMapping.expr2id(e)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case IR_IntegerConstant(i) =>
        constraints.append(java.lang.Long.toString(i))

      case b : IR_IV_NeighborIsValid =>
        val islStr : String = ScopNameMapping.expr2id(b)
        // vars and glblParConstr must not be null
        vars.add(islStr)
        gParConstr.append("(0<=").append(islStr).append("<=1)")
        gParConstr.append(" and ")
        constraints.append('(').append(islStr).append("=1)")

      case bExpr @ IR_BoundedScalar(min, max, _ : IR_VariableAccess | _ : IR_ArrayAccess | _ : IR_IV_IterationOffsetBegin | _ : IR_IV_IterationOffsetEnd) =>
        val islStr : String = ScopNameMapping.expr2id(bExpr, bExpr.expr)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)
        if (lParConstr != null) {
          lParConstr.append('(').append(min).append("<=").append(islStr).append("<=").append(max).append(')')
          lParConstr.append(" and ")
        }

      // case OffsetIndex(min, max, ind, off) =>
      //   off match {
      //     case ArrayAccess(_ : iv.IterationOffsetBegin, _, _) =>
      //       off.annotate(SimplifyExpression.EXTREMA_ANNOT, (min.toLong, max.toLong)) // preserve extrema information since OffsetIndex will be lost
      //     case ArrayAccess(_ : iv.IterationOffsetEnd, _, _) =>
      //       off.annotate(SimplifyExpression.EXTREMA_ANNOT, (min.toLong, max.toLong)) // preserve extrema information since OffsetIndex will be lost
      //     case _ => // nothing to do
      //   }
      //   constraints.append('(')
      //   bool |= extractConstraints(ind, constraints, formatString, lParConstr, gParConstr, vars)
      //   constraints.append('+')
      //   bool |= extractConstraints(off, constraints, formatString, lParConstr, gParConstr, vars)
      //   constraints.append(')')
      //   if (lParConstr != null) off match {
      //     case _ : VariableAccess | _ : ArrayAccess =>
      //       lParConstr.append('(').append(min).append("<=")
      //       lParConstr.append(ScopNameMapping.expr2id(off))
      //       lParConstr.append("<=").append(max).append(')')
      //       lParConstr.append(" and ")
      //
      //     case Multiplication(ListBuffer(IntegerConstant(c), arr : ArrayAccess)) =>
      //       lParConstr.append('(').append(min).append("<=").append(c).append('*')
      //       lParConstr.append(ScopNameMapping.expr2id(arr))
      //       lParConstr.append("<=").append(max).append(')')
      //       lParConstr.append(" and ")
      //
      //     case Multiplication(ListBuffer(arr : ArrayAccess, IntegerConstant(c))) =>
      //       lParConstr.append('(').append(min).append("<=").append(c).append('*')
      //       lParConstr.append(ScopNameMapping.expr2id(arr))
      //       lParConstr.append("<=").append(max).append(')')
      //       lParConstr.append(" and ")
      //
      //     case _ =>
      //   }

      case iff : IR_IV_IndexFromField =>
        val islStr : String = ScopNameMapping.expr2id(iff)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)
        if (gParConstr != null) {
          gParConstr.append('(').append(islStr).append(">=0)")
          gParConstr.append(" and ")
        }

      case IR_Addition(sums) =>
        constraints.append('(')
        for (s <- sums) {
          bool |= extractConstraints(s, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append('+')
        }
        constraints(constraints.length - 1) = ')' // replace last '+'

      case IR_Subtraction(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('-')
        bool |= extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')

      case IR_Multiplication(facs) =>
        constraints.append('(')
        for (s <- facs) {
          bool |= extractConstraints(s, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append('*')
        }
        constraints(constraints.length - 1) = ')' // replace last '*'

      case IR_Division(l, r) =>
        constraints.append("floord(")
        bool |= extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(',')
        bool |= extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')

      case IR_Modulo(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('%')
        if (formatString)
          constraints.append('%')
        bool |= extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')

      case IR_Minimum(es) =>
        constraints.append("min(")
        for (e <- es) {
          bool |= extractConstraints(e, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append(',')
        }
        constraints(constraints.length - 1) = ')' // replace last ','

      case IR_Maximum(es) =>
        constraints.append("max(")
        for (e <- es) {
          bool |= extractConstraints(e, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append(',')
        }
        constraints(constraints.length - 1) = ')' // replace last ','

      case IR_Negation(e) =>
        constraints.append("!(")
        extractConstraints(e, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_Lower(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('<')
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_LowerEqual(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append("<=")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_GreaterEqual(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(">=")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_Greater(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('>')
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_EqEq(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('=')
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_Neq(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append("!=")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_AndAnd(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(" and ")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_OrOr(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(" or ")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case _ => throw ExtractionException("unknown expression: " + expr.getClass + " - " + expr.prettyprint())
    }

    bool
  }

  private[polyhedron] def replaceSpecial(str : String) : String = {
    replaceSpecial(new StringBuilder(str)).toString()
  }

  private[polyhedron] def replaceSpecial(str : StringBuilder) : StringBuilder = {

    var i : Int = 0
    while (i < str.length) {
      str(i) match {
        case '.' | '[' | ']' | '(' | ')' | '-' | '>' => str(i) = '_'
        case _                                       =>
      }
      i += 1
    }

    str
  }

  private def checkCode(name : String) : Unit = {

    var i : Int = name.length()
    while (i > 0) {
      i -= 1
      name.charAt(i) match {
        case '=' | '+' | '*' | '/' | '[' | '(' | ' ' =>
          throw ExtractionException("expression in string constant found: " + name)
        case '-' if name.charAt(i + 1) != '>'        =>
          throw ExtractionException("expression in string constant found: " + name)
        case _                                       =>
      }
    }
  }
}

private final case class ExtractionException(msg : String) extends Exception(msg)

class IR_PolyExtractor extends Collector {

  private final val DEBUG : Boolean = false

  /** import all "static" attributes to allow an unqualified access */

  import exastencils.polyhedron.IR_PolyExtractor._

  /** current access node is a read/write access */
  private var isRead, isWrite : Boolean = false

  /** indicates, if the current subtree should be skipped */
  private var skip : Boolean = false

  /** indicates if a new scop starting at the next node can be merged with the last one */
  private var mergeScops : Boolean = false

  /** stack of additional conditions for the next statements found */
  private final val conditions = new ArrayStack[String]()

  /** function to execute after a loop has been processed */
  private final val executeAfterExtraction = new ListBuffer[() => Unit]()

  /** set of (potentially non-affine) expressions that should be treated as a single, new parameter */
  private final val paramExprs = HashSet[IR_Expression]()

  /** all found static control parts */
  final val scops = new ArrayBuffer[Scop](256)
  final val trash = new ArrayBuffer[(Node, String)]

  private object curScop {

    object curStmt {
      private var id_ : Int = -1
      private var label_ : String = null

      def exists() : Boolean = { label_ != null }
      def label() : String = { label_ }
      def id() : Int = { id_ }
      def leave() : Unit = { label_ = null }
      def next() : this.type = {
        id_ += 1
        label_ = "S%04d".format(id_)
        this
      }
    }

    private var scop_ : Scop = null
    private var modelLoopVars_ : String = null
    private var origLoopVars_ : ArrayBuffer[String] = null
    private var setTemplate_ : String = null
    private var mapTemplate_ : String = null

    private final val formatterResult : java.lang.StringBuilder = new java.lang.StringBuilder()
    private final val formatter = new java.util.Formatter(formatterResult)

    def create(root : IR_LoopOverDimensions, localContext : isl.Set,
        globalContext : isl.Set, optLevel : Int, origLoopVars : ArrayBuffer[String],
        modelLoopVars : String, setTempl : String, mapTempl : String, mergeWithPrev : Boolean) : Unit = {

      this.scop_ = new Scop(root, localContext, globalContext, optLevel,
        Knowledge.omp_parallelizeLoopOverDimensions && root.parallelizationIsReasonable, root.maxIterationCount())
      if (mergeWithPrev)
        scops.last.nextMerge = this.scop_
      this.modelLoopVars_ = modelLoopVars
      this.origLoopVars_ = origLoopVars
      this.setTemplate_ = setTempl
      this.mapTemplate_ = mapTempl
    }

    def exists() : Boolean = {
      scop_ != null
    }

    def get() : Scop = {
      scop_
    }

    def modelLoopVars() : String = {
      modelLoopVars_
    }

    def origLoopVars() : ArrayBuffer[String] = {
      origLoopVars_
    }

    // [..] -> { %s[..] : .. %s }
    def buildIslSet(tupleName : String, cond : String = "") : isl.Set = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(setTemplate_, tupleName, cond)
      isl.Set.readFromStr(Isl.ctx, formatterResult.toString)
    }

    // [..] -> { %s[..] -> %s[%s] }
    def buildIslMap(inTupleName : String, outTupleName : String, out : String) : isl.Map = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(mapTemplate_, inTupleName, outTupleName, out)
      try {
        isl.Map.readFromStr(Isl.ctx, formatterResult.toString)
      } catch {
        case e : isl.IslException =>
          throw ExtractionException("error in map creation (maybe not affine?):  " + e.getMessage)
      }
    }

    def finish() : Scop = {
      val res = scop_
      discard()
      res
    }

    def discard(msg : String = null) : Unit = {
      if (msg != null) {
        if (DEBUG)
          Logger.debug("[poly extr] Scop discarded:  " + msg)
        trash += ((if (scop_ != null) scop_.root else null, msg))
      }
      scop_ = null
      modelLoopVars_ = null
      origLoopVars_ = null
      setTemplate_ = null
      mapTemplate_ = null
      curStmt.leave()
    }
  }

  /////////////////// Collector methods \\\\\\\\\\\\\\\\\\\

  override def enter(node : Node) : Unit = {

    val merge : Boolean = mergeScops
    mergeScops = false

    node.getAnnotation(Access.ANNOT) match {
      case Some(acc) =>
        acc match {
          case Access.READ   => isRead = true
          case Access.WRITE  => isWrite = true
          case Access.UPDATE =>
            isRead = true
            isWrite = true
        }
      case None      =>
    }

    if (node.hasAnnotation(SKIP_ANNOT))
      skip = true
    if (skip)
      return

    try {
      if (!curScop.exists())
        node match {
          case loop : IR_LoopOverDimensions =>
            loop.indices.annotate(SKIP_ANNOT)
            loop.stepSize.annotate(SKIP_ANNOT)
            if (loop.condition.isDefined)
              loop.condition.get.annotate(SKIP_ANNOT)
            if (loop.parallelization.reduction.isDefined)
              loop.parallelization.reduction.get.annotate(SKIP_ANNOT)
            // loop.at1stIt is a list of tuple, StateManager does not handle these, so a skip annotation is not required
            if (loop.parallelizationIsReasonable && loop.polyOptLevel >= 1)
              enterLoop(loop, merge)

          case _ =>
        }

      else
        node match {

          // process
          case c : IR_IfCondition =>
            c.condition.annotate(SKIP_ANNOT)
            enterCondition(c)

          case a : IR_Assignment =>
            enterAssign(a)

          case acc : IR_PolyScalarAccessLike =>
            val id = acc.uniqueID
            if (id == null)
              throw new ExtractionException("method uniqueID returned null for object " + acc)
            for (att <- acc.productIterator)
              att match {
                case n : Annotatable => n.annotate(SKIP_ANNOT)
                case _               =>
              }
            enterScalarAccess(id)

          case acc : IR_PolyArrayAccessLike =>
            val id = acc.uniqueID
            if (id == null)
              throw new ExtractionException("method uniqueID returned null for object " + acc)
            for (att <- acc.productIterator)
              att match {
                case n : Annotatable => n.annotate(SKIP_ANNOT)
                case _               =>
              }
            enterArrayAccess(id, acc.index)

          case d : IR_VariableDeclaration =>
            d.datatype.annotate(SKIP_ANNOT)
            enterDecl(d)

          // ignore
          case IR_FunctionCall(function, _) if allowedFunctions.contains(function.name) =>
            function.annotate(SKIP_ANNOT)

          case _ : IR_IntegerConstant
               | _ : IR_RealConstant
               | _ : IR_BooleanConstant
               | _ : IR_Negative
               | _ : IR_Negation
               | _ : IR_AddressOf
               | _ : IR_DerefAccess
               | _ : IR_Addition
               | _ : IR_Subtraction
               | _ : IR_Multiplication
               | _ : IR_Division
               | _ : IR_Modulo
               | _ : IR_Power
               | _ : IR_Minimum
               | _ : IR_Maximum
               | _ : IR_ParallelizationInfo
               | _ : IR_Comment
               | IR_NullStatement => // nothing to do for all of them...

          // deny
          case e : IR_ExpressionStatement => throw ExtractionException("cannot deal with ExprStmt: " + e.prettyprint())
          case f : IR_FunctionCall        => throw ExtractionException("function call not in set of allowed ones: " + f.prettyprint())
          case x : Any                    => throw ExtractionException("cannot deal with " + x.getClass)
        }
    } catch {
      case ExtractionException(msg) =>
        for (exec <- executeAfterExtraction)
          exec()
        executeAfterExtraction.clear()
        conditions.clear()
        curScop.discard(msg)
    }
  }

  override def leave(node : Node) : Unit = {

    if (node.removeAnnotation(Access.ANNOT).isDefined) {
      isRead = false
      isWrite = false
    }

    if (node.removeAnnotation(SKIP_ANNOT).isDefined) {
      skip = false
      return
    }
    if (skip)
      return

    if (curScop.exists())
      node match {
        case l : IR_LoopOverDimensions        => leaveLoop(l)
        case c : IR_IfCondition               => leaveCondition(c)
        case _ : IR_Assignment                => leaveAssign()
        case _ : IR_PolyScalarAccessLike      => leaveScalarAccess()
        case _ : IR_PolyArrayAccessLike       => leaveArrayAccess()
        case _ : IR_VariableDeclaration       => leaveDecl()
        case _                                =>
      }
  }

  override def reset() : Unit = {
    curScop.discard()
    conditions.clear()
    isRead = false
    isWrite = false
    skip = false
    mergeScops = false
    executeAfterExtraction.clear()
    paramExprs.clear()
    scops.clear()
    trash.clear()
  }

  /////////////////// methods for node processing \\\\\\\\\\\\\\\\\\\

  private def enterLoop(loop : IR_LoopOverDimensions, mergeWithPrev : Boolean) : Unit = {

    for (step <- loop.stepSize)
      if (step != IR_IntegerConstant(1))
        throw ExtractionException("only stride 1 supported yet")

    val dims : Int = loop.numDimensions

    val hasOmpLoop : Boolean = loop.explParLoop
    val (begin : IR_ExpressionIndex, end : IR_ExpressionIndex) =
      if (hasOmpLoop)
        (loop.ompIndices.begin, loop.ompIndices.end)
      else
        (loop.indices.begin, loop.indices.end)
    if (hasOmpLoop && !loop.areOmpIndicesAffine)
      paramExprs += begin.last += end.last
    val loopVarExps : IR_ExpressionIndex = IR_LoopOverDimensions.defIt(loop.numDimensions)

    val params = new HashSet[String]()
    val modelLoopVars = new ArrayStack[String]()
    val constrs = new StringBuilder()
    val locCtxConstrs = new StringBuilder()
    val gloCtxConstrs = new StringBuilder()

    val origLoopVars = new ArrayBuffer[String]()

    var bool : Boolean = false
    var i : Int = 0
    do {
      bool |= extractConstraints(begin(i), constrs, true, paramExprs, locCtxConstrs, gloCtxConstrs, params)
      constrs.append("<=")
      constrs.append(ScopNameMapping.expr2id(IR_FieldIteratorAccess(i)))
      constrs.append('<')
      bool |= extractConstraints(end(i), constrs, true, paramExprs, locCtxConstrs, gloCtxConstrs, params)
      constrs.append(" and ")
      val lVar : IR_Expression = loopVarExps(i)
      modelLoopVars.push(ScopNameMapping.expr2id(lVar))
      origLoopVars += lVar.asInstanceOf[IR_VariableAccess].name
      i += 1
    } while (i < dims)

    if (bool)
      throw ExtractionException("loop bounds contain (in)equalities")

    // TODO: interaction betweed condition and at1stIt (see also: TODO in LoopOverDimensions.expandSpecial)
    if (loop.condition.isDefined)
      extractConstraints(loop.condition.get, constrs, true, paramExprs, locCtxConstrs, gloCtxConstrs, params)
    else
      constrs.delete(constrs.length - " and ".length(), constrs.length)

    // remove variables from params set
    for (v <- modelLoopVars)
      params.remove(v)

    val templateBuilder : StringBuilder = new StringBuilder()
    templateBuilder.append('[')
    for (p <- params)
      templateBuilder.append(p).append(',')
    if (params.nonEmpty)
      templateBuilder.deleteCharAt(templateBuilder.length - 1)
    templateBuilder.append("]->{")

    // create local context
    var tmp : Int = templateBuilder.length
    templateBuilder.append(':')
    if (locCtxConstrs.nonEmpty)
      templateBuilder.append(locCtxConstrs.delete(locCtxConstrs.length - " and ".length(), locCtxConstrs.length))
    templateBuilder.append('}')
    val localContext = isl.Set.readFromStr(Isl.ctx, templateBuilder.toString())

    // create global context
    templateBuilder.delete(tmp, templateBuilder.length)
    templateBuilder.append(':')
    if (gloCtxConstrs.nonEmpty)
      templateBuilder.append(gloCtxConstrs.delete(gloCtxConstrs.length - " and ".length(), gloCtxConstrs.length))
    templateBuilder.append('}')
    val globalContext = isl.Set.readFromStr(Isl.ctx, templateBuilder.toString())

    // continue with templates
    templateBuilder.delete(tmp, templateBuilder.length)
    templateBuilder.append("%s[")
    for (v <- modelLoopVars)
      templateBuilder.append(v).append(',')
    templateBuilder.setCharAt(templateBuilder.length - 1, ']')

    // finish for set
    tmp = templateBuilder.length
    templateBuilder.append(':')
    templateBuilder.append(constrs)
    templateBuilder.append("%s") // additional constraints (e.g. from conditions)
    templateBuilder.append('}')
    val setTemplate : String = templateBuilder.toString()

    // remove last and finish for map
    templateBuilder.delete(tmp, templateBuilder.length)
    templateBuilder.append("->%s[%s]}")
    val mapTemplate : String = templateBuilder.toString()

    curScop.create(loop, localContext, globalContext, loop.polyOptLevel, origLoopVars, modelLoopVars.mkString(","), setTemplate, mapTemplate, mergeWithPrev)

    // deal with at1stIt
    val conds = loop.create1stItConds()
    val nrConds = conds.length
    conds ++=: loop.body // prepend to body
    // undo prepend after extraction
    executeAfterExtraction += { () => loop.body.remove(0, nrConds) }
  }

  private def leaveLoop(loop : IR_LoopOverDimensions) : Unit = {
    for (exec <- executeAfterExtraction)
      exec()
    executeAfterExtraction.clear()
    val scop = curScop.finish()
    if (scop != null) {
      scop.updateLoopVars()
      loop.annotate(IR_PolyOpt.SCOP_ANNOT, scop)
      scops += scop
      mergeScops = true
    }
  }

  private def enterCondition(cond : IR_IfCondition) : Unit = {
    if (cond.falseBody.isEmpty) {
      val sb = new StringBuilder(" and ")
      extractConstraints(cond.condition, sb, false, paramExprs)
      conditions.push(sb.toString())
    } else
      throw ExtractionException("cannot deal with a non-empty falseBody in a ConditionStatement: " + cond.prettyprint())
  }

  private def leaveCondition(cond : IR_IfCondition) : Unit = {
    if (cond.falseBody.isEmpty)
      conditions.pop()
  }

  private def enterStmt(stmt : IR_Statement) : Unit = {

    val scop : Scop = curScop.get()

    val label : String = curScop.curStmt.next().label()
    scop.stmts.put(label, (ListBuffer(stmt), curScop.origLoopVars()))

    val domain = curScop.buildIslSet(label, conditions.mkString)
    scop.domain = if (scop.domain == null) domain else scop.domain.addSet(domain)
    val schedule = curScop.buildIslMap(label, "", curScop.modelLoopVars() + ',' + curScop.curStmt.id())
    scop.schedule = if (scop.schedule == null) schedule else scop.schedule.addMap(schedule)
  }

  private def leaveStmt() : Unit = {
    curScop.curStmt.leave()
  }

  private def enterAssign(assign : IR_Assignment) : Unit = {

    enterStmt(assign) // as an assignment is also a statement

    if (isRead || isWrite)
      throw ExtractionException("nested assignments are not supported (yet...?); skipping scop")

    assign.op match {

      case "=" =>
        assign.dest.annotate(Access.ANNOT, Access.WRITE)

      case "+=" | "-=" | "*=" | "/=" =>
        assign.dest.annotate(Access.ANNOT, Access.UPDATE)

      case _ =>
        throw ExtractionException("unrecognized assignment operator: " + assign.op)
    }

    assign.src.annotate(Access.ANNOT, Access.READ)
  }

  private def leaveAssign() : Unit = {
    leaveStmt() // as an assignment is also a statement
  }

  private def enterScalarAccess(varName : String, deadAfterScop : Boolean = false) : Unit = {

    // hack: filter out code
    checkCode(varName)

    if (!curScop.curStmt.exists() || (!isRead && !isWrite))
      throw ExtractionException("misplaced access expression?")

    // is access to loop variable?
    if (curScop.origLoopVars().contains(varName)) {
      if (isWrite)
        throw ExtractionException("write to loop variable found")
      return
    }

    val scop : Scop = curScop.get()

    val access : isl.Map = curScop.buildIslMap(curScop.curStmt.label(), replaceSpecial(varName), "")

    if (isRead)
      scop.reads = if (scop.reads == null) access else scop.reads.addMap(access)
    if (isWrite)
      scop.writes = if (scop.writes == null) access else scop.writes.addMap(access)

    if (deadAfterScop) {
      val dead : isl.Set = access.domain()
      scop.deadAfterScop = if (scop.deadAfterScop == null) dead else scop.deadAfterScop.addSet(dead)
    }
  }

  private def leaveScalarAccess() : Unit = {
    // nothing to do here...
  }

  private def enterArrayAccess(name : String, index : IR_Expression, deadAfterScop : Boolean = false) : Unit = {

    if (!curScop.curStmt.exists() || (!isRead && !isWrite))
      throw ExtractionException("misplaced access expression?")

    // hack: check for code in name
    checkCode(name)

    val scop : Scop = curScop.get()

    var ineq : Boolean = false
    val indB : StringBuilder = new StringBuilder()
    index match {
      case mInd : IR_ExpressionIndex =>
        for (i <- mInd) {
          ineq |= extractConstraints(i, indB, false, paramExprs)
          indB.append(',')
        }
        if (mInd.nonEmpty)
          indB.deleteCharAt(indB.length - 1)
      case ind                       =>
        ineq |= extractConstraints(ind, indB, false, paramExprs)
    }

    if (ineq)
      throw ExtractionException("array access contains (in)equalities")

    val access : isl.Map = curScop.buildIslMap(curScop.curStmt.label(), replaceSpecial(name), indB.toString())
    if (deadAfterScop) {
      val dead : isl.Set = access.domain()
      scop.deadAfterScop = if (scop.deadAfterScop == null) dead else scop.deadAfterScop.addSet(dead)
    }

    if (isRead)
      scop.reads = if (scop.reads == null) access else scop.reads.addMap(access)
    if (isWrite)
      scop.writes = if (scop.writes == null) access else scop.writes.addMap(access)
  }

  private def leaveArrayAccess() : Unit = {
    // nothing to do here...
  }

  private def enterDecl(decl : IR_VariableDeclaration) : Unit = {
    if (isRead || isWrite)
      throw ExtractionException("nested assignments are not supported (yet...?); skipping scop")

    if (decl.initialValue.isDefined) {
      val stmt = IR_Assignment(
        IR_VariableAccess(decl.name, decl.datatype), decl.initialValue.get, "=")
      enterStmt(stmt) // as a declaration is also a statement
      decl.initialValue.get.annotate(Access.ANNOT, Access.READ)
      isWrite = true
      if (decl.datatype.dimensionality > 0)
        throw new ExtractionException("initialization of a local non-scalar variable not supported (yet)")
      else
        enterScalarAccess(decl.name, true)
      isWrite = false
    }

    curScop.get().decls += decl
  }

  private def leaveDecl() : Unit = {

    leaveScalarAccess()
    leaveStmt()
  }
}
