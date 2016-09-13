package exastencils.polyhedron

import scala.collection.mutable.{ ArrayBuffer, ArrayStack, HashSet, ListBuffer, Set, StringBuilder }

import exastencils.base.ir._
import exastencils.core.collectors._
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._

/** Object for all "static" attributes */
object Extractor {

  /** constants for read/write annotations */
  private final object Access extends Enumeration {
    type Access = Value
    final val ANNOT : String = "PolyAcc"
    final val READ, WRITE, UPDATE = Value

    exastencils.core.Duplicate.registerImmutable(this.getClass())
  }

  /** annotation id used to indicate that this subtree should be skipped */
  private final val SKIP_ANNOT : String = "PolySkip"

  /** set of all functions that are allowed in a scop (these must not have side effects) */
  private final val allowedFunctions = Set[String]("abs", "fabs") ++= MathFunctions.signatures.keys

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

      case _ if (!paramExprs.isEmpty && paramExprs.contains(expr)) =>
        val islStr : String = ScopNameMapping.expr2id(expr)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case _ : VariableAccess | _ : ArrayAccess =>
        val islStr : String = ScopNameMapping.expr2id(expr)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      // a StringConstant is only allowed, if the value it represents was used correctly before (as a VariableAccess, for example)
      case str : IR_StringLiteral if (ScopNameMapping.id2expr(str.value).isDefined) =>
        val e = ScopNameMapping.id2expr(str.value).get
        val islStr : String = ScopNameMapping.expr2id(e)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case IR_IntegerConstant(i) =>
        constraints.append(java.lang.Long.toString(i))

      case b : iv.NeighborIsValid =>
        val islStr : String = ScopNameMapping.expr2id(b)
        // vars and glblParConstr must not be null
        vars.add(islStr)
        gParConstr.append("(0<=").append(islStr).append("<=1)")
        gParConstr.append(" and ")
        constraints.append('(').append(islStr).append("=1)")

      case bExpr @ BoundedExpression(min, max, _ : VariableAccess | _ : ArrayAccess) =>
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
      //     case MultiplicationExpression(ListBuffer(IntegerConstant(c), arr : ArrayAccess)) =>
      //       lParConstr.append('(').append(min).append("<=").append(c).append('*')
      //       lParConstr.append(ScopNameMapping.expr2id(arr))
      //       lParConstr.append("<=").append(max).append(')')
      //       lParConstr.append(" and ")
      //
      //     case MultiplicationExpression(ListBuffer(arr : ArrayAccess, IntegerConstant(c))) =>
      //       lParConstr.append('(').append(min).append("<=").append(c).append('*')
      //       lParConstr.append(ScopNameMapping.expr2id(arr))
      //       lParConstr.append("<=").append(max).append(')')
      //       lParConstr.append(" and ")
      //
      //     case _ =>
      //   }

      case iff : iv.IndexFromField =>
        val islStr : String = ScopNameMapping.expr2id(iff)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)
        if (gParConstr != null) {
          gParConstr.append('(').append(islStr).append(">=0)")
          gParConstr.append(" and ")
        }

      case IR_AdditionExpression(sums) =>
        constraints.append('(')
        for (s <- sums) {
          bool |= extractConstraints(s, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append('+')
        }
        constraints(constraints.length - 1) = ')' // replace last '+'

      case IR_SubtractionExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('-')
        bool |= extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')

      case IR_MultiplicationExpression(facs) =>
        constraints.append('(')
        for (s <- facs) {
          bool |= extractConstraints(s, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append('*')
        }
        constraints(constraints.length - 1) = ')' // replace last '*'

      case IR_DivisionExpression(l, r) =>
        constraints.append("floord(")
        bool |= extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(',')
        bool |= extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')

      case IR_ModuloExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('%')
        if (formatString)
          constraints.append('%')
        bool |= extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')

      case IR_MinimumExpression(es) =>
        constraints.append("min(")
        for (e <- es) {
          bool |= extractConstraints(e, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append(',')
        }
        constraints(constraints.length - 1) = ')' // replace last ','

      case IR_MaximumExpression(es) =>
        constraints.append("max(")
        for (e <- es) {
          bool |= extractConstraints(e, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
          constraints.append(',')
        }
        constraints(constraints.length - 1) = ')' // replace last ','

      case IR_NegationExpression(e) =>
        constraints.append("!(")
        extractConstraints(e, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_LowerExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('<')
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_LowerEqualExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append("<=")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_GreaterEqualExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(">=")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_GreaterExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('>')
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_EqEqExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append('=')
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_NeqExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append("!=")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_AndAndExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(" and ")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case IR_OrOrExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(" or ")
        extractConstraints(r, constraints, formatString, paramExprs, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case _ => throw new ExtractionException("unknown expression: " + expr.getClass() + " - " + expr.prettyprint())
    }

    return bool
  }

  private[polyhedron] def replaceSpecial(str : String) : String = {
    return replaceSpecial(new StringBuilder(str)).toString()
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

    return str
  }

  private def checkCode(name : String) : Unit = {

    var i : Int = name.length()
    while (i > 0) {
      i -= 1
      name.charAt(i) match {
        case '=' | '+' | '*' | '/' | '[' | '(' | ' ' =>
          throw new ExtractionException("expression in string constant found: " + name)
        case '-' if name.charAt(i + 1) != '>'        =>
          throw new ExtractionException("expression in string constant found: " + name)
        case _                                       =>
      }
    }
  }
}

private final case class ExtractionException(msg : String) extends Exception(msg)

class Extractor extends Collector {

  private final val DEBUG : Boolean = false

  /** import all "static" attributes to allow an unqualified access */

  import exastencils.polyhedron.Extractor._

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

  // TODO: debug; remove

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
        return this
      }
    }

    private var scop_ : Scop = null
    private var modelLoopVars_ : String = null
    private var origLoopVars_ : ArrayBuffer[String] = null
    private var setTemplate_ : String = null
    private var mapTemplate_ : String = null

    private final val formatterResult : java.lang.StringBuilder = new java.lang.StringBuilder()
    private final val formatter = new java.util.Formatter(formatterResult)

    def create(root : LoopOverDimensions with PolyhedronAccessible, localContext : isl.Set,
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
      return scop_ != null
    }

    def get() : Scop = {
      return scop_
    }

    def modelLoopVars() : String = {
      return modelLoopVars_
    }

    def origLoopVars() : ArrayBuffer[String] = {
      return origLoopVars_
    }

    // [..] -> { %s[..] : .. %s }
    def buildIslSet(tupleName : String, cond : String = "") : isl.Set = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(setTemplate_, tupleName, cond)
      return isl.Set.readFromStr(Isl.ctx, formatterResult.toString())
    }

    // [..] -> { %s[..] -> %s[%s] }
    def buildIslMap(inTupleName : String, outTupleName : String, out : String) : isl.Map = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(mapTemplate_, inTupleName, outTupleName, out)
      try {
        return isl.Map.readFromStr(Isl.ctx, formatterResult.toString())
      } catch {
        case e : isl.IslException =>
          throw new ExtractionException("error in map creation (maybe not affine?):  " + e.getMessage())
      }
    }

    def finish() : Scop = {
      val res = scop_
      discard()
      return res
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
          case loop : LoopOverDimensions with PolyhedronAccessible =>
            loop.indices.annotate(SKIP_ANNOT)
            loop.stepSize.annotate(SKIP_ANNOT)
            if (loop.condition.isDefined)
              loop.condition.get.annotate(SKIP_ANNOT)
            if (loop.reduction.isDefined)
              loop.reduction.get.annotate(SKIP_ANNOT)
            // loop.at1stIt is a list of tuple, StateManager does not handle these, so a skip annotation is not required
            if (loop.parallelizationIsReasonable && loop.optLevel >= 1)
              enterLoop(loop, merge)

          case _ =>
        }

      else
        node match {

          // process
          case c : ConditionStatement =>
            c.condition.annotate(SKIP_ANNOT)
            enterCondition(c)

          case a : AssignmentStatement =>
            enterAssign(a)

          case IR_StringLiteral(varName) =>
            enterScalarAccess(varName)

          case VariableAccess(varName, ty) =>
            if (ty.isDefined)
              ty.get.annotate(SKIP_ANNOT)
            enterScalarAccess(varName)

          case ArrayAccess(array @ IR_StringLiteral(varName), index, _) =>
            array.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterArrayAccess(varName, index)

          case ArrayAccess(array @ VariableAccess(varName, ty), index, _) =>
            if (ty.isDefined)
              ty.get.annotate(SKIP_ANNOT)
            array.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterArrayAccess(varName, index)

          case ArrayAccess(tmp : iv.TmpBuffer, index, _) =>
            tmp.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterArrayAccess(tmp.prettyprint(), index)

          case DirectFieldAccess(fieldSelection, index) =>
            fieldSelection.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterFieldAccess(fieldSelection, index)

          case TempBufferAccess(buffer, index, extent) =>
            buffer.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            extent.annotate(SKIP_ANNOT)
            enterTempBufferAccess(buffer, index)

          case LoopCarriedCSBufferAccess(buffer, index) =>
            buffer.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterLoopCarriedCSBufferAccess(buffer, index)

          case d : VariableDeclarationStatement =>
            d.datatype.annotate(SKIP_ANNOT)
            enterDecl(d)

          // for the following 4 matches: do not distinguish between different elements of
          //    PrimitivePositionBegin and PrimitivePositionEnd (conservative approach)
          case ArrayAccess(ppVec : iv.PrimitivePositionBegin, index, _) =>
            ppVec.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterScalarAccess(replaceSpecial(ppVec.prettyprint()))

          case ArrayAccess(ppVec : iv.PrimitivePositionEnd, index, _) =>
            ppVec.annotate(SKIP_ANNOT)
            index.annotate(SKIP_ANNOT)
            enterScalarAccess(replaceSpecial(ppVec.prettyprint()))

          case MemberAccess(ppVec : iv.PrimitivePositionBegin, _) =>
            ppVec.annotate(SKIP_ANNOT)
            enterScalarAccess(replaceSpecial(ppVec.prettyprint()))

          case MemberAccess(ppVec : iv.PrimitivePositionEnd, _) =>
            ppVec.annotate(SKIP_ANNOT)
            enterScalarAccess(replaceSpecial(ppVec.prettyprint()))

          // ignore
          case FunctionCallExpression(name, _) if (allowedFunctions.contains(name)) =>
          // nothing to do...

          case _ : IR_IntegerConstant
               | _ : IR_RealConstant
               | _ : IR_BooleanConstant
               | _ : IR_NegativeExpression
               | _ : IR_NegationExpression
               | _ : IR_AddressofExpression
               | _ : DerefAccess
               | _ : IR_AdditionExpression
               | _ : IR_SubtractionExpression
               | _ : IR_MultiplicationExpression
               | _ : IR_DivisionExpression
               | _ : IR_ModuloExpression
               | _ : IR_PowerExpression
               | _ : IR_MinimumExpression
               | _ : IR_MaximumExpression
               | _ : CommentStatement
               | IR_NullStatement => // nothing to do for all of them...

          // deny
          case e : IR_ExpressionStatement => throw new ExtractionException("cannot deal with ExprStmt: " + e.prettyprint())
          case ArrayAccess(a, _, _)       => throw new ExtractionException("ArrayAccess to base " + a.getClass() + " not yet implemented")
          case f : FunctionCallExpression => throw new ExtractionException("function call not in set of allowed ones: " + f.prettyprint())
          case x : Any                    => throw new ExtractionException("cannot deal with " + x.getClass())
        }
    } catch {
      case ExtractionException(msg) =>
        for (exec <- executeAfterExtraction)
          exec()
        executeAfterExtraction.clear()
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
        case l : LoopOverDimensions           => leaveLoop(l)
        case c : ConditionStatement           => leaveCondition(c)
        case _ : AssignmentStatement          => leaveAssign()
        case _ : IR_StringLiteral             => leaveScalarAccess()
        case _ : VariableAccess               => leaveScalarAccess()
        case _ : ArrayAccess                  => leaveArrayAccess()
        case _ : DirectFieldAccess            => leaveFieldAccess()
        case _ : TempBufferAccess             => leaveTempBufferAccess()
        case _ : LoopCarriedCSBufferAccess    => leaveLoopCarriedCSBufferAccess()
        case _ : VariableDeclarationStatement => leaveDecl()
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

  private def enterLoop(loop : LoopOverDimensions with PolyhedronAccessible, mergeWithPrev : Boolean) : Unit = {

    for (step <- loop.stepSize)
      if (step != IR_IntegerConstant(1))
        throw new ExtractionException("only stride 1 supported yet")

    val dims : Int = loop.numDimensions

    val hasOmpLoop : Boolean = loop.explParLoop
    val (begin : MultiIndex, end : MultiIndex) =
      if (hasOmpLoop)
        (loop.ompIndices.begin, loop.ompIndices.end)
      else
        (loop.indices.begin, loop.indices.end)
    if (hasOmpLoop && !loop.areOmpIndicesAffine)
      paramExprs += begin.last += end.last
    val loopVarExps : MultiIndex = LoopOverDimensions.defIt(loop.numDimensions)

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
      constrs.append(ScopNameMapping.expr2id(new VariableAccess(dimToString(i), IR_IntegerDatatype)))
      constrs.append('<')
      bool |= extractConstraints(end(i), constrs, true, paramExprs, locCtxConstrs, gloCtxConstrs, params)
      constrs.append(" and ")
      val lVar : IR_Expression = loopVarExps(i)
      modelLoopVars.push(ScopNameMapping.expr2id(lVar))
      origLoopVars += lVar.asInstanceOf[VariableAccess].name
      i += 1
    } while (i < dims)

    if (bool)
      throw new ExtractionException("loop bounds contain (in)equalities")

    // TODO: interaction betweed condition and at1stIt (see also: TODO in LoopOverDimensions.expandSpecial)
    if (loop.condition.isDefined)
      extractConstraints(loop.condition.get, constrs, true, paramExprs, locCtxConstrs, gloCtxConstrs, params)
    else
      constrs.delete(constrs.length - (" and ".length()), constrs.length)

    // remove variables from params set
    for (v <- modelLoopVars)
      params.remove(v)

    val templateBuilder : StringBuilder = new StringBuilder()
    templateBuilder.append('[')
    for (p <- params)
      templateBuilder.append(p).append(',')
    if (!params.isEmpty)
      templateBuilder.deleteCharAt(templateBuilder.length - 1)
    templateBuilder.append("]->{")

    // create local context
    var tmp : Int = templateBuilder.length
    templateBuilder.append(':')
    if (!locCtxConstrs.isEmpty)
      templateBuilder.append(locCtxConstrs.delete(locCtxConstrs.length - (" and ".length()), locCtxConstrs.length))
    templateBuilder.append('}')
    val localContext = isl.Set.readFromStr(Isl.ctx, templateBuilder.toString())

    // create global context
    templateBuilder.delete(tmp, templateBuilder.length)
    templateBuilder.append(':')
    if (!gloCtxConstrs.isEmpty)
      templateBuilder.append(gloCtxConstrs.delete(gloCtxConstrs.length - (" and ".length()), gloCtxConstrs.length))
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

    curScop.create(loop, localContext, globalContext, loop.optLevel, origLoopVars, modelLoopVars.mkString(","), setTemplate, mapTemplate, mergeWithPrev)

    // deal with at1stIt
    val conds = loop.create1stItConds()
    val nrConds = conds.length
    conds ++=: loop.body // prepend to body
    // undo prepend after extraction
    executeAfterExtraction += { () => loop.body.remove(0, nrConds) }
  }

  private def leaveLoop(loop : LoopOverDimensions) : Unit = {
    for (exec <- executeAfterExtraction)
      exec()
    executeAfterExtraction.clear()
    val scop = curScop.finish()
    if (scop != null) {
      scop.updateLoopVars()
      loop.annotate(PolyOpt.SCOP_ANNOT, scop)
      scops += scop
      mergeScops = true
    }
  }

  private def enterCondition(cond : ConditionStatement) : Unit = {
    if (cond.falseBody.isEmpty) {
      val sb = new StringBuilder(" and ")
      extractConstraints(cond.condition, sb, false, paramExprs)
      conditions.push(sb.toString())
    } else
      throw new ExtractionException("cannot deal with a non-empty falseBody in a ConditionStatement: " + cond.prettyprint())
  }

  private def leaveCondition(cond : ConditionStatement) : Unit = {
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

  private def enterAssign(assign : AssignmentStatement) : Unit = {

    enterStmt(assign) // as an assignment is also a statement

    if (isRead || isWrite)
      throw new ExtractionException("nested assignments are not supported (yet...?); skipping scop")

    assign.op match {

      case "=" =>
        assign.dest.annotate(Access.ANNOT, Access.WRITE)

      case "+=" | "-=" | "*=" | "/=" =>
        assign.dest.annotate(Access.ANNOT, Access.UPDATE)

      case _ =>
        throw new ExtractionException("unrecognized assignment operator: " + assign.op)
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
      throw new ExtractionException("misplaced access expression?")

    // is access to loop variable?
    if (curScop.origLoopVars.contains(varName)) {
      if (isWrite)
        throw new ExtractionException("write to loop variable found")
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
      throw new ExtractionException("misplaced access expression?")

    // hack: check for code in name
    checkCode(name)

    val scop : Scop = curScop.get()

    var ineq : Boolean = false
    val indB : StringBuilder = new StringBuilder()
    index match {
      case mInd : MultiIndex =>
        for (i <- mInd) {
          ineq |= extractConstraints(i, indB, false, paramExprs)
          indB.append(',')
        }
        if (!mInd.isEmpty)
          indB.deleteCharAt(indB.length - 1)
      case ind               =>
        ineq |= extractConstraints(ind, indB, false, paramExprs)
    }

    if (ineq)
      throw new ExtractionException("array access contains (in)equalities")

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

  private def enterFieldAccess(fSel : FieldSelection, index : MultiIndex) : Unit = {
    val name = new StringBuilder("field")
    name.append('_').append(fSel.field.identifier).append(fSel.field.index).append('_').append(fSel.field.level)
    name.append("_l").append(fSel.level.prettyprint()).append('a').append(fSel.arrayIndex)
    name.append('_').append(fSel.fragIdx.prettyprint()).append('_')
    fSel.slot match {
      case SlotAccess(_, offset) => name.append('s').append(offset)
      case s                     => name.append(s.prettyprint())
    }
    enterArrayAccess(replaceSpecial(name.toString()), index)
  }

  private def leaveFieldAccess() : Unit = {
    leaveArrayAccess()
  }

  private def enterTempBufferAccess(buffer : iv.TmpBuffer, index : MultiIndex) : Unit = {
    val name = new StringBuilder("buffer")
    name.append('_').append(buffer.direction)
    name.append('_').append(buffer.field.identifier).append(buffer.field.index).append('_').append(buffer.field.level)
    name.append("_n").append(buffer.neighIdx.prettyprint())
    name.append("_f").append(buffer.fragmentIdx.prettyprint())
    enterArrayAccess(name.toString(), index)
  }

  private def leaveTempBufferAccess() : Unit = {
    leaveArrayAccess()
  }

  private def enterLoopCarriedCSBufferAccess(buffer : iv.LoopCarriedCSBuffer, index : MultiIndex) : Unit = {
    enterArrayAccess(buffer.resolveName, index, true)
  }

  private def leaveLoopCarriedCSBufferAccess() : Unit = {
    leaveArrayAccess()
  }

  private def enterDecl(decl : VariableDeclarationStatement) : Unit = {
    if (isRead || isWrite)
      throw new ExtractionException("nested assignments are not supported (yet...?); skipping scop")

    if (decl.expression.isDefined) {
      val stmt = new AssignmentStatement(
        new VariableAccess(decl.name, decl.datatype), decl.expression.get, "=")
      enterStmt(stmt) // as a declaration is also a statement
      decl.expression.get.annotate(Access.ANNOT, Access.READ)
      isWrite = true
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
