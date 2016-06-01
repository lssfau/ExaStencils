package exastencils.polyhedron

import exastencils.core.collectors._
import exastencils.cuda._
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.util._

import scala.collection.mutable.{ArrayBuffer, ArrayStack, HashSet, ListBuffer, Set, StringBuilder}

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

  /**
   * Search for IterationOffsetBegin/IterationOffsetEnd accesses and annotate extrema information of OffsetIndex.
   *
   * @param minOffset the minimum
   * @param maxOffset the maximum
   * @param off the offset expression that should be traversed
   */
  private def annotateOffsetExpression(minOffset : Long, maxOffset : Long, off : Expression) : Unit = {
    off match {
      case ArrayAccess(_ : iv.IterationOffsetBegin, _, _) | ArrayAccess(_ : iv.IterationOffsetEnd, _, _) =>
        off.annotate(SimplifyExpression.EXTREMA_ANNOT, (minOffset, maxOffset))
      case AdditionExpression(summands : ListBuffer[Expression]) =>
        summands.foreach(o => annotateOffsetExpression(minOffset, maxOffset, o))
      case SubtractionExpression(left : Expression, right : Expression) =>
        annotateOffsetExpression(minOffset, maxOffset, left)
        annotateOffsetExpression(minOffset, maxOffset, right)
      case MultiplicationExpression(factors : ListBuffer[Expression]) =>
        factors.foreach(o => annotateOffsetExpression(minOffset, maxOffset, o))
      case DivisionExpression(left : Expression, right : Expression) =>
        annotateOffsetExpression(minOffset, maxOffset, left)
        annotateOffsetExpression(minOffset, maxOffset, right)
      case ModuloExpression(left : Expression, right : Expression) =>
        annotateOffsetExpression(minOffset, maxOffset, left)
        annotateOffsetExpression(minOffset, maxOffset, right)
      case PowerExpression(left : Expression, right : Expression) =>
        annotateOffsetExpression(minOffset, maxOffset, left)
        annotateOffsetExpression(minOffset, maxOffset, right)
      case NegativeExpression(left : Expression) =>
        annotateOffsetExpression(minOffset, maxOffset, left)
      case MaximumExpression(args : ListBuffer[Expression]) =>
        args.foreach(o => annotateOffsetExpression(minOffset, maxOffset, o))
      case MinimumExpression(args : ListBuffer[Expression]) =>
        args.foreach(o => annotateOffsetExpression(minOffset, maxOffset, o))
      case x => // nothing to do
    }
  }

  private def extractConstraints(expr : Expression, constraints : StringBuilder, formatString : Boolean,
    lParConstr : StringBuilder = null, gParConstr : StringBuilder = null, vars : Set[String] = null) : Boolean = {

    var bool : Boolean = false

    expr match {

      case varAcc : VariableAccess =>
        val islStr : String = ScopNameMapping.expr2id(varAcc)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      // a StringConstant is only allowed, if the value it represents was used correctly before (as a VariableAccess, for example)
      case str : StringLiteral if (ScopNameMapping.id2expr(str.value).isDefined) =>
        val e = ScopNameMapping.id2expr(str.value).get
        val islStr : String = ScopNameMapping.expr2id(e)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case array : ArrayAccess =>
        val islStr : String = ScopNameMapping.expr2id(array)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case IntegerConstant(i) =>
        constraints.append(java.lang.Long.toString(i))

      case b : iv.NeighborIsValid =>
        val islStr : String = ScopNameMapping.expr2id(b)
        // vars and glblParConstr must not be null
        vars.add(islStr)
        gParConstr.append("(0<=").append(islStr).append("<=1)")
        gParConstr.append(" and ")
        constraints.append('(').append(islStr).append("=1)")

      case OffsetIndex(min, max, ind, off) =>
        // preserve extrema information since OffsetIndex will be lost
        annotateOffsetExpression(min, max, off)
        constraints.append('(')
        bool |= extractConstraints(ind, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append('+')
        bool |= extractConstraints(off, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        if (lParConstr != null) off match {
          case _ : VariableAccess | _ : ArrayAccess =>
            lParConstr.append('(').append(min).append("<=")
            lParConstr.append(ScopNameMapping.expr2id(off))
            lParConstr.append("<=").append(max).append(')')
            lParConstr.append(" and ")

          case MultiplicationExpression(ListBuffer(IntegerConstant(c), arr : ArrayAccess)) =>
            lParConstr.append('(').append(min).append("<=").append(c).append('*')
            lParConstr.append(ScopNameMapping.expr2id(arr))
            lParConstr.append("<=").append(max).append(')')
            lParConstr.append(" and ")

          case MultiplicationExpression(ListBuffer(arr : ArrayAccess, IntegerConstant(c))) =>
            lParConstr.append('(').append(min).append("<=").append(c).append('*')
            lParConstr.append(ScopNameMapping.expr2id(arr))
            lParConstr.append("<=").append(max).append(')')
            lParConstr.append(" and ")

          case _ =>
        }
      //        val islStr : String = ScopNameMapping.expr2id(expr)
      //        if (vars != null)
      //          vars.add(islStr)
      //        constraints.append(islStr)
      //        if (paramConstr != null) {
      //          paramConstr.append(" and ")
      //          paramConstr.append('(').append(min).append('+')
      //          extractConstraints(ind, paramConstr, formatString, null, vars)
      //          paramConstr.append("<=").append(islStr).append("<=")
      //          extractConstraints(ind, paramConstr, formatString, null, vars)
      //          paramConstr.append('+').append(max).append(')')
      //        }

      case iff : iv.IndexFromField =>
        val islStr : String = ScopNameMapping.expr2id(iff)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)
        if (gParConstr != null) {
          gParConstr.append('(').append(islStr).append(">=0)")
          gParConstr.append(" and ")
        }

      case AdditionExpression(sums) =>
        constraints.append('(')
        for (s <- sums) {
          bool |= extractConstraints(s, constraints, formatString, lParConstr, gParConstr, vars)
          constraints.append('+')
        }
        constraints(constraints.length - 1) = ')' // replace last '+'

      case SubtractionExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append('-')
        bool |= extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')

      case MultiplicationExpression(facs) =>
        constraints.append('(')
        for (s <- facs) {
          bool |= extractConstraints(s, constraints, formatString, lParConstr, gParConstr, vars)
          constraints.append('*')
        }
        constraints(constraints.length - 1) = ')' // replace last '*'

      case DivisionExpression(l, r) =>
        constraints.append("floord(")
        bool |= extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(',')
        bool |= extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')

      case ModuloExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append('%')
        if (formatString)
          constraints.append('%')
        bool |= extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')

      case MinimumExpression(es) =>
        constraints.append("min(")
        for (e <- es) {
          bool |= extractConstraints(e, constraints, formatString, lParConstr, gParConstr, vars)
          constraints.append(',')
        }
        constraints(constraints.length - 1) = ')' // replace last ','

      case MaximumExpression(es) =>
        constraints.append("max(")
        for (e <- es) {
          bool |= extractConstraints(e, constraints, formatString, lParConstr, gParConstr, vars)
          constraints.append(',')
        }
        constraints(constraints.length - 1) = ')' // replace last ','

      case NegationExpression(e) =>
        constraints.append("!(")
        extractConstraints(e, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case LowerExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append('<')
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case LowerEqualExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append("<=")
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case GreaterEqualExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(">=")
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case GreaterExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append('>')
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case EqEqExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append('=')
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case NeqExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append("!=")
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case AndAndExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(" and ")
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(')')
        bool = true

      case OrOrExpression(l, r) =>
        constraints.append('(')
        extractConstraints(l, constraints, formatString, lParConstr, gParConstr, vars)
        constraints.append(" or ")
        extractConstraints(r, constraints, formatString, lParConstr, gParConstr, vars)
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
        case _ =>
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
        case '-' if name.charAt(i + 1) != '>' =>
          throw new ExtractionException("expression in string constant found: " + name)
        case _ =>
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
  private val conditions = new ArrayStack[String]()

  /** all found static control parts */
  val scops = new ArrayBuffer[Scop](256)
  val trash = new ArrayBuffer[(Node, String)] // TODO: debug; remove

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

    def create(root : LoopOverDimensions, localContext : isl.Set, globalContext : isl.Set, optLevel : Int,
      origLoopVars : ArrayBuffer[String], modelLoopVars : String, setTempl : String, mapTempl : String,
      mergeWithPrev : Boolean) : Unit = {

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
          case Access.READ => isRead = true
          case Access.WRITE => isWrite = true
          case Access.UPDATE =>
            isRead = true
            isWrite = true
        }
      case None =>
    }

    if (node.hasAnnotation(SKIP_ANNOT))
      skip = true
    if (skip)
      return

    try {
      if (!curScop.exists())
        node match {
          case loop : LoopOverDimensions with PolyhedronAccessible if loop.hasAnnotation(CudaStrategiesUtils.CUDA_LOOP_ANNOTATION) =>
            loop.indices.annotate(SKIP_ANNOT)
            loop.stepSize.annotate(SKIP_ANNOT)
            if (loop.condition.isDefined)
              loop.condition.get.annotate(SKIP_ANNOT)
            if (loop.reduction.isDefined)
              loop.reduction.get.annotate(SKIP_ANNOT)
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

          case StringLiteral(varName) =>
            enterScalarAccess(varName)

          case VariableAccess(varName, ty) =>
            if (ty.isDefined)
              ty.get.annotate(SKIP_ANNOT)
            enterScalarAccess(varName)

          case ArrayAccess(array @ StringLiteral(varName), index, _) =>
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
            d.dataType.annotate(SKIP_ANNOT)
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

          case _ : IntegerConstant
            | _ : FloatConstant
            | _ : BooleanConstant
            | _ : NegativeExpression
            | _ : NegationExpression
            | _ : AddressofExpression
            | _ : DerefAccess
            | _ : AdditionExpression
            | _ : SubtractionExpression
            | _ : MultiplicationExpression
            | _ : DivisionExpression
            | _ : ModuloExpression
            | _ : PowerExpression
            | _ : MinimumExpression
            | _ : MaximumExpression
            | _ : CommentStatement
            | NullStatement => // nothing to do for all of them...

          // deny
          case e : ExpressionStatement => throw new ExtractionException("cannot deal with ExprStmt: " + e.prettyprint())
          case ArrayAccess(a, _, _) => throw new ExtractionException("ArrayAccess to base " + a.getClass() + " not yet implemented")
          case f : FunctionCallExpression => throw new ExtractionException("function call not in set of allowed ones: " + f.prettyprint())
          case x : Any => throw new ExtractionException("cannot deal with " + x.getClass())
        }
    } catch {
      case ExtractionException(msg) => curScop.discard(msg)
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
        case l : LoopOverDimensions => leaveLoop(l)
        case c : ConditionStatement => leaveCondition(c)
        case _ : AssignmentStatement => leaveAssign()
        case _ : StringLiteral => leaveScalarAccess()
        case _ : VariableAccess => leaveScalarAccess()
        case _ : ArrayAccess => leaveArrayAccess()
        case _ : DirectFieldAccess => leaveFieldAccess()
        case _ : TempBufferAccess => leaveTempBufferAccess()
        case _ : LoopCarriedCSBufferAccess => leaveLoopCarriedCSBufferAccess()
        case _ : VariableDeclarationStatement => leaveDecl()
        case _ =>
      }
  }

  override def reset() : Unit = {
    curScop.discard()
    conditions.clear()
    isRead = false
    isWrite = false
    skip = false
    mergeScops = false
    scops.clear()
    trash.clear()
  }

  /////////////////// methods for node processing \\\\\\\\\\\\\\\\\\\

  private def enterLoop(loop : LoopOverDimensions with PolyhedronAccessible, mergeWithPrev : Boolean) : Unit = {

    for (step <- loop.stepSize)
      if (step != IntegerConstant(1))
        throw new ExtractionException("only stride 1 supported yet")

    val dims : Int = loop.numDimensions

    val begin : MultiIndex = loop.indices.begin
    val end : MultiIndex = loop.indices.end
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
      bool |= extractConstraints(begin(i), constrs, true, locCtxConstrs, gloCtxConstrs, params)
      constrs.append("<=")
      constrs.append(ScopNameMapping.expr2id(new VariableAccess(dimToString(i), IntegerDatatype)))
      constrs.append('<')
      bool |= extractConstraints(end(i), constrs, true, locCtxConstrs, gloCtxConstrs, params)
      constrs.append(" and ")
      val lVar : Expression = loopVarExps(i)
      modelLoopVars.push(ScopNameMapping.expr2id(lVar))
      origLoopVars += lVar.asInstanceOf[VariableAccess].name
      i += 1
    } while (i < dims)

    if (bool)
      throw new ExtractionException("loop bounds contain (in)equalities")

    if (loop.condition.isDefined)
      extractConstraints(loop.condition.get, constrs, true, locCtxConstrs, gloCtxConstrs, params)
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
  }

  private def leaveLoop(loop : LoopOverDimensions) : Unit = {
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
      extractConstraints(cond.condition, sb, false)
      conditions.push(sb.toString())
    } else
      throw new ExtractionException("cannot deal with a non-empty falseBody in a ConditionStatement: " + cond.prettyprint())
  }

  private def leaveCondition(cond : ConditionStatement) : Unit = {
    if (cond.falseBody.isEmpty)
      conditions.pop()
  }

  private def enterStmt(stmt : Statement) : Unit = {

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

  private def enterArrayAccess(name : String, index : Expression, deadAfterScop : Boolean = false) : Unit = {

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
          ineq |= extractConstraints(i, indB, false)
          indB.append(',')
        }
        if (!mInd.isEmpty)
          indB.deleteCharAt(indB.length - 1)
      case ind =>
        ineq |= extractConstraints(ind, indB, false)
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
      case s => name.append(s.prettyprint())
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
        new VariableAccess(decl.name, decl.dataType), decl.expression.get, "=")
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
