package exastencils.polyhedron

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set
import scala.collection.mutable.StringBuilder
import scala.language.implicitConversions

import exastencils.core.Logger
import exastencils.core.collectors.Collector
import exastencils.datastructures.Annotation
import exastencils.datastructures.Node
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.ArrayAccess
import exastencils.datastructures.ir.AssignmentStatement
import exastencils.datastructures.ir.BooleanConstant
import exastencils.datastructures.ir.DirectFieldAccess
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.EqEqExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.ExpressionStatement
import exastencils.datastructures.ir.FieldAccess
import exastencils.datastructures.ir.FloatConstant
import exastencils.datastructures.ir.GreaterEqualExpression
import exastencils.datastructures.ir.GreaterExpression
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.LowerEqualExpression
import exastencils.datastructures.ir.LowerExpression
import exastencils.datastructures.ir.ModuloExpression
import exastencils.datastructures.ir.MultiIndex
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.NeqNeqExpression
import exastencils.datastructures.ir.OffsetIndex
import exastencils.datastructures.ir.PowerExpression
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.StatementBlock
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.VariableAccess
import exastencils.knowledge.Field
import exastencils.knowledge.Knowledge
import exastencils.knowledge.dimToString
import exastencils.primitives.LoopOverDimensions

object Extractor extends Collector {
  import scala.language.implicitConversions

  /** implicit conversion for use in this class */
  //  private final implicit def convertLongToVal(l : Long) : isl.Val = new isl.Val(l.toString())

  /** constants for 1 and -1, which are needed more often later */
  //  private final val POS_ONE : isl.Val = convertIntToVal(1)
  //  private final val NEG_ONE : isl.Val = convertIntToVal(-1)

  /** constants for read/write annotations */
  private object Access extends Enumeration {
    type Access = Value
    final val ANNOT : String = "PolyAcc"
    final val READ, WRITE, UPDATE = Value

    exastencils.core.Duplicate.registerImmutable(this.getClass())
  }

  /** current access node is a read/write access */
  private var isRead, isWrite : Boolean = false

  /** annotation id used to indicate that this subtree should be skipped */
  final val SKIP_ANNOT : String = "PolySkip"

  /** indicates, if the current subtree should be skipped */
  private var skip : Boolean = false

  /** integer used to create an identifier for new statements and to determine the ordering of the statements inside the model */
  //  private var id : Int = 0

  /** all found static control parts */
  val scops = new ArrayStack[SCoP]
  val trash = new ArrayStack[(Node, String)] // TODO: debug; remove

  /** template for current iteration domain */
  //  private var template : isl.BasicSet = null

  private object curSCoP {

    object curStmt {

      private var id_ : Int = -1
      private var label_ : String = null

      def exists() : Boolean = { label_ != null }
      def label() : String = { label_ }
      def id() : Int = { id_ }
      def leave() : Unit = { label_ = null }
      def next() : this.type = {
        id_ += 1
        label_ = "S" + id_
        return this
      }
    }

    private var scop_ : SCoP = null
    private var loopVars_ : String = null
    private var setTemplate_ : String = null
    private var mapTemplate_ : String = null

    private final val formatterResult : java.lang.StringBuilder = new java.lang.StringBuilder()
    private final val formatter = new java.util.Formatter(formatterResult)

    def create(root : Node, loopVars : String, setTempl : String, mapTempl : String) : Unit = {
      this.scop_ = new SCoP(root)
      this.loopVars_ = loopVars
      this.setTemplate_ = setTempl
      this.mapTemplate_ = mapTempl
    }

    def exists() : Boolean = {
      return scop_ != null
    }

    def get() : SCoP = {
      return scop_
    }

    def loopVars() : String = {
      return loopVars_
    }

    def buildIslSet(tupleName : String) : isl.Set = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(setTemplate_, tupleName)
      return new isl.Set(formatterResult.toString())
    }

    def buildIslMap(inTupleName : String, outTupleName : String, out : String) : isl.Map = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(mapTemplate_, inTupleName, outTupleName, out)
      return new isl.Map(formatterResult.toString())
    }

    def finish() : SCoP = {
      val res = scop_
      discard()
      return res
    }

    def discard(msg : String = null) : Unit = {
      if (msg != null) {
        Logger.debug("[poly ex] SCoP discarded:  " + msg)
        trash.push((if (scop_ != null) scop_.root else null, msg))
      }
      scop_ = null
      loopVars_ = null
      setTemplate_ = null
      mapTemplate_ = null
      curStmt.leave()
    }
  }

  /** iteration domain of current statement (when below a statement node) */
  //  private var curStmtDomain : isl.BasicSet = null

  /////////////////// Collector methods \\\\\\\\\\\\\\\\\\\

  override def enter(node : Node) : Unit = {

    node.getAnnotation(Access.ANNOT) match {
      case Some(Annotation(_, Some(acc))) =>
        acc match {
          case Access.READ  => isRead = true
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

    if (!curSCoP.exists())
      node match {
        case loop : LoopOverDimensions with PolyhedronAccessable =>
          loop.indices.annotate(SKIP_ANNOT)
          loop.stepSize.annotate(SKIP_ANNOT)
          enterLoop(loop)

        case _ =>
      }

    else
      node match {

        // process
        case a : AssignmentStatement =>
          a.op.annotate(SKIP_ANNOT)
          enterAssign(a)

        case StringConstant(varName) =>
          enterScalarAccess(varName)

        case VariableAccess(varName, _) =>
          enterScalarAccess(varName)

        case ArrayAccess(array @ StringConstant(varName), index) =>
          array.annotate(SKIP_ANNOT)
          index.annotate(SKIP_ANNOT)
          enterArrayAccess(varName, index)

        case ArrayAccess(array @ VariableAccess(varName, _), index) =>
          array.annotate(SKIP_ANNOT)
          index.annotate(SKIP_ANNOT)
          enterArrayAccess(varName, index)

        case DirectFieldAccess(owner, field, slot, index) =>
          owner.annotate(SKIP_ANNOT)
          //field.annotate(SKIP_ANNOT)
          slot.annotate(SKIP_ANNOT)
          index.annotate(SKIP_ANNOT)
          enterFieldAccess(owner, field, slot, index)

        case FieldAccess(owner, field, slot, index) =>
          owner.annotate(SKIP_ANNOT)
          //field.annotate(SKIP_ANNOT)
          slot.annotate(SKIP_ANNOT)
          index.annotate(SKIP_ANNOT)
          enterFieldAccess(owner, field, slot, index, field.referenceOffset)

        // ignore
        case _ : StatementBlock
          | _ : IntegerConstant
          | _ : FloatConstant
          | _ : BooleanConstant
          | _ : UnaryExpression
          | _ : AdditionExpression
          | _ : SubtractionExpression
          | _ : MultiplicationExpression
          | _ : DivisionExpression
          | _ : ModuloExpression
          | _ : PowerExpression => // nothing to do for all of them...

        // deny
        case e : ExpressionStatement => curSCoP.discard("cannot deal with ExprStmt: " + e.cpp)
        case x : Any                 => curSCoP.discard("cannot deal with " + x.getClass())
      }
  }

  override def leave(node : Node) : Unit = {

    if (node.removeAnnotation(Access.ANNOT).isDefined) {
      isRead = false
      isWrite = false
    }

    if (node.removeAnnotation(SKIP_ANNOT).isDefined)
      skip = false

    node match {
      case l : LoopOverDimensions  => leaveLoop(l)
      case _ : AssignmentStatement => leaveAssign()
      case _ : StringConstant      => leaveScalarAccess()
      case _ : VariableAccess      => leaveScalarAccess()
      case _ : ArrayAccess         => leaveArrayAccess()
      case _ : DirectFieldAccess   => leaveFieldAccess()
      case _ : FieldAccess         => leaveFieldAccess()
      case _                       =>
    }
  }

  override def reset() : Unit = {
    curSCoP.discard()
    isRead = false
    isWrite = false
    skip = false
    scops.clear()
    trash.clear()
  }

  /////////////////// auxiliary methodes \\\\\\\\\\\\\\\\\\\

  private def extractConstraints(expr : Expression, constraints : StringBuilder, vars : Set[String] = null) : Boolean = {

    var bool : Boolean = false

    expr match {

      case StringConstant(name) =>
        val clean : String = replaceSpecial(new StringBuilder(name)).toString()
        if (vars != null)
          vars.add(clean)
        constraints.append(clean)

      case VariableAccess(name, _) =>
        val clean : String = replaceSpecial(new StringBuilder(name)).toString()
        if (vars != null)
          vars.add(clean)
        constraints.append(clean)

      case IntegerConstant(i) =>
        constraints.append(java.lang.Long.toString(i))

      case OffsetIndex(_, _, ind, off) =>
        constraints.append('(')
        bool |= extractConstraints(ind, constraints, vars)
        constraints.append('+')
        bool |= extractConstraints(off, constraints, vars)
        constraints.append(')')

      case AdditionExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, vars)
        constraints.append('+')
        bool |= extractConstraints(r, constraints, vars)
        constraints.append(')')

      case SubtractionExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, vars)
        constraints.append('-')
        bool |= extractConstraints(r, constraints, vars)
        constraints.append(')')

      case MultiplicationExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, vars)
        constraints.append('*')
        bool |= extractConstraints(r, constraints, vars)
        constraints.append(')')

      case DivisionExpression(l, r) =>
        constraints.append("floord(")
        bool |= extractConstraints(l, constraints, vars)
        constraints.append(',')
        bool |= extractConstraints(r, constraints, vars)
        constraints.append(')')

      case ModuloExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, vars)
        constraints.append("% ") // blank after % is needed to prevent creating unwanted labels in format string
        bool |= extractConstraints(r, constraints, vars)
        constraints.append(')')

      case LowerExpression(l, r) =>
        extractConstraints(l, constraints, vars)
        constraints.append('<')
        extractConstraints(r, constraints, vars)
        bool = true

      case LowerEqualExpression(l, r) =>
        extractConstraints(l, constraints, vars)
        constraints.append("<=")
        extractConstraints(r, constraints, vars)
        bool = true

      case GreaterEqualExpression(l, r) =>
        extractConstraints(l, constraints, vars)
        constraints.append(">=")
        extractConstraints(r, constraints, vars)
        bool = true

      case GreaterExpression(l, r) =>
        extractConstraints(l, constraints, vars)
        constraints.append('>')
        extractConstraints(r, constraints, vars)
        bool = true

      case EqEqExpression(l, r) =>
        extractConstraints(l, constraints, vars)
        constraints.append('=')
        extractConstraints(r, constraints, vars)
        bool = true

      case NeqNeqExpression(l, r) =>
        extractConstraints(l, constraints, vars)
        constraints.append("!=")
        extractConstraints(r, constraints, vars)
        bool = true

      case _ => throw new ExtractionException("unknown expression: " + expr.getClass() + " - " + expr.cpp())
    }

    return bool
  }

  case class ExtractionException(msg : String) extends Exception(msg)

  private def replaceSpecial(str : String) : String = {
    return replaceSpecial(new StringBuilder(str)).toString()
  }

  private def replaceSpecial(str : StringBuilder) : StringBuilder = {

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

  /////////////////// methods for node processing \\\\\\\\\\\\\\\\\\\

  private def enterLoop(loop : LoopOverDimensions) : Unit = {

    if (loop.reduction.isDefined) { // TODO: support reductions
      curSCoP.discard("reductions not supported yet")
      return
    }

    val dims : Int = Knowledge.dimensionality

    val begin : MultiIndex = loop.indices.begin
    val end : MultiIndex = loop.indices.end

    val params : Set[String] = new HashSet[String]()
    val loopVars : ArrayStack[String] = new ArrayStack[String]()
    val constrs : StringBuilder = new StringBuilder()

    var bool : Boolean = false
    try {
      var i : Int = 0
      do {
        bool |= extractConstraints(begin(i), constrs, params)
        constrs.append("<=")
        constrs.append(dimToString(i))
        constrs.append('<')
        bool |= extractConstraints(end(i), constrs, params)
        constrs.append(',')
        loopVars.push(dimToString(i))
        i += 1
      } while (i < dims)
    } catch {
      case ExtractionException(msg) =>
        curSCoP.discard(msg)
        return
    }
    if (bool) {
      curSCoP.discard("loop bounds contain (in)equalities")
      return
    }

    if (loop.condition.isDefined)
      extractConstraints(loop.condition.get, constrs)
    else
      constrs.deleteCharAt(constrs.length - 1)

    // remove variables from params set
    for (v <- loopVars)
      params.remove(v)

    val templateBuilder : StringBuilder = new StringBuilder()
    templateBuilder.append('[')
    for (p <- params)
      templateBuilder.append(p).append(',')
    if (!params.isEmpty)
      templateBuilder.deleteCharAt(templateBuilder.length - 1)
    templateBuilder.append("]->{%s[")
    for (v <- loopVars)
      templateBuilder.append(v).append(',')
    templateBuilder.setCharAt(templateBuilder.length - 1, ']')

    // finish for set
    val tmp : Int = templateBuilder.length
    templateBuilder.append(':')
    templateBuilder.append(constrs)
    templateBuilder.append('}')
    val setTemplate : String = templateBuilder.toString()

    // remove last and finish for map
    templateBuilder.delete(tmp, templateBuilder.length)
    templateBuilder.append("->%s[%s]}")
    val mapTemplate : String = templateBuilder.toString()

    curSCoP.create(loop, loopVars.mkString(","), setTemplate, mapTemplate)
  }

  //  private def enterLoop(loop : LoopOverDimensions) : Unit = {
  //
  //    if (template != null)
  //      discardCurrentSCoP("nested LoopOverDimensions?! possibly a bug? ignoring outermost")
  //
  //    if (loop.reduction.isDefined) { // TODO: support reductions
  //      discardCurrentSCoP("reductions not supported yet")
  //      return
  //    }
  //
  //    if (loop.condition.isDefined) { // TODO: implement
  //      discardCurrentSCoP("conditions in loops not supported yet")
  //      return
  //    }
  //
  //    val dims : Int = Knowledge.dimensionality
  //    var domain : isl.BasicSet = isl.BasicSet.universe(isl.Space.setAlloc(0, dims))
  //    val nameToPos = new HashMap[String, (isl.DimType, Int)]()
  //    var d : Int = 0
  //    do {
  //      nameToPos(dimToString(d)) = (isl.DimType.Set, dims - d - 1)
  //      d += 1
  //    } while (d < dims)
  //
  //    val begin : MultiIndex = loop.indices.begin
  //    val end : MultiIndex = loop.indices.end
  //
  //    d = 0
  //    do {
  //      val stride : Expression = loop.stepSize(d) // TODO: remove restriction
  //      if (!stride.isInstanceOf[IntegerConstant] || (stride.asInstanceOf[IntegerConstant].value != 1)) {
  //        discardCurrentSCoP("LoopOverDimensions stepSize/stride must be one (for now...), ignoring loop")
  //        return
  //      }
  //
  //      val (curBegin, curEnd) : (HashMap[String, Long], HashMap[String, Long]) =
  //        try {
  //          (SimplifyExpression.evalIntegralAffine(begin(d)), SimplifyExpression.evalIntegralAffine(end(d)))
  //        } catch {
  //          case EvaluationException(msg) =>
  //            discardCurrentSCoP("evaluating loop bounds: " + msg)
  //            return
  //        }
  //
  //      // count nr of new params and set stub in name mapping
  //      var nrParam : Int = 0
  //      for (name : String <- curBegin.keys if (name != SimplifyExpression.constName && !nameToPos.contains(name))) {
  //        nameToPos(name) = null
  //        nrParam += 1
  //      }
  //      for (name : String <- curEnd.keys if (name != SimplifyExpression.constName && !nameToPos.contains(name))) {
  //        nameToPos(name) = null
  //        nrParam += 1
  //      }
  //
  //      // replace stub in mapping and set name in domain
  //      var i : Int = domain.dim(isl.DimType.Param)
  //      domain = domain.addDims(isl.DimType.Param, nrParam)
  //      for ((name : String, pos) <- nameToPos if (pos == null)) {
  //        nameToPos(name) = (isl.DimType.Param, i)
  //        domain = domain.setDimName(isl.DimType.Param, i, name)
  //        i += 1
  //      }
  //
  //      val (dimType : isl.DimType, pos : Int) = nameToPos(dimToString(d))
  //      var constraint : isl.Constraint = null
  //      var const : Option[Long] = null
  //
  //      // begin <= i  -->  (1)*i + (-begin) >= 0
  //      constraint = isl.Constraint.inequalityAlloc(domain.getLocalSpace())
  //      constraint = constraint.setCoefficientVal(dimType, pos, POS_ONE)
  //      const = curBegin.remove(SimplifyExpression.constName)
  //      if (const.isDefined)
  //        constraint = constraint.setConstantVal(-const.get)
  //      for ((name : String, value : Long) <- curBegin) {
  //        var (beginDimType : isl.DimType, beginPos : Int) = nameToPos(name)
  //        constraint = constraint.setCoefficientVal(beginDimType, beginPos, -value)
  //      }
  //      domain = domain.addConstraint(constraint)
  //
  //      // i < end  -->  (-1)*i + (end-1) >= 0
  //      constraint = isl.Constraint.inequalityAlloc(domain.getLocalSpace())
  //      constraint = constraint.setCoefficientVal(dimType, pos, NEG_ONE)
  //      const = curEnd.remove(SimplifyExpression.constName)
  //      constraint = constraint.setConstantVal(const.getOrElse(0L) - 1)
  //      for ((name : String, value : Long) <- curEnd) {
  //        var (beginDimType : isl.DimType, beginPos : Int) = nameToPos(name)
  //        constraint = constraint.setCoefficientVal(beginDimType, beginPos, value)
  //      }
  //      domain = domain.addConstraint(constraint)
  //
  //      d += 1
  //    } while (d < dims)
  //
  //    loop.indices.annotate(SKIP_ANNOT)
  //    loop.stepSize.annotate(SKIP_ANNOT)
  //
  //    scops.push(new SCoP(loop, nameToPos, domain.getSpace().params()))
  //    template = domain
  //  }

  private def leaveLoop(loop : LoopOverDimensions) : Unit = {
    val scop = curSCoP.finish()
    if (scop != null) {
      loop.annotate(PolyOpt.SCOP_ANNOT, scop)
      scops.push(scop)
    }
  }

  private def enterStmt(stmt : Statement) : Unit = {

    val scop : SCoP = curSCoP.get()

    val label : String = curSCoP.curStmt.next().label()
    scop.stmts.put(label, stmt)

    val domain = curSCoP.buildIslSet(label)
    scop.domain = if (scop.domain == null) domain else scop.domain.addSet(domain)
    val schedule = curSCoP.buildIslMap(label, "", curSCoP.loopVars() + ',' + curSCoP.curStmt.id())
    scop.schedule = if (scop.schedule == null) schedule else scop.schedule.addMap(schedule)
  }

  private def leaveStmt() : Unit = {
    curSCoP.curStmt.leave()
  }

  private def enterAssign(assign : AssignmentStatement) : Unit = {

    enterStmt(assign) // as an assignment is also a statement

    if (isRead || isWrite) {
      curSCoP.discard("nested assignments are not supported (yet...?); skipping scop")
      return
    }

    assign.op match {

      case StringConstant("=") =>
        assign.dest.annotate(Access.ANNOT, Some(Access.WRITE))

      case StringConstant("+=") | StringConstant("-=") | StringConstant("*=") | StringConstant("/=") =>
        assign.dest.annotate(Access.ANNOT, Some(Access.UPDATE))

      case _ =>
        curSCoP.discard("unrecognized assignment operator: " + assign.op)
        return
    }

    assign.src.annotate(Access.ANNOT, Some(Access.READ))
  }

  private def leaveAssign() : Unit = {
    leaveStmt() // as an assignment is also a statement
  }

  private def enterScalarAccess(varName : String) : Unit = {

    // hack: filter out code
    var i : Int = varName.length()
    while (i > 0) {
      i -= 1
      varName.charAt(i) match {
        case '=' | '+' | '-' | '*' | '/' | '[' | '(' =>
          curSCoP.discard("expression in StringConstant found: " + varName)
          return
        case _ =>
      }
    }

    if (!curSCoP.curStmt.exists() || (!isRead && !isWrite)) {
      curSCoP.discard("misplaced access expression?")
      return
    }

    val scop : SCoP = curSCoP.get()

    var access : isl.Map = curSCoP.buildIslMap(curSCoP.curStmt.label(), varName, "")

    if (isRead)
      scop.reads = if (scop.reads == null) access else scop.reads.addMap(access)
    if (isWrite)
      scop.writes = if (scop.writes == null) access else scop.writes.addMap(access)
  }

  private def leaveScalarAccess() : Unit = {
    // nothing to do here...
  }

  private def enterArrayAccess(name : String, index : Expression) : Unit = {

    if (!curSCoP.curStmt.exists() || (!isRead && !isWrite)) {
      curSCoP.discard("misplaced access expression?")
      return
    }

    val scop : SCoP = curSCoP.get()

    var bool : Boolean = false
    val indB : StringBuilder = new StringBuilder()
    index match {
      case mInd : MultiIndex =>
        for (i <- mInd) {
          bool |= extractConstraints(i, indB)
          indB.append(',')
        }
        indB.deleteCharAt(indB.length - 1)
      case ind =>
        bool |= extractConstraints(ind, indB)
    }
    if (bool) {
      curSCoP.discard("array access contains (in)equalities")
      return
    }

    var access : isl.Map = curSCoP.buildIslMap(curSCoP.curStmt.label(), replaceSpecial(name), indB.toString())

    if (isRead)
      scop.reads = if (scop.reads == null) access else scop.reads.addMap(access)
    if (isWrite)
      scop.writes = if (scop.writes == null) access else scop.writes.addMap(access)
  }

  private def leaveArrayAccess() {
    // nothing to do here...
  }

  private def enterFieldAccess(owner : Expression, field : Field, slot : Expression, index : MultiIndex, offset : MultiIndex = null) : Unit = {

    val name : StringBuilder = new StringBuilder()

    owner.cppsb(name)
    field.codeName.cppsb(name)
    name += '_'
    slot.cppsb(name)

    enterArrayAccess(name.toString(), if (offset == null) index else index + offset)
  }

  private def leaveFieldAccess() : Unit = {
    leaveArrayAccess()
  }
}
