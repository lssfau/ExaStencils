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
import exastencils.datastructures.ir.DefaultLoopMultiIndex
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

class Extractor extends Collector {
  import scala.language.implicitConversions

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

  /** all found static control parts */
  val scops = new ArrayStack[Scop]
  val trash = new ArrayStack[(Node, String)] // TODO: debug; remove

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
        label_ = "S" + id_
        return this
      }
    }

    private var scop_ : Scop = null
    private var loopVars_ : String = null
    private var setTemplate_ : String = null
    private var mapTemplate_ : String = null

    private final val formatterResult : java.lang.StringBuilder = new java.lang.StringBuilder()
    private final val formatter = new java.util.Formatter(formatterResult)

    def create(root : Node, loopVars : String, setTempl : String, mapTempl : String) : Unit = {
      this.scop_ = new Scop(root)
      this.loopVars_ = loopVars
      this.setTemplate_ = setTempl
      this.mapTemplate_ = mapTempl
    }

    def exists() : Boolean = {
      return scop_ != null
    }

    def get() : Scop = {
      return scop_
    }

    def loopVars() : String = {
      return loopVars_
    }

    // [..] -> { %s[..] : .. }
    def buildIslSet(tupleName : String) : isl.Set = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(setTemplate_, tupleName)
      val set = new isl.Set(formatterResult.toString())
      // TODO: remove
      //      val f = classOf[isl.UnionSet].getDeclaredField("ptr")
      //      f.setAccessible(true)
      //      if (f.get(set).asInstanceOf[com.sun.jna.PointerType] == null) {
      //        println()
      //        println("======================================================")
      //        println(setTemplate_)
      //        println(tupleName)
      //        println()
      //      }
      return set
    }

    // [..] -> { %s[..] -> %s[%s] }
    def buildIslMap(inTupleName : String, outTupleName : String, out : String) : isl.Map = {
      formatterResult.delete(0, Int.MaxValue)
      formatter.format(mapTemplate_, inTupleName, outTupleName, out)
      val map = new isl.Map(formatterResult.toString())
      // TODO: remove
      //      val f = classOf[isl.UnionMap].getDeclaredField("ptr")
      //      f.setAccessible(true)
      //      if (f.get(map).asInstanceOf[com.sun.jna.PointerType] == null) {
      //        println()
      //        println("======================================================")
      //        println(mapTemplate_)
      //        println(inTupleName)
      //        println(outTupleName)
      //        println(out)
      //        println()
      //      }
      return map
    }

    def finish() : Scop = {
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

    if (!curScop.exists())
      node match {
        case loop : LoopOverDimensions with PolyhedronAccessable =>
          loop.indices.annotate(SKIP_ANNOT)
          loop.stepSize.annotate(SKIP_ANNOT)
          if (loop.condition.isDefined)
            loop.condition.get.annotate(SKIP_ANNOT)
          if (loop.reduction.isDefined)
            loop.reduction.get.annotate(SKIP_ANNOT)
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
        case e : ExpressionStatement => curScop.discard("cannot deal with ExprStmt: " + e.cpp)
        case x : Any                 => curScop.discard("cannot deal with " + x.getClass())
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
    curScop.discard()
    isRead = false
    isWrite = false
    skip = false
    scops.clear()
    trash.clear()
  }

  /////////////////// auxiliary methodes \\\\\\\\\\\\\\\\\\\

  private def extractConstraints(expr : Expression, constraints : StringBuilder,
    formatString : Boolean, vars : Set[String] = null) : Boolean = {

    var bool : Boolean = false

    expr match {

      case str : StringConstant =>
        val islStr : String = ScopNameMapping.expr2id(str)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case varAcc : VariableAccess =>
        val islStr : String = ScopNameMapping.expr2id(varAcc)
        if (vars != null)
          vars.add(islStr)
        constraints.append(islStr)

      case IntegerConstant(i) =>
        constraints.append(java.lang.Long.toString(i))

      case OffsetIndex(_, _, ind, off) =>
        constraints.append('(')
        bool |= extractConstraints(ind, constraints, formatString, vars)
        constraints.append('+')
        bool |= extractConstraints(off, constraints, formatString, vars)
        constraints.append(')')

      case AdditionExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, vars)
        constraints.append('+')
        bool |= extractConstraints(r, constraints, formatString, vars)
        constraints.append(')')

      case SubtractionExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, vars)
        constraints.append('-')
        bool |= extractConstraints(r, constraints, formatString, vars)
        constraints.append(')')

      case MultiplicationExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, vars)
        constraints.append('*')
        bool |= extractConstraints(r, constraints, formatString, vars)
        constraints.append(')')

      case DivisionExpression(l, r) =>
        constraints.append("floord(")
        bool |= extractConstraints(l, constraints, formatString, vars)
        constraints.append(',')
        bool |= extractConstraints(r, constraints, formatString, vars)
        constraints.append(')')

      case ModuloExpression(l, r) =>
        constraints.append('(')
        bool |= extractConstraints(l, constraints, formatString, vars)
        constraints.append('%')
        if (formatString)
          constraints.append('%')
        bool |= extractConstraints(r, constraints, formatString, vars)
        constraints.append(')')

      case LowerExpression(l, r) =>
        extractConstraints(l, constraints, formatString, vars)
        constraints.append('<')
        extractConstraints(r, constraints, formatString, vars)
        bool = true

      case LowerEqualExpression(l, r) =>
        extractConstraints(l, constraints, formatString, vars)
        constraints.append("<=")
        extractConstraints(r, constraints, formatString, vars)
        bool = true

      case GreaterEqualExpression(l, r) =>
        extractConstraints(l, constraints, formatString, vars)
        constraints.append(">=")
        extractConstraints(r, constraints, formatString, vars)
        bool = true

      case GreaterExpression(l, r) =>
        extractConstraints(l, constraints, formatString, vars)
        constraints.append('>')
        extractConstraints(r, constraints, formatString, vars)
        bool = true

      case EqEqExpression(l, r) =>
        extractConstraints(l, constraints, formatString, vars)
        constraints.append('=')
        extractConstraints(r, constraints, formatString, vars)
        bool = true

      case NeqNeqExpression(l, r) =>
        extractConstraints(l, constraints, formatString, vars)
        constraints.append("!=")
        extractConstraints(r, constraints, formatString, vars)
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
      curScop.discard("reductions not supported yet")
      return
    }

    for (step <- loop.stepSize)
      if (step != IntegerConstant(1)) {
        curScop.discard("only stride 1 supported yet")
        return
      }

    val dims : Int = Knowledge.dimensionality

    val begin : MultiIndex = loop.indices.begin
    val end : MultiIndex = loop.indices.end
    val loopVarExps : MultiIndex = DefaultLoopMultiIndex.apply()

    val params : Set[String] = new HashSet[String]()
    val loopVars : ArrayStack[String] = new ArrayStack[String]()
    val constrs : StringBuilder = new StringBuilder()

    var bool : Boolean = false
    try {
      var i : Int = 0
      do {
        bool |= extractConstraints(begin(i), constrs, true, params)
        constrs.append("<=")
        constrs.append(dimToString(i))
        constrs.append('<')
        bool |= extractConstraints(end(i), constrs, true, params)
        constrs.append(" and ")
        loopVars.push(ScopNameMapping.expr2id(loopVarExps(i)))
        i += 1
      } while (i < dims)
    } catch {
      case ExtractionException(msg) =>
        curScop.discard(msg)
        return
    }
    if (bool) {
      curScop.discard("loop bounds contain (in)equalities")
      return
    }

    if (loop.condition.isDefined)
      extractConstraints(loop.condition.get, constrs, true)
    else
      constrs.delete(constrs.length - 5, Int.MaxValue)

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

    curScop.create(loop, loopVars.mkString(","), setTemplate, mapTemplate)
  }

  private def leaveLoop(loop : LoopOverDimensions) : Unit = {
    val scop = curScop.finish()
    if (scop != null) {
      loop.annotate(PolyOpt.SCOP_ANNOT, scop)
      scops.push(scop)
    }
  }

  private def enterStmt(stmt : Statement) : Unit = {

    val scop : Scop = curScop.get()

    val label : String = curScop.curStmt.next().label()
    scop.stmts.put(label, stmt)

    val domain = curScop.buildIslSet(label)
    scop.domain = if (scop.domain == null) domain else scop.domain.addSet(domain)
    val schedule = curScop.buildIslMap(label, "", curScop.loopVars() + ',' + curScop.curStmt.id())
    scop.schedule = if (scop.schedule == null) schedule else scop.schedule.addMap(schedule)
  }

  private def leaveStmt() : Unit = {
    curScop.curStmt.leave()
  }

  private def enterAssign(assign : AssignmentStatement) : Unit = {

    enterStmt(assign) // as an assignment is also a statement

    if (isRead || isWrite) {
      curScop.discard("nested assignments are not supported (yet...?); skipping scop")
      return
    }

    assign.op match {

      case StringConstant("=") =>
        assign.dest.annotate(Access.ANNOT, Some(Access.WRITE))

      case StringConstant("+=") | StringConstant("-=") | StringConstant("*=") | StringConstant("/=") =>
        assign.dest.annotate(Access.ANNOT, Some(Access.UPDATE))

      case _ =>
        curScop.discard("unrecognized assignment operator: " + assign.op)
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
        case '=' | '+' | '-' | '*' | '/' | '[' | '(' | '.' =>
          curScop.discard("expression in StringConstant found: " + varName)
          return
        case _ =>
      }
    }

    if (!curScop.curStmt.exists() || (!isRead && !isWrite)) {
      curScop.discard("misplaced access expression?")
      return
    }

    val scop : Scop = curScop.get()

    var access : isl.Map = curScop.buildIslMap(curScop.curStmt.label(), varName, "")

    if (isRead)
      scop.reads = if (scop.reads == null) access else scop.reads.addMap(access)
    if (isWrite)
      scop.writes = if (scop.writes == null) access else scop.writes.addMap(access)
  }

  private def leaveScalarAccess() : Unit = {
    // nothing to do here...
  }

  private def enterArrayAccess(name : String, index : Expression) : Unit = {

    if (!curScop.curStmt.exists() || (!isRead && !isWrite)) {
      curScop.discard("misplaced access expression?")
      return
    }

    val scop : Scop = curScop.get()

    var bool : Boolean = false
    val indB : StringBuilder = new StringBuilder()
    index match {
      case mInd : MultiIndex =>
        for (i <- mInd) {
          bool |= extractConstraints(i, indB, false)
          indB.append(',')
        }
        indB.deleteCharAt(indB.length - 1)
      case ind =>
        bool |= extractConstraints(ind, indB, false)
    }
    if (bool) {
      curScop.discard("array access contains (in)equalities")
      return
    }

    var access : isl.Map = curScop.buildIslMap(curScop.curStmt.label(), replaceSpecial(name), indB.toString())

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
