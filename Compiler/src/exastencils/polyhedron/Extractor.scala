package exastencils.polyhedron

import scala.collection.mutable.ArrayStack
import scala.collection.mutable.HashMap
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
import exastencils.datastructures.ir.Datatype
import exastencils.datastructures.ir.DirectFieldAccess
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.ExpressionStatement
import exastencils.datastructures.ir.FieldAccess
import exastencils.datastructures.ir.FloatConstant
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.ModuloExpression
import exastencils.datastructures.ir.MultiIndex
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.PowerExpression
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.StatementBlock
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.UnaryExpression
import exastencils.datastructures.ir.VariableAccess
import exastencils.knowledge.Field
import exastencils.knowledge.IndexRange
import exastencils.knowledge.Knowledge
import exastencils.knowledge.dimToString
import exastencils.primitives.LoopOverDimensions
import exastencils.util.EvaluationException
import exastencils.util.SimplifyExpression
import isl.Conversions.convertIntToVal

object Extractor extends Collector {
  import scala.language.implicitConversions

  /** implicit conversion for use in this class */
  private final implicit def convertLongToVal(l : Long) : isl.Val = new isl.Val(l.toString())

  /** constants for 1 and -1, which are needed more often later */
  private final val POS_ONE : isl.Val = convertIntToVal(1)
  private final val NEG_ONE : isl.Val = convertIntToVal(-1)

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
  private var id : Int = 0

  /** all found static control parts */
  val scops = new ArrayStack[SCoP]
  val trash = new ArrayStack[(Node, String)] // TODO: debug; remove

  /** template for current iteration domain */
  private var template : isl.BasicSet = null

  /** iteration domain of current statement (when below a statement node) */
  private var curStmtDomain : isl.BasicSet = null

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

    // create new SCoP or replace old
    if (node.isInstanceOf[LoopOverDimensions with PolyhedronAccessable])
      enterLoop(node.asInstanceOf[LoopOverDimensions with PolyhedronAccessable])

    else if (template != null) // we have a SCoP now
      node match {

        // process
        case a : AssignmentStatement                        => enterAssign(a)
        case StringConstant(varName)                        => enterScalarAccess(varName)
        case VariableAccess(varName, _)                     => enterScalarAccess(varName)
        case ArrayAccess(StringConstant(varName), index)    => enterArrayAccess(varName, index)
        case ArrayAccess(VariableAccess(varName, _), index) => enterArrayAccess(varName, index)
        case DirectFieldAccess(owner, field, slot, index)   => enterFieldAccess(owner, field, slot, index)
        case FieldAccess(owner, field, slot, index)         => enterFieldAccess(owner, field, slot, index + field.referenceOffset)

        // ignore
        case _ : Datatype // and all of its subclasses
          | _ : StatementBlock
          | _ : MultiIndex
          | _ : IndexRange
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
        case e : ExpressionStatement => discardCurrentSCoP("ExprStmt: " + e.cpp)
        case x : Any                 => discardCurrentSCoP("cannot deal with " + x.getClass())
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
      case l : LoopOverDimensions                   => leaveLoop(l)
      case a : AssignmentStatement                  => leaveAssign(a)
      case _ : StringConstant                       => leaveScalarAccess()
      case _ : VariableAccess                       => leaveScalarAccess()
      case _ : ArrayAccess                          => leaveArrayAccess()
      case DirectFieldAccess(owner, field, slot, _) => leaveFieldAccess(owner, field, slot)
      case FieldAccess(owner, field, slot, _)       => leaveFieldAccess(owner, field, slot)
      case _                                        =>
    }
  }

  override def reset() : Unit = {
    template = null
    isRead = false
    isWrite = false
    scops.clear()
    trash.clear()
  }

  /////////////////// auxiliary methodes \\\\\\\\\\\\\\\\\\\

  private def nextId() : Int = {
    id += 1
    return id
  }

  /** discard current SCoP (top of stack scops) and print a warning massage */
  private def discardCurrentSCoP(msg : String) : Unit = {

    if (msg != null)
      Logger.debug("[poly ex] SCoP discarded:  " + msg)
    if (template != null) {
      trash.push((scops.pop().root, msg))
      template = null
    }
  }

  /////////////////// methods for node processing \\\\\\\\\\\\\\\\\\\

  private def enterLoop(loop : LoopOverDimensions) : Unit = {

    if (template != null) {
      discardCurrentSCoP("nested LoopOverDimensions?! possibly a bug? ignoring outermost")
      return
    }

    if (loop.reduction != None) { // TODO: support reductions
      discardCurrentSCoP("reductions not supported yet")
      return
    }

    val dims : Int = Knowledge.dimensionality
    var domain : isl.BasicSet = isl.BasicSet.universe(isl.Space.setAlloc(0, dims))
    val nameToPos = new HashMap[String, (isl.DimType, Int)]()
    var d : Int = 0
    do {
      nameToPos(dimToString(d)) = (isl.DimType.Set, dims - d - 1)
      d += 1
    } while (d < dims)

    val begin : MultiIndex = loop.indices.begin
    val end : MultiIndex = loop.indices.end

    d = 0
    do {
      val stride : Expression = loop.stepSize(d) // TODO: remove restriction
      if (!stride.isInstanceOf[IntegerConstant] || (stride.asInstanceOf[IntegerConstant].value != 1)) {
        discardCurrentSCoP("LoopOverDimensions stepSize/stride must be one (for now...), ignoring loop")
        return
      }

      val (curBegin, curEnd) : (HashMap[String, Long], HashMap[String, Long]) =
        try {
          (SimplifyExpression.evalIntegralAffine(begin(d)), SimplifyExpression.evalIntegralAffine(end(d)))
        } catch {
          case EvaluationException(msg) =>
            discardCurrentSCoP("evaluating loop bounds: " + msg)
            return
        }

      // count nr of new params and set stub in name mapping
      var nrParam : Int = 0
      for (name : String <- curBegin.keys if (name != SimplifyExpression.constName && !nameToPos.contains(name))) {
        nameToPos(name) = null
        nrParam += 1
      }
      for (name : String <- curEnd.keys if (name != SimplifyExpression.constName && !nameToPos.contains(name))) {
        nameToPos(name) = null
        nrParam += 1
      }

      // replace stub in mapping and set name in domain
      var i : Int = domain.dim(isl.DimType.Param)
      domain = domain.addDims(isl.DimType.Param, nrParam)
      for ((name : String, pos) <- nameToPos if (pos == null)) {
        nameToPos(name) = (isl.DimType.Param, i)
        domain = domain.setDimName(isl.DimType.Param, i, name)
        i += 1
      }

      val (dimType : isl.DimType, pos : Int) = nameToPos(dimToString(d))
      var constraint : isl.Constraint = null
      var const : Option[Long] = null

      // begin <= i  -->  (1)*i + (-begin) >= 0
      constraint = isl.Constraint.inequalityAlloc(domain.getLocalSpace())
      constraint = constraint.setCoefficientVal(dimType, pos, POS_ONE)
      const = curBegin.remove(SimplifyExpression.constName)
      if (const.isDefined)
        constraint = constraint.setConstantVal(-const.get)
      for ((name : String, value : Long) <- curBegin) {
        var (beginDimType : isl.DimType, beginPos : Int) = nameToPos(name)
        constraint = constraint.setCoefficientVal(beginDimType, beginPos, -value)
      }
      domain = domain.addConstraint(constraint)

      // i < end  -->  (-1)*i + (end-1) >= 0
      constraint = isl.Constraint.inequalityAlloc(domain.getLocalSpace())
      constraint = constraint.setCoefficientVal(dimType, pos, NEG_ONE)
      const = curEnd.remove(SimplifyExpression.constName)
      constraint = constraint.setConstantVal(const.getOrElse(0L) - 1)
      for ((name : String, value : Long) <- curEnd) {
        var (beginDimType : isl.DimType, beginPos : Int) = nameToPos(name)
        constraint = constraint.setCoefficientVal(beginDimType, beginPos, value)
      }
      domain = domain.addConstraint(constraint)

      d += 1
    } while (d < dims)

    loop.indices.annotate(SKIP_ANNOT)
    loop.stepSize.annotate(SKIP_ANNOT)

    scops.push(new SCoP(loop, nameToPos, domain.getSpace().params()))
    template = domain
  }

  private def leaveLoop(loop : LoopOverDimensions) : Unit = {
    if (template != null)
      loop.annotate(PolyOpt.SCOP_ANNOT, scops.top)
    template = null
  }

  private def enterStmt(stmt : Statement) : Unit = {

    val scop = scops.top

    val id : Int = nextId()
    val strId : String = "S" + id
    scop.stmts.put(strId, stmt)

    curStmtDomain = template.setTupleName(strId)

    scop.domain = scop.domain.addSet(curStmtDomain)

    var schedule : isl.BasicMap = isl.BasicMap.identity(curStmtDomain.getSpace().mapFromSet())
    schedule = schedule.add(isl.DimType.Out, 1) // this also drops all names from out (which is required here)
    schedule = schedule.fixVal(isl.DimType.Out, schedule.dim(isl.DimType.Out) - 1, id)

    scop.schedule = scop.schedule.addMap(schedule)
  }

  private def leaveStmt() : Unit = {
    curStmtDomain = null
  }

  private def enterAssign(assign : AssignmentStatement) : Unit = {

    enterStmt(assign) // as an assignment is also a statement

    if (isRead || isWrite) {
      discardCurrentSCoP("nested assignments are not supported (yet...); skipping scop")
      return
    }

    assign.op match {

      case StringConstant("=") =>
        assign.dest.annotate(Access.ANNOT, Some(Access.WRITE))

      case StringConstant("+=") | StringConstant("-=") | StringConstant("*=") | StringConstant("/=") =>
        assign.dest.annotate(Access.ANNOT, Some(Access.UPDATE))

      case _ =>
        discardCurrentSCoP("unrecognized assignment operator: " + assign.op)
        return
    }

    assign.src.annotate(Access.ANNOT, Some(Access.READ))
    assign.op.annotate(SKIP_ANNOT)
  }

  private def leaveAssign(assign : AssignmentStatement) : Unit = {

    leaveStmt() // as an assignment is also a statement
  }

  private def enterScalarAccess(varName : String) : Unit = {

    // hack: filter out code
    var i : Int = varName.length()
    while (i > 0) {
      i -= 1
      varName.charAt(i) match {
        case '=' | '+' | '-' | '*' | '/' =>
          discardCurrentSCoP("expression in StringConstant found: " + varName)
          return
        case _ =>
      }
    }

    val scop : SCoP = scops.top

    val paramDim : Int = curStmtDomain.dim(isl.DimType.Param)
    val space : isl.Space = curStmtDomain.getSpace().fromDomain()
    var access : isl.BasicMap = isl.BasicMap.universe(space)
    access = access.setTupleName(isl.DimType.Out, varName)

    if (isRead)
      scop.reads = scop.reads.addMap(access)
    if (isWrite)
      scop.writes = scop.writes.addMap(access)
  }

  private def leaveScalarAccess() : Unit = {
    // nothing to do here...
  }

  private def enterArrayAccess(name : String, index : Expression) : Unit = {

    val mIndex : MultiIndex = index match {
      case mInd : MultiIndex => mInd
      case _                 => null
    }

    val scop : SCoP = scops.top

    val fieldDim : Int = if (mIndex != null) Knowledge.dimensionality else 1
    val space : isl.Space = curStmtDomain.getSpace().fromDomain().addDims(isl.DimType.Out, fieldDim)
    var access : isl.BasicMap = isl.BasicMap.universe(space)
    access = access.setTupleName(isl.DimType.Out, name)

    var i : Int = 0
    do {
      val aff : HashMap[String, Long] =
        try {
          SimplifyExpression.evalIntegralAffine(if (mIndex != null) mIndex(i) else index)
        } catch {
          case EvaluationException(msg) =>
            discardCurrentSCoP("evaluating access expression: " + msg)
            return
        }

      var constr : isl.Constraint = isl.Constraint.equalityAlloc(access.getLocalSpace())
      constr = constr.setCoefficientVal(isl.DimType.Out, i, NEG_ONE)
      val const = aff.remove(SimplifyExpression.constName)
      if (const.isDefined)
        constr = constr.setConstantVal(const.get)
      for ((name : String, value : Long) <- aff) {
        var (dimType : isl.DimType, pos : Int) = scop.nameToPos(name)
        if (dimType == isl.DimType.Set)
          dimType = isl.DimType.In
        constr = constr.setCoefficientVal(dimType, pos, value)
      }
      access = access.addConstraint(constr)

      i += 1
    } while (i < fieldDim)

    if (isRead)
      scop.reads = scop.reads.addMap(access)
    if (isWrite)
      scop.writes = scop.writes.addMap(access)
  }

  private def leaveArrayAccess() {
    // nothing to do here...
  }

  private def enterFieldAccess(owner : Expression, field : Field, slot : Expression, index : MultiIndex) : Unit = {

    owner.annotate(SKIP_ANNOT)
    field.annotate(SKIP_ANNOT)
    slot.annotate(SKIP_ANNOT)

    val name : StringBuilder = new StringBuilder

    owner.cppsb(name)
    field.codeName.cppsb(name)
    //    name += '_'
    name += '['
    slot.cppsb(name)
    name += ']'

    //    var i : Int = 0
    //    while (i < name.length) {
    //      name(i) match {
    //        case '.' | '[' | ']' | '(' | ')' => name(i) = '_'
    //        case _                           =>
    //      }
    //      i += 1
    //    }

    enterArrayAccess(name.toString(), index)
  }

  private def leaveFieldAccess(owner : Expression, field : Field, slot : Expression) : Unit = {
    leaveArrayAccess()
  }
}
