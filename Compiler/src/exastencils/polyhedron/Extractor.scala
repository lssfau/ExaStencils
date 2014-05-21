package exastencils.polyhedron

import scala.collection.mutable.ArrayStack
import scala.language.implicitConversions
import exastencils.core.Logger
import exastencils.core.collectors.Collector
import exastencils.datastructures.Annotation
import exastencils.datastructures.Node
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.AssignmentStatement
import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.Statement
import exastencils.datastructures.ir.StringConstant
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.knowledge.Knowledge
import exastencils.primitives.LoopOverDimensions
import isl.Conversions.convertIntToVal
import exastencils.datastructures.ir.MultiIndex

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
    final val ID : String = "PolyAcc"
    final val READ, WRITE, UPDATE = Value

    exastencils.core.Duplicate.registerImmutable(this.getClass())
  }

  /** current access node is a read/write access */
  private var isRead, isWrite : Boolean = false

  /** integer used to create an identifier for new statements and to determine the ordering of the statements inside the model */
  private var id : Int = 0

  /** all found static control parts */
  val scops = new ArrayStack[SCoP]
  val trash = new ArrayStack[(LoopOverDimensions, String)] // TODO: debug; remove

  /** template for current iteration domain */
  private var template : isl.BasicSet = null

  /////////////////// Collector methods \\\\\\\\\\\\\\\\\\\

  override def enter(node : Node) : Unit = {

    node.getAnnotation(Access.ID) match {
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

    // create new SCoP or replace old
    if (node.isInstanceOf[LoopOverDimensions])
      enterLoop(node.asInstanceOf[LoopOverDimensions])

    else if (template != null) // we have a SCoP now
      node match {
        case a : AssignmentStatement => enterAssign(a)
        // case s : StringConstant      => discardCurrentSCoP("string found (" + s.value + "), unsure about usage, skipping scop")
        case s : Statement           => enterStmt(s) // ensure the general Statement case is below others of its subclasses
        case x : Any                 => Logger.debug("[poly ex]: ignoring " + x.getClass())
      }
  }

  override def leave(node : Node) : Unit = {

    node.getAnnotation(Access.ID) match {
      case Some(Annotation(_, Some(acc))) =>
        isRead = false
        isWrite = false
      case None =>
    }

    node match {
      case l : LoopOverDimensions  => leaveLoop(l)
      case a : AssignmentStatement => leaveAssign(a)
      case s : Statement           => leaveStmt(s) // ensure the general Statement case is below others of its subclasses
      case _                       =>
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

    if (template == null)
      return // no active SCoP to discard, return without action

    if (msg != null)
      Logger.warn("[poly ex] SCoP discarded:  " + msg)
    trash.push((scops.pop().root.asInstanceOf[LoopOverDimensions], msg))
    template = null
  }

  case class EvaluationException(msg : String) extends Exception(msg) {}

  private def evaluateExpression(expr : Expression) : Long = expr match {
    case IntegerConstant(v)                                       => v
    case AdditionExpression(l : Expression, r : Expression)       => evaluateExpression(l) + evaluateExpression(r)
    case SubtractionExpression(l : Expression, r : Expression)    => evaluateExpression(l) - evaluateExpression(r)
    case MultiplicationExpression(l : Expression, r : Expression) => evaluateExpression(l) * evaluateExpression(r)
    case DivisionExpression(l : Expression, r : Expression)       => evaluateExpression(l) / evaluateExpression(r)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }

  /////////////////// methods for node processing \\\\\\\\\\\\\\\\\\\

  private def enterLoop(loop : LoopOverDimensions) : Unit = {

    scops.push(new SCoP(loop))

    if (template != null) {
      discardCurrentSCoP("nested LoopOverDimensions?! possibly a bug? ignoring outermost")
      return
    }
    if (loop.reduction != None) { // TODO: support reductions
      discardCurrentSCoP("reductions not supported yet")
      return
    }

    val dims : Int = Knowledge.dimensionality
    template = isl.BasicSet.universe(isl.Space.setAlloc(0, dims))

    val begin : MultiIndex = loop.indices.begin
    val end : MultiIndex = loop.indices.end

    var geo_dim : Int = 0
    do {
      val loop_dim : Int = dims - geo_dim - 1

      val stride : Expression = loop.stepSize(geo_dim) // TODO: remove restriction
      if (!stride.isInstanceOf[IntegerConstant] || (stride.asInstanceOf[IntegerConstant].value != 1)) {
        discardCurrentSCoP("LoopOverDimensions stepSize/stride must be one (for now...), ignoring loop")
        return
      }

      var cur_begin : Long = 0
      var cur_end : Long = 0
      try {
        cur_begin = evaluateExpression(begin(geo_dim))
        cur_end = evaluateExpression(end(geo_dim))
      } catch {
        case EvaluationException(msg) =>
          discardCurrentSCoP(msg)
          return
      }

      var constr : isl.Constraint = null

      // begin <= i  -->  (1)*i + (-begin) >= 0
      constr = isl.Constraint.inequalityAlloc(template.getLocalSpace())
      constr = constr.setCoefficientVal(isl.DimType.Set, loop_dim, POS_ONE)
      constr = constr.setConstantVal(-cur_begin)
      template = template.addConstraint(constr)

      // i < end  -->  (-1)*i + (end-1) >= 0
      constr = isl.Constraint.inequalityAlloc(template.getLocalSpace())
      constr = constr.setCoefficientVal(isl.DimType.Set, loop_dim, NEG_ONE)
      constr = constr.setConstantVal(cur_end-1)
      template = template.addConstraint(constr)

      geo_dim += 1
    } while (geo_dim < dims)
  }

  private def leaveLoop(loop : LoopOverDimensions) : Unit = {
    if (template != null)
      loop.annotate(PolyOpt.SCOP_ANNOT, Some(scops.top))
    template = null
  }

  private def enterAssign(assign : AssignmentStatement) : Unit = {

    enterStmt(assign) // as an assignment is also a statement

    if (isRead || isWrite) {
      discardCurrentSCoP("nested assignments are not supported (yet...); skipping scop")
      return
    }

    assign.op match {

      case StringConstant("=") =>
        assign.dest.annotate(Access.ID, Some(Access.WRITE))

      case StringConstant("+=") | StringConstant("-=") | StringConstant("*=") | StringConstant("/=") =>
        assign.dest.annotate(Access.ID, Some(Access.UPDATE))

      case _ =>
        discardCurrentSCoP("unrecognized assignment operator: " + assign.op)
        return
    }

    assign.src.annotate(Access.ID, Some(Access.READ))
  }

  private def leaveAssign(assign : AssignmentStatement) : Unit = {

    leaveStmt(assign) // as an assignment is also a statement

    assign.dest.removeAnnotation(Access.ID)
    assign.src.removeAnnotation(Access.ID)
  }

  private def enterStmt(stmt : Statement) : Unit = {

    val scop = scops.top

    val id : Int = nextId()
    val strId : String = "S" + id
    scop.stmts.put(strId, stmt)

    var domain : isl.BasicSet = template
    domain = domain.setTupleName(strId)

    scop.domain = scop.domain.addSet(domain)

    val dim : Int = domain.dim(isl.DimType.Set)
    val space : isl.Space = domain.getSpace().mapFromDomainAndRange(isl.Space.setAlloc(domain.dim(isl.DimType.Param), dim + 1))
    var schedule : isl.BasicMap = isl.BasicMap.universe(space)
    var constr : isl.Constraint = null

    var d = 0
    do {

      constr = isl.Constraint.equalityAlloc(schedule.getLocalSpace())
      constr = constr.setCoefficientVal(isl.DimType.In, d, POS_ONE)
      constr = constr.setCoefficientVal(isl.DimType.Out, d, NEG_ONE)
      schedule = schedule.addConstraint(constr)

      d += 1
    } while (d < dim)

    constr = isl.Constraint.equalityAlloc(schedule.getLocalSpace())
    constr = constr.setCoefficientVal(isl.DimType.Out, Knowledge.dimensionality, NEG_ONE)
    constr = constr.setConstantVal(id)
    schedule = schedule.addConstraint(constr)

    scop.schedule = scop.schedule.addMap(schedule)
  }

  private def leaveStmt(stmt : Statement) : Unit = {
    // anything to do here? I guess not...
  }
}
