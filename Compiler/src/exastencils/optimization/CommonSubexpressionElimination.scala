package exastencils.optimization

import scala.annotation.tailrec
import scala.collection.convert.Wrappers.JSetWrapper
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.reflect.ClassTag
import scala.util.Sorting

import exastencils.core.Duplicate
import exastencils.core.Settings
import exastencils.core.StateManager
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PrettyPrintable
import exastencils.strategies.SimplifyStrategy

object CommonSubexpressionElimination extends CustomStrategy("Common subexpression elimination") {
  import scala.language.implicitConversions

  implicit def intCst(i : Int) = IntegerConstant(i)
  implicit def floatCst(f : Double) = FloatConstant(f)
  implicit def varAcc(s : String) = VariableAccess(s, Some(RealDatatype))
  def arr1(i : Expression) = ArrayAccess(VariableAccess("a1", Some(PointerDatatype(RealDatatype))), i)
  def x = VariableAccess("x", Some(RealDatatype))
  def i = VariableAccess("i", Some(IntegerDatatype))
  def b = VariableAccess("b", Some(RealDatatype))

  def main(args : Array[String]) : Unit = {
    val scope = new Scope()
    var body : ListBuffer[Statement] = null
    def newLoop() = {
      val loop = new ForLoopStatement(NullStatement, NullExpression, NullStatement, new ListBuffer[Statement]()) with OptimizationHint
      loop.isInnermost = true
      scope.body += loop
      body = loop.body
    }

    newLoop()
    body += ReturnStatement(Some(x * x + x + 3 + ((b + i) * (x * x + x + i + 3))))
    body += AssignmentStatement(x, 42, "=")
    body += ReturnStatement(Some(x * x + x + 3 + ((b + i) * (x * x + x + i + 3))))
    newLoop()
    body += ReturnStatement(Some(x * x + x + 3 + ((b + i) * (x * x + x + i + 3))))
    body += AssignmentStatement(b, 42, "=")
    body += ReturnStatement(Some(x * x + x + 3 + ((b + i) * (x * x + x + i + 3))))
    newLoop()
    body += ReturnStatement(Some(3 + x + x * x + (b * (x * x + (9 * x) + 3))))
    newLoop()
    body += ReturnStatement(Some(arr1(i * 2 + 3) + arr1(i * 2 + 3) + arr1(i * 2 + 4)))
    newLoop()
    StateManager.root_ = scope
    SimplifyStrategy.doUntilDone()
    println(scope)
    println(scope.prettyprint())
    CommonSubexpressionElimination.apply()
    println(scope)
    println(scope.prettyprint())
  }

  def main2(args : Array[String]) : Unit = {
    val orig = AdditionExpression(ListBuffer(x, x, 1, 2, 3))
    val r1 = CommonSubexpressionElimination.duplicateAndFill(orig, { n => n != x })
    val singletons = new HashSet[Node]()
    for ((r, _) <- r1)
      if (singletons.add(r))
        println(r)
  }

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val bak = Logger.getLevel
    Logger.setLevel(Logger.WARNING)
    this.applyRec()
    Logger.setLevel(bak)

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  @tailrec
  private def applyRec() : Unit = {
    val coll = new CollectBaseCSes()
    this.register(coll)
    this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty))
    this.unregister(coll)

    var repeat : Boolean = false
    for ((body, commSubs) <- coll.scopes) {
      findCommSubs(commSubs)
      //      println("------------------------------")
      //      for ((_, x) <- commSubs.toSeq.sortBy(_._2.weight)) {
      //        println(s"${x.weight}:  ${x.getPositions().size}  -  ${x.witness.prettyprint()}")
      //      }
      repeat |= updateAST(body, commSubs)
    }

    if (repeat)
      return this.applyRec() // return is unnecessay, but just to ensure compiler recognizes the tail recursion ;)
  }

  private def findCommSubs(commSubs : HashMap[Node, Subexpression]) : Unit = {
    val processedChildren = new java.util.IdentityHashMap[Any, Null]()
    var nju : ArrayBuffer[List[Node]] = commSubs.view.flatMap { x => x._2.getPositions() }.to[ArrayBuffer]
    val njuCommSubs = new HashMap[Node, Subexpression]()
    def registerCS(node : Expression with Product, weight : Int, pos : List[Node], recurse : Boolean, children : Seq[Any]) : Unit = {
      if (njuCommSubs.getOrElseUpdate(node, new Subexpression(node, weight)).addPosition(pos)) {
        for (child <- children)
          processedChildren.put(child, null)
        if (recurse)
          nju += pos
      }
    }

    do {
      val toProc = nju
      nju = new ArrayBuffer[List[Node]]()
      for (c :: (pos @ par :: _) <- toProc) // orignal head was the common expression itself
        // only process this node if not already done (based on ref eq)
        if (!processedChildren.containsKey(c) && par.isInstanceOf[Expression])
          par.asInstanceOf[Expression] match {
            case parent : Product =>
              val (prods, buffs, Nil, _) = splitIt3[Product, Buffer[AnyRef], Seq[_]](parent.productIterator)
              val nrProds = prods.length
              val nrBuffs = buffs.length

              if (nrProds <= 2 && nrBuffs == 0) {
                val childCSes = prods.view.collect({ case n : Node => commSubs.get(n) })
                if (childCSes.forall(_.isDefined))
                  registerCS(parent, childCSes.map(_.get.weight).sum, pos, true, prods)

                // this is the only one, that may create new instances for subexpression witnesses,
                //   this must be in sync with Subexpression.initDeclRepl below
              } else if (nrProds == 0 && nrBuffs == 1) {
                if (parent.isInstanceOf[FunctionCallExpression]) {
                  val childCSes = buffs.head.view.collect({ case n : Node => commSubs.get(n) })
                  if (childCSes.forall(_.isDefined))
                    registerCS(parent, childCSes.map(_.get.weight).sum + 1, pos, true, prods)
                } else {
                  val dupParents : Array[(Expression with Product, Buffer[PrettyPrintable])] =
                    duplicateAndFill(parent, { case n : Node => commSubs.contains(n); case _ => false })
                  for ((dupPar, dupParChildren) <- dupParents)
                    registerCS(dupPar, dupParChildren.view.map { case x : Node => commSubs(x).weight }.sum, pos, dupPar eq parent, dupParChildren)
                }

              } else
                Logger.warn("  wat?! unexpected number/type of children:  " + par)

            case _ =>
              Logger.warn("  wat?! node type is no Product:  " + par)
          }

      for ((key, value) <- njuCommSubs.view.filter { case (_, sExpr) => sExpr.getPositions().size > 1 }) {
        val old = commSubs.get(key)
        if (old.isEmpty)
          commSubs.put(key, value)
      }
      njuCommSubs.clear()
      processedChildren.clear() // we can clear the set of processed nodes, they cannot appear in nju again (this keeps the size of the map small)
    } while (!nju.isEmpty)
  }

  private def updateAST(body : ListBuffer[Statement], commSubs : HashMap[Node, Subexpression]) : Boolean = {
    val (_, commSub) = commSubs.toList.sortBy { x => -x._2.weight }.head
    if (commSub.weight <= 1)
      return false

    val replAnnot : String = "CSE_Rep"
    var repl : Boolean = false
    for ((old, nju) <- commSub.getRepls()) {
      old.annotate(replAnnot, nju)
      repl = true
    }
    if (repl)
      this.execute(new Transformation("replace common subexpressions", {
        case x if (x.hasAnnotation(replAnnot)) => x.removeAnnotation(replAnnot).get.asInstanceOf[Node]
      }), Some(Scope(body)))

    // add declaration after transformation to prevent modifying it
    commSub.declaration +=: body
    return true
  }

  private def splitIt3[A : ClassTag, B : ClassTag, C : ClassTag](it : TraversableOnce[_]) : (Seq[A], Seq[B], Seq[C], Seq[Any]) = {

    val listA = new ListBuffer[A]()
    val listB = new ListBuffer[B]()
    val listC = new ListBuffer[C]()
    val listZ = new ListBuffer[Any]()
    for (obj <- it) obj match {
      case a : A => listA += a
      case b : B => listB += b
      case c : C => listC += c
      case any   => listZ += any
    }
    return (listA, listB, listC, listZ)
  }

  /**
    * @param orig  	 The original element to be duplicated and filled.
    * 							 `orig.productElement(0)` must be a `Buffer[PrettyPrintable]`, whose elements will be sorted.
    */
  private def duplicateAndFill[T <: Product : ClassTag](orig : T, relevant : Any => Boolean) : Array[(T, Buffer[PrettyPrintable])] = {

    // every child here should be PrettyPrintable
    val children : Buffer[PrettyPrintable] = orig.productIterator.find { x => x.isInstanceOf[Buffer[_]] }.get.asInstanceOf[Buffer[PrettyPrintable]]

    // make prettyprint explicit to prevent calling it several times
    val interm : Seq[(PrettyPrintable, String)] = children.view.map { x => (x, x.prettyprint()) }

    // sort according to prettyprinted version
    val sortedO : Array[(PrettyPrintable, String)] = Sorting.stableSort(interm, { x => x._2 })
    val sortedF = sortedO.view.map { x => x._1 }.filter(relevant).toBuffer

    val len = sortedF.length
    val filtered : Boolean = len != children.length
    if (len == 1 && !filtered)
      return Array((orig, children))
    if (len < 2)
      return new Array[(T, Buffer[PrettyPrintable])](0) // too few relevant expressions remaining... exit

    // duplicate (empty) original node (to create nodes for alternatives) and store a reference of its children buffer
    children.clear()
    val twoPowLen = 1 << len
    val nrCombs = twoPowLen - 1 - len
    val dups = new Array[(T, Buffer[PrettyPrintable])](nrCombs)
    val start =
      if (!filtered) {
        dups(0) = (orig, children) // if we are not able to keep all children, do NOT use orignal reference
        1
      } else
        0
    for (i <- start until nrCombs) {
      val dup = Duplicate(orig)
      val dupChildren = dup.productElement(0).asInstanceOf[Buffer[PrettyPrintable]]
      dups(i) = (dup, dupChildren)
    }

    // restore orig if required (but sorted; this must be in sync with Subexpression.initReplDecl below)
    if (filtered)
      children ++= sortedO.view.map(_._1)

    // fill alternatives with elements from powerset
    val toSet = (twoPowLen >> 1) - 1
    val startInds = new ArrayBuffer[Int](toSet + 1)
    startInds += 0
    for (child : PrettyPrintable <- sortedF) {
      val nrInds = startInds.length
      var s = 0
      for (i <- 0 until nrInds) {
        var ls = startInds(i)
        val locToSet = ls + (toSet + 1) / nrInds
        while (ls < locToSet && s < toSet) {
          dups(ls)._2 += child
          ls += 1
          s += 1
        }
        startInds += ls
      }
    }
    return dups
  }

  private final class CollectBaseCSes extends StackCollector {

    val scopes = new ListBuffer[(ListBuffer[Statement], HashMap[Node, Subexpression])]()

    private final val markerAnnot : String = "CSE_CM"

    private final val SEARCH_SCOPE : Int = 1
    private final val IN_SCOPE : Int = 2
    private final val SEARCH_CS : Int = 3
    private final val SKIP : Int = 4

    private var state : Int = SEARCH_SCOPE

    private var innerBody : ListBuffer[Statement] = null
    private var commSubs : HashMap[Node, Subexpression] = null

    override def enter(node : Node) : Unit = state match {
      case SEARCH_SCOPE =>
        def processInnermost(body : ListBuffer[Statement]) : Unit = {
          for (s <- body)
            s.annotate(markerAnnot)
          innerBody = body
          commSubs = new HashMap[Node, Subexpression]()
          node.annotate(markerAnnot)
          state = IN_SCOPE
        }
        node match {
          case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
            processInnermost(l.body)
          case l : LoopOverDimensions =>
            processInnermost(l.body)
          case _ =>
          // nothing to do
        }

      case IN_SCOPE =>
        if (node.hasAnnotation(markerAnnot)) {
          state = SEARCH_CS
          this.enter(node) // execute code for new state immediatly
        }

      case SEARCH_CS =>
        super.enter(node) // adds current node to the stack
        node match {
          case VariableDeclarationStatement(dt, name, _) =>
            commSubs(VariableAccess(name, Some(dt))) = null
          case AssignmentStatement(vAcc : VariableAccess, _, _) =>
            commSubs(vAcc) = null
          case AssignmentStatement(ArrayAccess(vAcc : VariableAccess, _, _), _, _) =>
            commSubs(vAcc) = null
          case AssignmentStatement(ArrayAccess(iv : iv.InternalVariable, _, _), _, _) =>
            commSubs(iv) = null
          case AssignmentStatement(dfa : DirectFieldAccess, _, _) =>
            commSubs(dfa) = null
          case AssignmentStatement(tba : TempBufferAccess, _, _) =>
            commSubs(tba) = null

          case _ : IntegerConstant
            | _ : FloatConstant
            | _ : BooleanConstant
            | _ : VariableAccess
            | _ : StringLiteral
            | _ : ArrayAccess
            | _ : DirectFieldAccess
            | _ : TempBufferAccess
            | _ : iv.InternalVariable
            | _ : ConcatenationExpression //
            =>

            // all matched types are subclasses of Expression and Product
            val cs = commSubs.getOrElseUpdate(node, new Subexpression(node.asInstanceOf[Expression with Product]))
            if (cs != null)
              cs.addPosition(stack.elems) // extract internal (immutable) list from stack

            // skip subtree of this node
            node.annotate(markerAnnot)
            state = SKIP

          case _ =>
          // nothing to do
        }

      case SKIP =>
    }

    override def leave(node : Node) : Unit = state match {
      case SEARCH_SCOPE =>

      case IN_SCOPE =>
        if (node.removeAnnotation(markerAnnot).isDefined) {
          if (!commSubs.retain { (_, cs) => cs != null && cs.getPositions().size > 1 }.isEmpty) {
            scopes += ((innerBody, commSubs))
            innerBody = null
            commSubs = null
          }
          state = SEARCH_SCOPE
        }

      case SEARCH_CS =>
        super.leave(node) // removes current node from stack
        if (node.removeAnnotation(markerAnnot).isDefined)
          state = IN_SCOPE

      case SKIP =>
        if (node.removeAnnotation(markerAnnot).isDefined) {
          super.leave(node) // removes current node from stack
          state = SEARCH_CS
        }
    }

    override def reset() : Unit = {
      super.reset()
      scopes.clear()
      innerBody = null
      commSubs = null
      state = SEARCH_SCOPE
    }
  }

}

object Subexpression {
  private final val nameTempl : String = "_ce%03d"
  private var counter : Int = -1 // increment happens before first usage

  def getNewName() : String = {
    counter += 1
    return nameTempl.format(counter)
  }
}

private class Subexpression(val witness : Expression with Product, val weight : Int = 1) {
  private val positions = new java.util.IdentityHashMap[List[Node], Any]()

  private lazy val tmpVarDatatype : Datatype = RealDatatype // FIXME: make generic!
  private lazy val tmpVarName : String = Subexpression.getNewName()

  lazy val declaration = VariableDeclarationStatement(tmpVarDatatype, tmpVarName, Some(witness))

  def addPosition(nju : List[Node]) : Boolean = {
    return positions.put(nju, nju) == null
  }

  def getPositions() : Set[List[Node]] = {
    return new JSetWrapper(positions.keySet())
  }

  def getRepls() : Traversable[(Expression, Expression)] = {
    return getPositions().view.map {
      posStack : List[Node] =>
        val oldExpr = posStack.head.asInstanceOf[Expression with Product]
        if (witness == oldExpr) { // we can completely replace the subtree
          (oldExpr, VariableAccess(tmpVarName, Some(tmpVarDatatype)))
        } else { // only a part of the n-ary expression can be extracted...
          // according to the matching above (in findCommSubs), this expression must have a single child, which is a Buffer
          val allChildren = oldExpr.productElement(0).asInstanceOf[Buffer[Any]]
          val commSubsChildren = witness.productElement(0).asInstanceOf[Buffer[Any]]
          // according to the generation of witnesses children above, both buffers have the same ordering
          allChildren --= commSubsChildren
          allChildren += VariableAccess(tmpVarName, Some(tmpVarDatatype))
          null
        }
    }.filter { x => x != null }
  }
}
