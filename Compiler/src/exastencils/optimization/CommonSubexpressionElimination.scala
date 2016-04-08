package exastencils.optimization

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
import exastencils.core.collectors.Collector
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PrettyPrintable
import exastencils.strategies.SimplifyStrategy

object CommonSubexpressionElimination extends CustomStrategy("Common subexpression elimination") {
  /////////////////////// DEBUG \\\\\\\\\\\\\\\\\\\\\\\\\\\
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
  //\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\

  private final val REPLACE_ANNOT : String = "CSE_RDrepl"
  private final val REMOVE_ANNOT : String = "CSE_RDrem"

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val scopes = new ArrayBuffer[(String, Node, () => ListBuffer[Statement])]() // actual node and shortcut to its body buffer
    var curFunc : String = null
    this.execute(new Transformation("find and extract relevant scopes", {
      case f : FunctionStatement =>
        curFunc = f.name
        f
      case l : ForLoopStatement with OptimizationHint if (l.isInnermost) =>
        scopes += ((curFunc, l, l.body _))
        l
      case l : LoopOverDimensions =>
        scopes += ((curFunc, l, l.body _))
        l
    }))

    Logger.debug(s"Perform common subexpression elimination on ${scopes.length} scopes...")
    val bak = Logger.getLevel
    Logger.setLevel(Logger.WARNING) // be quiet! (R)
    for ((curFunc, parent, body) <- scopes) {
      // inline declarations with no additional write to the same variable first (CSE afterwards may find larger CSes)
      inlineDecls(parent, body())
      conventionalCSE(curFunc, body())
    }
    Logger.setLevel(bak) // restore verbosity
    Logger.debug(" ... done")

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private def inlineDecls(parent : Node, body : ListBuffer[Statement]) : Unit = {
    val accesses = new HashMap[String, (VariableDeclarationStatement, Buffer[Expression])]()
    this.execute(new Transformation("find removable declarations", {
      case decl @ VariableDeclarationStatement(_ : ScalarDatatype, name, Some(init)) =>
        accesses(name) = (decl, new ArrayBuffer[Expression]())
        decl
      case ass @ AssignmentStatement(VariableAccess(name, _), _, _) =>
        accesses.remove(name)
        ass
      case inc @ PreDecrementExpression(VariableAccess(name, _)) =>
        accesses.remove(name)
        inc
      case inc @ PreIncrementExpression(VariableAccess(name, _)) =>
        accesses.remove(name)
        inc
      case inc @ PostIncrementExpression(VariableAccess(name, _)) =>
        accesses.remove(name)
        inc
      case inc @ PostDecrementExpression(VariableAccess(name, _)) =>
        accesses.remove(name)
        inc
      case acc @ VariableAccess(name, _) =>
        for ((_, uses) <- accesses.get(name))
          uses += acc
        acc
    }), Some(Scope(body))) // restrict to body only

    for ((_, (decl, uses)) <- accesses) {
      decl.annotate(REMOVE_ANNOT)
      val init = decl.expression.get
      for (use <- uses)
        use.annotate(REPLACE_ANNOT, Duplicate(init))
    }

    this.execute(new Transformation("inline removable declarations", {
      case n if (n.hasAnnotation(REMOVE_ANNOT))  => List()
      case n if (n.hasAnnotation(REPLACE_ANNOT)) => n.getAnnotation(REPLACE_ANNOT).get.asInstanceOf[Node]
    }, false), Some(parent)) // modifications in a list result in a new list created, so work with original parent and not with body

    if (!accesses.isEmpty)
      SimplifyStrategy.doUntilDoneStandalone(parent, true)
  }

  private def conventionalCSE(curFunc : String, body : ListBuffer[Statement]) : Unit = {
    var repeat : Boolean = false
    do {
      val coll = new CollectBaseCSes(curFunc)
      this.register(coll)
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(Scope(body)))
      this.unregister(coll)
      val commonSubs : HashMap[Node, Subexpression] = coll.commonSubs
      commonSubs.retain { (_, cs) => cs != null && cs.getPositions().size > 1 }
      if (!commonSubs.isEmpty) {
        findCommonSubs(commonSubs)
        repeat = updateAST(body, commonSubs)
      } else
        repeat = false
    } while (repeat)
  }

  private def findCommonSubs(commonSubs : HashMap[Node, Subexpression]) : Unit = {
    val processedChildren = new java.util.IdentityHashMap[Any, Null]()
    val func : String = commonSubs.valuesIterator.next().func
    var nju : ArrayBuffer[List[Node]] = commonSubs.view.flatMap { x => x._2.getPositions() }.to[ArrayBuffer]
    val njuCommSubs = new HashMap[Node, Subexpression]()
    def registerCS(node : Expression with Product, weight : Int, pos : List[Node], recurse : Boolean, children : Seq[Any]) : Unit = {
      if (njuCommSubs.getOrElseUpdate(node, new Subexpression(func, node, weight)).addPosition(pos)) {
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
            case func : FunctionCallExpression =>
              if (!func.name.contains("std::rand")) { // HACK to prevent inlining call to std::rand
                val childCSes = func.arguments.view.map { e => commonSubs.get(e) }
                if (childCSes.forall(_.isDefined))
                  registerCS(func, childCSes.map(_.get.weight).sum + 1, pos, true, List.empty)
              }

            case parent : Product =>
              val (prods, buffs, Nil, _) = splitIt3[Product, Buffer[AnyRef], Seq[_]](parent.productIterator)
              val nrProds = prods.length
              val nrBuffs = buffs.length

              if (nrProds <= 2 && nrBuffs == 0) {
                val childCSes = prods.view.collect({ case n : Node => commonSubs.get(n) })
                if (childCSes.forall(_.isDefined))
                  registerCS(parent, childCSes.map(_.get.weight).sum, pos, true, prods)

                // this is the only one, that may create new instances for subexpression witnesses,
                //   this must be in sync with Subexpression.getRepls below
              } else if (nrProds == 0 && nrBuffs == 1) {
                val dupParents : Array[(Expression with Product, Buffer[PrettyPrintable])] =
                  duplicateAndFill(parent, { case n : Node => commonSubs.contains(n); case _ => false })
                for ((dupPar, dupParChildren) <- dupParents)
                  registerCS(dupPar, dupParChildren.view.map { case x : Node => commonSubs(x).weight }.sum, pos, dupPar eq parent, dupParChildren)

              } else
                Logger.warn("  wat?! unexpected number/type of children:  " + par)

            case _ =>
              Logger.warn("  wat?! node type is no Product:  " + par)
          }

      for ((key, value) <- njuCommSubs.view.filter { case (_, sExpr) => sExpr.getPositions().size > 1 }) {
        val old = commonSubs.get(key)
        if (old.isEmpty)
          commonSubs.put(key, value)
      }
      njuCommSubs.clear()
      processedChildren.clear() // we can clear the set of processed nodes, they cannot appear in nju again (this keeps the size of the map small)
    } while (!nju.isEmpty)
  }

  private def updateAST(body : ListBuffer[Statement], commSubs : HashMap[Node, Subexpression]) : Boolean = {
    val (_, commSub) = commSubs.toList.sortBy { x => (-x._2.weight, -x._2.getPositions().size, x._2.ppString) }.head
    if (commSub.weight <= 1)
      return false

    val REPL_ANNOT : String = "CSE_Rep"
    var repl : Boolean = false
    for ((old, nju) <- commSub.getRepls()) {
      old.annotate(REPL_ANNOT, nju)
      repl = true
    }
    if (repl)
      this.execute(new Transformation("replace common subexpressions", {
        case x if (x.hasAnnotation(REPL_ANNOT)) => x.removeAnnotation(REPL_ANNOT).get.asInstanceOf[Node]
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
    * 							 `orig.productIterator` must contain a single `Buffer[PrettyPrintable]`, whose elements will be sorted.
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
}

private class CollectBaseCSes(curFunc : String) extends StackCollector {

  private final val SKIP_ANNOT : String = "CSE_skip"
  private var skip : Boolean = false

  val commonSubs = new HashMap[Node, Subexpression]()

  override def enter(node : Node) : Unit = {
    super.enter(node) // adds current node to the stack

    if (skip)
      return

    node match {
      case VariableDeclarationStatement(dt, name, _) =>
        commonSubs(VariableAccess(name, Some(dt))) = null
      case AssignmentStatement(vAcc : VariableAccess, _, _) =>
        commonSubs(vAcc) = null
      case AssignmentStatement(ArrayAccess(vAcc : VariableAccess, _, _), _, _) =>
        commonSubs(vAcc) = null
      case AssignmentStatement(ArrayAccess(iv : iv.InternalVariable, _, _), _, _) =>
        commonSubs(iv) = null
      case AssignmentStatement(dfa : DirectFieldAccess, _, _) =>
        commonSubs(dfa) = null
      case AssignmentStatement(tba : TempBufferAccess, _, _) =>
        commonSubs(tba) = null

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
        val cs = commonSubs.getOrElseUpdate(node, new Subexpression(curFunc, node.asInstanceOf[Expression with Product]))
        if (cs != null)
          cs.addPosition(stack.elems) // extract internal (immutable) list from stack

        // skip subtree of this node
        node.annotate(SKIP_ANNOT)
        skip = true

      case _ =>
      // nothing to do
    }
  }

  override def leave(node : Node) : Unit = {
    super.leave(node) // removes current node from stack

    if (node.removeAnnotation(SKIP_ANNOT).isDefined)
      skip = false
  }

  override def reset() : Unit = {
    super.reset()
    commonSubs.clear()
    skip = false
  }
}

object Subexpression {
  private final val nameTempl : String = "_ce%03d"
  private var counter = new HashMap[String, Int]

  def getNewName(func : String) : String = {
    val c = counter.getOrElse(func, 0)
    counter(func) = c + 1
    return nameTempl.format(c)
  }
}

private class Subexpression(val func : String, val witness : Expression with Product, val weight : Int = 1) {
  private val positions = new java.util.IdentityHashMap[List[Node], Any]()

  private lazy val tmpVarDatatype : Datatype = RealDatatype // FIXME: make generic!
  private lazy val tmpVarName : String = Subexpression.getNewName(func)

  lazy val declaration = VariableDeclarationStatement(tmpVarDatatype, tmpVarName, Some(witness))
  lazy val ppString : String = witness.prettyprint()

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
          // according to the matching above (in findCommSubs), this expression must have a single Buffer child
          val allChildren = oldExpr.productIterator.find { x => x.isInstanceOf[Buffer[_]] }.get.asInstanceOf[Buffer[Any]]
          val commSubsChildren = witness.productIterator.find { x => x.isInstanceOf[Buffer[_]] }.get.asInstanceOf[Buffer[Any]]
          // according to the generation of witnesses children above, both buffers have the same ordering
          allChildren --= commSubsChildren
          allChildren += VariableAccess(tmpVarName, Some(tmpVarDatatype))
          null // no need to replace node, since its children were already modified
        }
    }.filter { x => x != null }
  }
}
