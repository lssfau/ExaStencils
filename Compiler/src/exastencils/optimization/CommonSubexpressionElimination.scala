package exastencils.optimization

import scala.collection.convert.Wrappers.JSetWrapper
import scala.collection.mutable.{ ArrayBuffer, BitSet, Buffer, HashMap, ListBuffer, Map, Set }
import scala.reflect.ClassTag
import scala.util.Sorting

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.IR_TempBufferAccess
import exastencils.core._
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.knowledge._
import exastencils.logger.Logger
import exastencils.omp.OMP_PotentiallyParallel
import exastencils.prettyprinting._
import exastencils.strategies.SimplifyStrategy
import exastencils.util._

object CommonSubexpressionElimination extends CustomStrategy("Common subexpression elimination") {
  private final val REPLACE_ANNOT : String = "CSE_repl"
  private final val REMOVE_ANNOT : String = "CSE_rem"
  private final val ID_ANNOT : String = "CSE_ID"

  override def apply() : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    // contains function name, actual loop node, (loop iterator, begin, end, and increment)*, and shortcut to its body buffer
    val scopes = new ArrayBuffer[(String, IR_LoopOverDimensions, Array[(String, IR_Expression, IR_Expression, Long)], () => ListBuffer[IR_Statement])]()
    var curFunc : String = null
    this.execute(new Transformation("find and extract relevant scopes", {
      case f : IR_Function           =>
        curFunc = f.name
        f
      case l : IR_LoopOverDimensions =>
        val incr = (0 until l.stepSize.length - Knowledge.opt_loopCarriedCSE_skipOuter).view.map { d =>
          l.stepSize(d) match {
            case IR_IntegerConstant(i) if (i > 0) => (dimToString(d), l.indices.begin(d), l.indices.end(d), i)
            case _                                => null
          }
        }.toArray
        scopes += ((curFunc, l, incr, l.body _))
        l
    }))

    Logger.debug(s"Perform common subexpression elimination on ${ scopes.length } scopes...")
    val orderMap = new HashMap[Any, Int]()
    val bak = Logger.getLevel
    Logger.setLevel(Logger.WARNING) // be quiet! (R)
    for ((curFunc, parentLoop, loopIt, body) <- scopes) {
      // inline declarations with no additional write to the same variable first (CSE afterwards may find larger CSes)
      inlineDecls(parentLoop, body())
      val njuScopes : Seq[ListBuffer[IR_Statement]] =
        if (Knowledge.opt_loopCarriedCSE && parentLoop.condition.isEmpty)
          loopCarriedCSE(curFunc, parentLoop, loopIt, orderMap)
        else
          Nil
      if (Knowledge.opt_conventionalCSE) {
        conventionalCSE(curFunc, body(), orderMap)
        for (njuBody <- njuScopes)
          conventionalCSE(curFunc, njuBody, orderMap)
      }
      removeAnnotations(body())
    }
    Logger.setLevel(bak) // restore verbosity
    Logger.debug(" ... done")

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private def inlineDecls(parent : Node, body : ListBuffer[IR_Statement]) : Unit = {
    val accesses = new HashMap[String, (IR_VariableDeclaration, Buffer[IR_Expression])]()
    val usageIn = new HashMap[String, ListBuffer[String]]().withDefault(_ => new ListBuffer[String]())
    var assignTo : String = null
    this.execute(new Transformation("find removable declarations", {
      case decl @ IR_VariableDeclaration(_ : IR_ScalarDatatype, vName, Some(init)) =>
        accesses(vName) = (decl, new ArrayBuffer[IR_Expression]())
        assignTo = vName
        decl
      case ass @ IR_Assignment(IR_VariableAccess(vName, _), _, _)                  =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        ass
      case inc @ IR_PreDecrementExpression(IR_VariableAccess(vName, _))            =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case inc @ IR_PreIncrementExpression(IR_VariableAccess(vName, _))            =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case inc @ IR_PostIncrementExpression(IR_VariableAccess(vName, _))           =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case inc @ IR_PostDecrementExpression(IR_VariableAccess(vName, _))           =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case acc @ IR_VariableAccess(vName, _)                                       =>
        for ((_, uses) <- accesses.get(vName))
          uses += acc
        usageIn(vName) += assignTo
        acc
    }), Some(IR_Scope(body))) // restrict to body only

    for ((_, (decl, uses)) <- accesses) {
      decl.annotate(REMOVE_ANNOT)
      val init = decl.initialValue.get
      for (use <- uses)
        use.annotate(REPLACE_ANNOT, init) // do not duplicate here, since init could get additional annotations later (which would not be visible, when we copy here)
    }

    this.execute(new Transformation("inline removable declarations", {
      case n if (n.removeAnnotation(REMOVE_ANNOT).isDefined) => List()
      case n if (n.hasAnnotation(REPLACE_ANNOT))             => Duplicate(n.getAnnotation(REPLACE_ANNOT).get.asInstanceOf[Node]) // duplicate here
    }), Some(parent)) // modifications in a list result in a new list created, so work with original parent and not with wrapped body

    SimplifyFloatExpressions.applyStandalone(parent)
    SimplifyStrategy.doUntilDoneStandalone(parent, true)
  }

  private def loopCarriedCSE(curFunc : String, loop : IR_LoopOverDimensions,
      loopIt : Array[(String, IR_Expression, IR_Expression, Long)], orderMap : Map[Any, Int]) : Seq[ListBuffer[IR_Statement]] = {
    if (loopIt == null || loopIt.forall(_ == null))
      return Nil

    // first, number all nodes for overlap-test later
    val currItBody = IR_Scope(loop.body)
    var maxID : Int = 0
    this.execute(new Transformation("number nodes", {
      case _ : ConcatenationExpression =>
        Logger.warn(s"cannot perform loopCarriedCSE, because ConcatenationExpression are too difficult to analyze")
        return Nil // don't do anything, since we cannot ensure the transformation is correct
      // there are some singleton datatypes, so don't enumerate them
      case d : IR_Datatype   => d
      case IR_NullStatement  => IR_NullStatement
      case IR_NullExpression => IR_NullExpression
      case node : Node       => // only enumerate subtypes of Node, all others are irrelevant
        node.annotate(ID_ANNOT, maxID)
        maxID += 1
        node
      case x                 => x // no node, so don't enumerate it, as it may appear multiple times (legally) in the AST
    }), Some(currItBody))
    currItBody.annotate(ID_ANNOT, maxID) // trafo does not get access to the AST root, so set annotation manually

    assert(collectIDs(currItBody, new BitSet(maxID)))

    var foundIDs = new BitSet(maxID)
    val njuScopes = new ArrayBuffer[ListBuffer[IR_Statement]]()
    var bufferCounter : Int = 0

    var tmpBufLen = new Array[IR_Expression](0)
    var tmpBufInd = new Array[IR_Expression](0)
    for (((loopItVar, loopBegin, loopEnd, loopIncr), dim) <- loopIt.zipWithIndex) {
      val prevItBody = IR_Scope(Duplicate(currItBody.body)) // prevItBody does not get an ID (to distinguish between curr and prev)
      this.execute(new Transformation("create previous iteration body", {
        case varAcc : IR_VariableAccess if (varAcc.name == loopItVar) =>
          IR_SubtractionExpression(varAcc, IR_IntegerConstant(loopIncr))
        case strLit : IR_StringLiteral if (strLit.value == loopItVar) =>
          IR_SubtractionExpression(strLit, IR_IntegerConstant(loopIncr))
      }, false), Some(prevItBody))

      SimplifyFloatExpressions.applyStandalone(prevItBody)
      SimplifyStrategy.doUntilDoneStandalone(prevItBody, true)

      val coll = new CollectBaseCSes(curFunc)
      this.register(coll)
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(currItBody))
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(prevItBody))
      this.unregister(coll)
      val commonSubs : Map[Node, Subexpression] = coll.commonSubs
      commonSubs.retain {
        (n, cs) =>
          cs != null &&
            (cs.getPositions().view.map({
              n : List[Node] => n.last.asInstanceOf[Annotatable].hasAnnotation(ID_ANNOT)
            }).toSet.size == 2)
      }

      findCommonSubs(curFunc, commonSubs, orderMap)

      val filteredCS : Array[(Int, String, Subexpression)] =
        commonSubs.view.filter {
          case (_, cs) =>
            cs != null && cs.prio >= 10 && // common subexpression must be large enough to justify the additional memory requirements
              (cs.getPositions().view.map({
                n : List[Node] => n.last.asInstanceOf[Annotatable].hasAnnotation(ID_ANNOT)
              }).toSet.size == 2)
        }.map {
          case (_, cs) => (-(cs.prio + math.min(cs.prio, cs.prioBonus * cs.getPositions().size)), cs.ppString, cs)
        }.toArray
      java.util.Arrays.sort(filteredCS, Ordering.Tuple2[Int, String] on { x : (Int, String, Subexpression) => (x._1, x._2) })

      val decls = new ArrayBuffer[IR_VariableDeclaration]()
      val firstInits = new ListBuffer[IR_Statement]()
      val nextUpdates = new ArrayBuffer[IR_Assignment]()
      for (commonExp <- filteredCS.view.map(_._3)) {
        val ids : BitSet = foundIDs.clone()
        val disjunct : Boolean = commonExp.getPositions().forall { x => collectIDs(x.head, ids) }
        if (disjunct) {
          foundIDs = ids
          for (pos <- commonExp.getPositions()) if (pos.last.hasAnnotation(ID_ANNOT)) {
            val posHead = pos.head.asInstanceOf[IR_Expression with Product]
            val njuExpr : IR_Expression = commonExp.getReplOrModify(posHead)
            if (njuExpr != null)
              this.execute(new Transformation("replace common subexpressions", {
                case x if (x eq posHead) => njuExpr
              }, false), Some(pos(1)))
          }

          var csNext : IR_Expression = Duplicate(commonExp.witness)
          this.execute(new Transformation("create subsequent iteration body", {
            case varAcc : IR_VariableAccess if (varAcc.name == loopItVar) =>
              IR_AdditionExpression(varAcc, IR_IntegerConstant(loopIncr))
            case strLit : IR_StringLiteral if (strLit.value == loopItVar) =>
              IR_AdditionExpression(strLit, IR_IntegerConstant(loopIncr))
          }, false), Some(csNext))
          csNext = SimplifyExpression.simplifyFloatingExpr(csNext)
          val csNextWrap = IR_ExpressionStatement(csNext)
          SimplifyStrategy.doUntilDoneStandalone(csNextWrap, true)
          csNext = csNextWrap.expression

          // FIXME: fix datatypes
          val decl : IR_VariableDeclaration = commonExp.declaration
          val tmpBuf = new iv.LoopCarriedCSBuffer(bufferCounter, decl.datatype, IR_ExpressionIndex(Duplicate(tmpBufLen)))
          bufferCounter += 1
          val tmpBufAcc = new IR_LoopCarriedCSBufferAccess(tmpBuf, IR_ExpressionIndex(Duplicate(tmpBufInd)))
          decl.initialValue = Some(tmpBufAcc)
          decls += decl

          firstInits += new IR_Assignment(Duplicate(tmpBufAcc), Duplicate(commonExp.witness), "=")
          nextUpdates += new IR_Assignment(Duplicate(tmpBufAcc), csNext, "=")
        }
      }

      if (!decls.isEmpty) {
        val (stmts, annots) = loop.at1stIt(dim)
        stmts ++= firstInits
        annots += ((Vectorization.COND_VECTABLE, None))
        if (tmpBufLen.isEmpty)
          annots += ((Vectorization.COND_IGN_INCR, None))
        // prepend to body
        decls ++=:
          nextUpdates ++=:
          loop.body
        njuScopes += stmts
        if (loop.parDims.contains(dim) && loop.isInstanceOf[OMP_PotentiallyParallel])
          loop.isVectorizable = true
        loop.parDims -= dim
        loop.lcCSEApplied = true
      }

      val loopBeginOpt =
        try {
          IR_IntegerConstant(SimplifyExpression.evalIntegralExtrema(loopBegin)._1)
        } catch {
          case ex : EvaluationException => Duplicate(loopBegin)
        }
      val loopEndOpt =
        try {
          IR_IntegerConstant(SimplifyExpression.evalIntegralExtrema(loopEnd)._2)
        } catch {
          case ex : EvaluationException => loopEnd // must not be duplicated, since it is not used elsewhere
        }

      var len = tmpBufInd.length
      tmpBufInd = java.util.Arrays.copyOf(tmpBufInd, len + 1)
      tmpBufInd(len) = IR_VariableAccess(loopItVar, IR_IntegerDatatype) - Duplicate(loopBeginOpt)

      len = tmpBufLen.length
      tmpBufLen = java.util.Arrays.copyOf(tmpBufLen, len + 1)
      tmpBufLen(len) = loopEndOpt - loopBeginOpt
      if (Knowledge.data_alignFieldPointers)
        try {
          val (_, size : Long) = SimplifyExpression.evalIntegralExtrema(tmpBufLen(len))
          val vecSize : Long = Platform.simd_vectorSize
          tmpBufLen(len) = IR_IntegerConstant((size + vecSize) & ~(vecSize - 1))
        } catch {
          case e : EvaluationException => // what a pitty...
        }
    }

    return njuScopes
  }

  private def collectIDs(node : Node, bs : BitSet) : Boolean = {
    var disjunct : Boolean = true
    this.execute(new Transformation("collect IDs", {
      case n =>
        for (id <- n.getAnnotation(ID_ANNOT))
          disjunct &= bs.add(id.asInstanceOf[Int])
        n
    }), Some(IR_Root(ListBuffer(node)))) // wrap to ensure node itself is also accessed by the trafo
    return disjunct
  }

  private def conventionalCSE(curFunc : String, body : ListBuffer[IR_Statement], orderMap : Map[Any, Int]) : Unit = {
    var repeat : Boolean = false
    do {
      val coll = new CollectBaseCSes(curFunc)
      this.register(coll)
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(IR_Scope(body)))
      this.unregister(coll)
      val commonSubs : Map[Node, Subexpression] = coll.commonSubs
      commonSubs.retain { (_, cs) => cs != null && cs.getPositions().size > 1 }
      repeat = false
      if (!commonSubs.isEmpty) {
        findCommonSubs(curFunc, commonSubs, orderMap)
        repeat = updateAST(body, commonSubs)
      }
    } while (repeat)
  }

  private def findCommonSubs(curFunc : String, commonSubs : Map[Node, Subexpression], orderMap : Map[Any, Int]) : Unit = {
    val processedChildren = new java.util.IdentityHashMap[Any, Null]()
    var nju : ArrayBuffer[List[Node]] = commonSubs.view.flatMap { x => x._2.getPositions() }.to[ArrayBuffer]
    val njuCommSubs = new HashMap[Node, Subexpression]()
    def registerCS(node : IR_Expression with Product, prio : Int, prioBonus : Int, pos : List[Node], recurse : Boolean, children : Seq[Any]) : Unit = {
      if (njuCommSubs.getOrElseUpdate(node, new Subexpression(curFunc, node, prio, prioBonus)).addPosition(pos)) {
        for (child <- children)
          processedChildren.put(child, null)
        if (recurse)
          nju += pos
      }
    }

    var todo : ArrayBuffer[List[Node]] = new ArrayBuffer[List[Node]]()
    while (!nju.isEmpty) {
      val tmp = nju
      nju = todo
      todo = tmp
      nju.clear()
      for (c :: (pos @ par :: _) <- todo) // orignal head was the common expression itself
      // only process this node if it is a CS and not already processed (based on ref eq)
        if (commonSubs.contains(c) && !processedChildren.containsKey(c) && par.isInstanceOf[IR_Expression])
          par.asInstanceOf[IR_Expression] match {
            case func : IR_FunctionCall =>
              if (!func.name.contains("std::rand")) {
                // HACK to prevent inlining call to std::rand
                val childCSes = func.arguments.view.map { e => commonSubs.get(e) }
                if (childCSes.forall(_.isDefined))
                  registerCS(func, childCSes.map(_.get.prio).sum + 1, 3, pos, true, List.empty)
              }

            case _ : IR_VectorExpression | _ : IR_MatrixExpression | _ : PrintExpression =>
            // don't do anything, these are never common subexpressions

            case parent : Product =>
              val (prods, buffs, Nil, _) = splitIt3[Product, Buffer[AnyRef], Seq[_]](parent.productIterator)
              val nrProds = prods.length
              val nrBuffs = buffs.length

              if (nrProds <= 2 && nrBuffs == 0) {
                val childCSes = prods.view.collect({ case n : Node => commonSubs.get(n) })
                if (childCSes.forall(_.isDefined))
                  registerCS(parent, childCSes.map(_.get.prio).sum + 1, 1, pos, true, prods)

                // this is the only one, that may create new instances for subexpression witnesses,
                //   this must be in sync with Subexpression.getRepls below
              } else if (nrProds == 0 && nrBuffs == 1) {
                val dupParents : Array[(IR_Expression with Product, Buffer[PrettyPrintable])] =
                  duplicateAndFill(parent, { case n : Node => commonSubs.contains(n); case _ => false }, orderMap)
                for ((dupPar, dupParChildren) <- dupParents)
                  registerCS(dupPar, dupParChildren.view.map { case x : Node => commonSubs(x).prio }.sum + 1, 1, pos, dupPar eq parent, dupParChildren)

              } else
                Logger.warn("  wat?! unexpected number/type of children:  " + par)

            case _ =>
              Logger.warn("  wat?! node type is no Product:  " + par)
          }

      for ((key, value) <- njuCommSubs.view.filter { case (_, sExpr) => sExpr.getPositions().size > 1 })
        if (!commonSubs.contains(key))
          commonSubs.put(key, value)
      njuCommSubs.clear()
      processedChildren.clear() // we can clear the set of processed nodes, they cannot appear in nju again (this keeps the size of the map small)
    }
  }

  private def updateAST(body : ListBuffer[IR_Statement], commSubs : Map[Node, Subexpression]) : Boolean = {
    val commSubSorted = commSubs.toList.sortBy {
      case (_, cs) => (-cs.prio, -cs.getPositions().size, cs.ppString)
    }
    val commSubOpt = commSubSorted.find {
      case (_, cs) => !cs.getPositions().forall { pos => pos.exists { parent => parent.isInstanceOf[IR_IfCondition] } }
    }
    if (commSubOpt.isEmpty)
      return false
    val Some((_, commSub)) = commSubOpt
    if (commSub.prio <= 1)
      return false

    var repl : Boolean = false
    for (pos <- commSub.getPositions()) {
      val oldExpr = pos.head.asInstanceOf[IR_Expression with Product]
      val njuExpr = commSub.getReplOrModify(oldExpr)
      if (njuExpr != null) {
        oldExpr.annotate(REPLACE_ANNOT, njuExpr)
        repl = true
      }
    }
    if (repl)
      this.execute(new Transformation("replace common subexpressions", {
        case x if (x.hasAnnotation(REPLACE_ANNOT)) => x.removeAnnotation(REPLACE_ANNOT).get.asInstanceOf[Node]
      }), Some(IR_Scope(body)))

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
    * @param orig The original element to be duplicated and filled.
    *             `orig.productIterator` must contain a single `Buffer[PrettyPrintable]`, whose elements will be sorted.
    */
  private def duplicateAndFill[T <: Product : ClassTag](orig : T, relevant : Any => Boolean, orderMap : Map[Any, Int]) : Array[(T, Buffer[PrettyPrintable])] = {

    // every child here should be PrettyPrintable
    val children : Buffer[PrettyPrintable] = orig.productIterator.find { x => x.isInstanceOf[Buffer[_]] }.get.asInstanceOf[Buffer[PrettyPrintable]]

    // make prettyprint explicit to prevent calling it several times
    val interm : Seq[(PrettyPrintable, String)] = children.view.map { x => (x, x.prettyprint()) }

    // sort according to prettyprinted version
    val sortedO : Array[(PrettyPrintable, String)] = Sorting.stableSort(interm, { x => (x._2, orderMap.getOrElseUpdate(x._1, { orderMap.size })) }) // orderMap ensures different nodes with same text. rep. are sorted, too!
    val sortedF = sortedO.view.map { x => x._1 }.filter(relevant).toBuffer

    val len : Int = sortedF.length
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
    val cloneStart =
      if (!filtered) {
        dups(0) = (orig, children) // if we are not able to keep all children, do NOT use orignal reference
        1
      } else
        0
    for (i <- cloneStart until nrCombs) {
      val dup = Duplicate(orig)
      val dupChildren = dup.productElement(0).asInstanceOf[Buffer[PrettyPrintable]]
      dups(i) = (dup, dupChildren)
    }

    // restore orig if required (but sorted; this must be in sync with Subexpression.getReplOrModify below)
    if (filtered)
      children ++= sortedO.view.map(_._1)

    // fill alternatives with elements from powerset
    val toSet = (twoPowLen >> 1) - 1
    val startInds = new ArrayBuffer[Int](twoPowLen + 1)
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

  private def removeAnnotations(body : ListBuffer[IR_Statement]) : Unit = {
    this.execute(new Transformation("remove old CSE annotations", {
      // eager `or` is required, since we want to remove ALL, not only the first available
      case node if (node.removeAnnotation(ID_ANNOT).isDefined |
        node.removeAnnotation(REMOVE_ANNOT).isDefined |
        node.removeAnnotation(REPLACE_ANNOT).isDefined) =>
        // match nodes for which we removed something (so the number of nodes modifed is counted by the statemanager)
        node
    }), Some(IR_Scope(body)))
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
      // blacklist concat
      case _ : ConcatenationExpression =>
        skip = true // skip everything from now on...
        commonSubs.clear()
        Logger.warn(s"cannot perform CSE, because ${ node.getClass } is too difficult to analyze")

      case c : IR_IfCondition =>
        c.annotate(SKIP_ANNOT)
        skip = true

      case IR_VariableDeclaration(dt, name, _)                                 =>
        commonSubs(IR_VariableAccess(name, dt)) = null
      case IR_Assignment(vAcc : IR_VariableAccess, _, _)                       =>
        commonSubs(vAcc) = null
      case IR_Assignment(IR_ArrayAccess(vAcc : IR_VariableAccess, _, _), _, _) =>
        commonSubs(vAcc) = null
      case IR_Assignment(IR_ArrayAccess(iv : iv.InternalVariable, _, _), _, _) =>
        commonSubs(iv) = null
      case IR_Assignment(dfa : IR_DirectFieldAccess, _, _)                     =>
        commonSubs(dfa) = null
      case IR_Assignment(tba : IR_TempBufferAccess, _, _)                      =>
        commonSubs(tba) = null

      case _ : IR_IntegerConstant
           | _ : IR_RealConstant
           | _ : IR_BooleanConstant
           | _ : IR_VariableAccess
           | _ : IR_StringLiteral
           | _ : IR_ArrayAccess
           | _ : IR_DirectFieldAccess
           | _ : IR_TempBufferAccess
           | _ : IR_LoopCarriedCSBufferAccess
           | _ : iv.InternalVariable //
      =>

        // all matched types are subclasses of Expression and Product
        val cs = commonSubs.getOrElseUpdate(node, new Subexpression(curFunc, node.asInstanceOf[IR_Expression with Product]))
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
    // commonSubs.clear() // do NOT reset commonSubs, since we want to collect data from several subsequent runs => use new instance for independent runs
    skip = false
  }
}

object Subexpression {
  private val cseCounter = new HashMap[String, Int]()

  def getNewCseName(func : String) : String = {
    val c = cseCounter.getOrElse(func, 0)
    cseCounter(func) = c + 1
    return "_ce%03d".format(c)
  }
}

private class Subexpression(val func : String, val witness : IR_Expression with Product, val prio : Int = 1, val prioBonus : Int = 0) {
  private val positions = new java.util.IdentityHashMap[List[Node], Any]()

  private lazy val tmpVarDatatype : IR_Datatype = IR_RealDatatype
  // FIXME: make generic!
  private lazy val tmpVarName : String = Subexpression.getNewCseName(func)

  lazy val declaration = IR_VariableDeclaration(tmpVarDatatype, tmpVarName, Some(witness))
  lazy val ppString : String = witness.prettyprint()

  def addPosition(nju : List[Node]) : Boolean = {
    return positions.put(nju, this) == null
  }

  def getPositions() : Set[List[Node]] = {
    return new JSetWrapper(positions.keySet())
  }

  def getReplOrModify(old : IR_Expression with Product) : IR_Expression = {
    if (witness == old) { // we can completely replace the subtree
      IR_VariableAccess(tmpVarName, tmpVarDatatype)
    } else {
      // only a part of the n-ary expression can be extracted...
      // according to the matching above (in findCommSubs), this expression must have a single Buffer child
      val allChildren = old.productIterator.find { x => x.isInstanceOf[Buffer[_]] }.get.asInstanceOf[Buffer[Any]]
      val commSubsChildren = witness.productIterator.find { x => x.isInstanceOf[Buffer[_]] }.get.asInstanceOf[Buffer[Any]]
      // according to the generation of witnesses children above, both buffers have the same ordering
      allChildren --= commSubsChildren
      allChildren += IR_VariableAccess(tmpVarName, tmpVarDatatype)
      null // no need to replace node, since its children were already modified
    }
  }
}

case class IR_LoopCarriedCSBufferAccess(var buffer : iv.LoopCarriedCSBuffer, var index : IR_ExpressionIndex) extends IR_Access {
  override def datatype = buffer.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def linearize() : IR_ArrayAccess = {
    if (buffer.dimSizes.isEmpty)
      IR_ArrayAccess(buffer, IR_IntegerConstant(0), Knowledge.data_alignFieldPointers)
    else
      IR_ArrayAccess(buffer, Mapping.resolveMultiIdx(index, buffer.dimSizes), Knowledge.data_alignFieldPointers)
  }
}
