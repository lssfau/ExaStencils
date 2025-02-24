//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.optimization.ir

import scala.collection.mutable.AbstractMap
import scala.collection.mutable.{ ArrayBuffer, BitSet, Buffer, HashMap, ListBuffer, Map }
import scala.reflect.ClassTag
import scala.util.Sorting

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir.IR_TempBufferAccess
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.fieldlike.ir.IR_DirectFieldLikeAccess
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.polyhedron.IR_PolyArrayAccessLike
import exastencils.prettyprinting._
import exastencils.scheduling.NoStrategyWrapper
import exastencils.util.DuplicateNodes
import exastencils.util.ir._

object IR_CommonSubexpressionElimination extends CustomStrategy("Common subexpression elimination") {
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
            case IR_IntegerConstant(i) if i > 0 => (IR_FieldIteratorAccess(d).name, l.indices.begin(d), l.indices.end(d), i)
            case _                              => null
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

  private def inlineDecls(parent : IR_Node, body : ListBuffer[IR_Statement]) : Unit = {
    val accesses = new HashMap[String, (IR_VariableDeclaration, Buffer[IR_Expression])]()
    val usageIn = new HashMap[String, ListBuffer[String]]().withDefault(_ => new ListBuffer[String]())
    var assignTo : String = null
    this.execute(new Transformation("find removable declarations", {
      case decl @ IR_VariableDeclaration(_ : IR_ScalarDatatype, vName, Some(init), _) =>
        accesses(vName) = (decl, new ArrayBuffer[IR_Expression]())
        assignTo = vName
        decl
      case ass @ IR_Assignment(IR_VariableAccess(vName, _), _, _)                     =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        ass
      case inc @ IR_PreDecrement(IR_VariableAccess(vName, _))                         =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case inc @ IR_PreIncrement(IR_VariableAccess(vName, _))                         =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case inc @ IR_PostIncrement(IR_VariableAccess(vName, _))                        =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case inc @ IR_PostDecrement(IR_VariableAccess(vName, _))                        =>
        accesses.remove(vName)
        for (declName <- usageIn(vName))
          accesses.remove(declName)
        usageIn(vName).clear()
        assignTo = vName
        inc
      case acc @ IR_VariableAccess(vName, _)                                          =>
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
      case n if n.removeAnnotation(REMOVE_ANNOT).isDefined => List()
      case n if n.hasAnnotation(REPLACE_ANNOT)             => Duplicate(n.getAnnotation(REPLACE_ANNOT).get.asInstanceOf[IR_Node]) // duplicate here
    }), Some(parent)) // modifications in a list result in a new list created, so work with original parent and not with wrapped body

    IR_SimplifyFloatExpressions.applyStandalone(parent)
    IR_GeneralSimplify.doUntilDoneStandalone(parent, true)
  }

  private def loopCarriedCSE(curFunc : String, loop : IR_LoopOverDimensions,
      loopIt : Array[(String, IR_Expression, IR_Expression, Long)], orderMap : Map[Any, Int]) : Seq[ListBuffer[IR_Statement]] = {
    if (loopIt == null || loopIt.forall(_ == null))
      return Nil

    // first, number all nodes for overlap-test later
    val currItBody = IR_Scope(loop.body)
    var maxID : Int = 0
    this.execute(new Transformation("number nodes", {
      case n : IR_Node if Duplicate.constants.contains(n) => n // ignore constants, e.g. case objects
      case node : IR_Node                                 => // only enumerate subtypes of Node, all others are irrelevant
        node.annotate(ID_ANNOT, maxID)
        maxID += 1
        node
      case x                                              => x // no node, so don't enumerate it, as it may appear multiple times (legally) in the AST
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
        case varAcc : IR_VariableAccess if varAcc.name == loopItVar =>
          IR_Subtraction(varAcc, IR_IntegerConstant(loopIncr))
        case strLit : IR_StringLiteral if strLit.value == loopItVar =>
          IR_Subtraction(strLit, IR_IntegerConstant(loopIncr))
      }, false), Some(prevItBody))

      IR_SimplifyFloatExpressions.applyStandalone(prevItBody)
      IR_GeneralSimplify.doUntilDoneStandalone(prevItBody, true)

      val coll = new CollectBaseCSes(curFunc)
      this.register(coll)
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(currItBody))
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(prevItBody))
      this.unregister(coll)
      val commonSubs : Map[IR_Node, Subexpression] = coll.commonSubs
      commonSubs.retain {
        (n, cs) =>
          cs != null &&
            (cs.getPositions().view.map({
              n : List[IR_Node] => n.last.asInstanceOf[Annotatable].hasAnnotation(ID_ANNOT)
            }).toSet.size == 2)
      }

      findCommonSubs(curFunc, commonSubs, orderMap)

      val filteredCS : Array[(Int, String, Subexpression)] =
        commonSubs.view.filter {
          case (_, cs) =>
            cs != null && cs.prio >= 10 && // common subexpression must be large enough to justify the additional memory requirements
              (cs.getPositions().view.map({
                n : List[IR_Node] => n.last.asInstanceOf[Annotatable].hasAnnotation(ID_ANNOT)
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
                case x if x eq posHead => njuExpr
              }, false), Some(pos(1)))
          }

          var csNext : IR_Expression = Duplicate(commonExp.witness)
          this.execute(new Transformation("create subsequent iteration body", {
            case varAcc : IR_VariableAccess if varAcc.name == loopItVar =>
              IR_Addition(varAcc, IR_IntegerConstant(loopIncr))
            case strLit : IR_StringLiteral if strLit.value == loopItVar =>
              IR_Addition(strLit, IR_IntegerConstant(loopIncr))
          }, false), Some(csNext))
          csNext = IR_SimplifyExpression.simplifyFloatingExpr(csNext)
          val csNextWrap = IR_ExpressionStatement(csNext)
          IR_GeneralSimplify.doUntilDoneStandalone(csNextWrap, true)
          csNext = csNextWrap.expression

          // FIXME: fix datatypes
          val decl : IR_VariableDeclaration = commonExp.declaration
          val tmpBuf = new IR_IV_LoopCarriedCSBuffer(bufferCounter, decl.datatype, IR_ExpressionIndex(Duplicate(tmpBufLen)))
          bufferCounter += 1
          val tmpBufAcc = IR_LoopCarriedCSBufferAccess(tmpBuf, IR_ExpressionIndex(Duplicate(tmpBufInd)))
          decl.initialValue = Some(tmpBufAcc)
          decls += decl

          firstInits += IR_Assignment(Duplicate(tmpBufAcc), Duplicate(commonExp.witness), "=")
          nextUpdates += IR_Assignment(Duplicate(tmpBufAcc), csNext, "=")
        }
      }

      if (decls.nonEmpty) {
        val (stmts, annots) = loop.at1stIt(dim)
        stmts ++= firstInits
        annots += ((IR_Vectorization.COND_VECTABLE, None))
        if (tmpBufLen.isEmpty)
          annots += ((IR_Vectorization.COND_IGN_INCR, None))
        // prepend to body
        decls ++=:
          nextUpdates ++=:
          loop.body
        njuScopes += stmts
        if (loop.parDims.contains(dim) && loop.parallelization.potentiallyParallel)
          loop.isVectorizable = true
        loop.parDims -= dim
        loop.lcCSEApplied = true
      }

      val loopBeginOpt =
        try {
          IR_IntegerConstant(IR_SimplifyExpression.evalIntegralExtrema(loopBegin)._1)
        } catch {
          case ex : EvaluationException => Duplicate(loopBegin)
        }
      val loopEndOpt =
        try {
          IR_IntegerConstant(IR_SimplifyExpression.evalIntegralExtrema(loopEnd)._2)
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
          val (_, size : Long) = IR_SimplifyExpression.evalIntegralExtrema(tmpBufLen(len))
          val vecSize : Long = Platform.simd_vectorSize
          tmpBufLen(len) = IR_IntegerConstant((size + vecSize) & ~(vecSize - 1))
        } catch {
          case e : EvaluationException => // what a pitty...
        }
    }

    njuScopes
  }

  private def collectIDs(node : IR_Node, bs : BitSet) : Boolean = {
    var disjunct : Boolean = true
    this.execute(new Transformation("collect IDs", {
      case n =>
        for (id <- n.getAnnotation(ID_ANNOT))
          disjunct &= bs.add(id.asInstanceOf[Int])
        n
    }), Some(IR_Root(ListBuffer(node)))) // wrap to ensure node itself is also accessed by the trafo
    disjunct
  }

  private def conventionalCSE(curFunc : String, body : ListBuffer[IR_Statement], orderMap : Map[Any, Int]) : Unit = {
    var repeat : Boolean = false
    do {
      val coll = new CollectBaseCSes(curFunc)
      this.register(coll)
      this.execute(new Transformation("collect base common subexpressions", PartialFunction.empty), Some(IR_Scope(body)))
      this.unregister(coll)
      val commonSubs : Map[IR_Node, Subexpression] = coll.commonSubs
      commonSubs.retain { (_, cs) => cs != null && cs.getPositions().size > 1 }
      repeat = false
      if (commonSubs.nonEmpty) {
        findCommonSubs(curFunc, commonSubs, orderMap)
        repeat = updateAST(body, commonSubs)
      }
    } while (repeat)
  }

  private def findCommonSubs(curFunc : String, commonSubs : Map[IR_Node, Subexpression], orderMap : Map[Any, Int]) : Unit = {
    val processedChildren = new java.util.IdentityHashMap[Any, Null]()
    var nju : ArrayBuffer[List[IR_Node]] = commonSubs.view.flatMap { x => x._2.getPositions() }.to[ArrayBuffer]
    val njuCommSubs = new HashMap[IR_Node, Subexpression]()

    def registerCS(node : IR_Expression with Product, prio : Int, prioBonus : Int, pos : List[IR_Node], recurse : Boolean, children : Seq[Any]) : Unit = {
      njuCommSubs.getOrElseUpdate(node, new Subexpression(curFunc, node, prio, prioBonus)).addPosition(pos)
      for (child <- children)
        processedChildren.put(child, null)
      if (recurse)
        nju += pos
    }

    var todo : ArrayBuffer[List[IR_Node]] = new ArrayBuffer[List[IR_Node]]()
    while (nju.nonEmpty) {
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

            case _ : IR_VectorExpression | _ : IR_MatrixExpression | _ : IR_Print =>
            // don't do anything, these are never common subexpressions

            case parent : Product =>
              val (prods, buffs, Nil, _) = splitIt3[Product, Buffer[AnyRef], Seq[_]](parent.productIterator)
              val nrProds = prods.length
              val nrBuffs = buffs.length

              if (nrProds <= 2 && nrBuffs == 0) {
                val childCSes = prods.view.collect({ case n : IR_Node => commonSubs.get(n) })
                if (childCSes.forall(_.isDefined))
                  registerCS(parent, childCSes.map(_.get.prio).sum + 1, 1, pos, true, prods)

                // this is the only one, that may create new instances for subexpression witnesses,
                //   this must be in sync with Subexpression.getRepls below
              } else if (nrProds == 0 && nrBuffs == 1) {
                val dupParents : Seq[(IR_Expression with Product, Buffer[PrettyPrintable])] =
                  duplicateAndFill(parent, { case n : IR_Node => commonSubs.contains(n); case _ => false }, orderMap)
                for ((dupPar, dupParChildren) <- dupParents)
                  registerCS(dupPar, dupParChildren.view.map { case x : IR_Node => commonSubs(x).prio }.sum + 1, 1, pos, dupPar eq parent, dupParChildren)

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

  private def updateAST(body : ListBuffer[IR_Statement], commSubs : Map[IR_Node, Subexpression]) : Boolean = {
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
        case x if x.hasAnnotation(REPLACE_ANNOT) => x.removeAnnotation(REPLACE_ANNOT).get.asInstanceOf[IR_Node]
      }), Some(IR_Scope(body)))

    // add declaration after transformation to prevent modifying it
    commSub.declaration +=: body
    true
  }

  private def splitIt3[A: ClassTag, B: ClassTag, C: ClassTag](it : TraversableOnce[_]) : (Seq[A], Seq[B], Seq[C], Seq[Any]) = {

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
    (listA, listB, listC, listZ)
  }

  /**
    * @param orig The original element to be duplicated and filled.
    *             `orig.productIterator` must contain a single `Buffer[PrettyPrintable]`, whose elements will be sorted.
    */
  private def duplicateAndFill[T <: Product : ClassTag](orig : T, relevant : Any => Boolean, orderMap : Map[Any, Int]) : Seq[(T, Buffer[PrettyPrintable])] = {

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
    // filter semantically equivalent expressions that overlap, since these cannot be replaced both,
    //  eg, the subexpression "x*x" must occur twice in "x*x*x*x"
    val seen = HashMap[T, Buffer[Buffer[PrettyPrintable]]]()
    val result = new ArrayBuffer[(T, Buffer[PrettyPrintable])]()
    for ((dup, dupChildren) <- dups) {
      var add : Boolean = true
      val inRes = seen.getOrElseUpdate(dup, new ArrayBuffer())
      // check if any of the elements in dupChildren is already somewhere in result (ie, it is in inRes)
      for (c <- dupChildren)
        for (prev <- inRes)
          for (prevC <- prev)
            if (c eq prevC)
              add = false
      if (add) {
        inRes += dupChildren
        result += ((dup, dupChildren))
      }
    }
    result
  }

  private def removeAnnotations(body : ListBuffer[IR_Statement]) : Unit = {
    this.execute(new Transformation("remove old CSE annotations", {
      // eager `or` is required, since we want to remove ALL, not only the first available
      case node if node.removeAnnotation(ID_ANNOT).isDefined |
        node.removeAnnotation(REMOVE_ANNOT).isDefined |
        node.removeAnnotation(REPLACE_ANNOT).isDefined =>
        // match nodes for which we removed something (so the number of nodes modifed is counted by the statemanager)
        node
    }), Some(IR_Scope(body)))
  }
}

private class CollectBaseCSes(curFunc : String) extends IR_StackCollector {

  private final val SKIP_ANNOT : String = "CSE_skip"
  private var skip : Boolean = false

  val commonSubs = new HashMap[IR_Node, Subexpression]()

  override def enter(node : Node) : Unit = {
    super.enter(node) // adds current node to the stack

    if (skip)
      return

    node match {
      case c : IR_IfCondition =>
        c.annotate(SKIP_ANNOT)
        skip = true
      case blv : IR_ProcessLocalBlockLoopVariable =>
        blv.annotate(SKIP_ANNOT)
        skip = true

      case IR_VariableDeclaration(dt, name, _, _)                              =>
        commonSubs(IR_VariableAccess(name, dt)) = null
      case IR_Assignment(vAcc : IR_VariableAccess, _, _)                       =>
        commonSubs(vAcc) = null
      case IR_Assignment(IR_ArrayAccess(vAcc : IR_VariableAccess, _, _), _, _) =>
        commonSubs(vAcc) = null
      case IR_Assignment(IR_ArrayAccess(iv : IR_InternalVariable, _, _), _, _) =>
        commonSubs(iv) = null
      case IR_Assignment(dfa : IR_DirectFieldLikeAccess, _, _)                 =>
        commonSubs(dfa) = null
      case IR_Assignment(tba : IR_TempBufferAccess, _, _)                      =>
        commonSubs(tba) = null

      case _ : IR_IntegerConstant
           | _ : IR_RealConstant
           | _ : IR_BooleanConstant
           | _ : IR_VariableAccess
           | _ : IR_IV_FragmentPosition
           | _ : IR_IV_FragmentPositionBegin
           | _ : IR_IV_FragmentPositionEnd
           | _ : IR_StringLiteral
           | _ : IR_ArrayAccess
           | _ : IR_DirectFieldLikeAccess
           | _ : IR_TempBufferAccess
           | _ : IR_LoopCarriedCSBufferAccess
           | _ : IR_InternalVariable //
      =>

        // all matched types are subclasses of Expression and Product
        val cs = commonSubs.getOrElseUpdate(node.asInstanceOf[IR_Node], new Subexpression(curFunc, node.asInstanceOf[IR_Expression with Product]))
        if (cs != null)
          cs.addPosition(stack) // stack (list) itself is immutable

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
    "_ce%03d".format(c)
  }
}

private class Subexpression(val func : String, val witness : IR_Expression with Product, val prio : Int = 1, val prioBonus : Int = 0) {
  private val positions = new ArrayBuffer[List[IR_Node]]()

  private lazy val tmpVarDatatype : IR_Datatype = IR_RealDatatype
  // FIXME: make generic!
  private lazy val tmpVarName : String = Subexpression.getNewCseName(func)

  lazy val declaration = IR_VariableDeclaration(tmpVarDatatype, tmpVarName, Some(witness))
  lazy val ppString : String = witness.prettyprint()

  def addPosition(nju : List[IR_Node]) : Unit = {
    positions += nju
  }

  def getPositions() : Seq[List[IR_Node]] = {
    positions
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

/// IR_LoopCarriedCSBufferAccess

case class IR_LoopCarriedCSBufferAccess(var buffer : IR_IV_LoopCarriedCSBuffer, var index : IR_ExpressionIndex) extends IR_Access with IR_SpecialExpandable with IR_PolyArrayAccessLike {
  override def datatype = buffer.datatype

  override def uniqueID : String = buffer.resolveName()

  def linearize : IR_ArrayAccess = {
    if (buffer.dimSizes.isEmpty)
      IR_ArrayAccess(buffer, IR_IntegerConstant(0), Knowledge.data_alignFieldPointers)
    else
      IR_ArrayAccess(buffer, IR_Linearization.linearizeIndex(index, buffer.dimSizes), Knowledge.data_alignFieldPointers)
  }
}

/// IR_LinearizeLoopCarriedCSBufferAccess

object IR_LinearizeLoopCarriedCSBufferAccess extends DefaultStrategy("Linearize LoopCarriedCSBufferAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_LoopCarriedCSBufferAccess => access.linearize
  })
}

/// IR_IV_AbstractLoopCarriedCSBuffer

abstract class IR_IV_AbstractLoopCarriedCSBuffer(var namePostfix : String, var freeInDtor : Boolean) extends IR_UnduplicatedVariable {

  def identifier : Int
  def baseDatatype : IR_Datatype

  override def getDeclaration() : IR_VariableDeclaration = {
    val superDecl = super.getDeclaration()
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1)
      superDecl.datatype = IR_ArrayDatatype(superDecl.datatype, Knowledge.omp_numThreads)
    superDecl
  }

  override def wrapInLoops(body : IR_Statement) : IR_Statement = {
    var wrappedBody = super.wrapInLoops(body)
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1) {
      val begin = IR_VariableDeclaration(IR_IntegerDatatype, IR_LoopOverDimensions.threadIdxName, IR_IntegerConstant(0))
      val end = IR_Lower(IR_VariableAccess(IR_LoopOverDimensions.threadIdxName, IR_IntegerDatatype), IR_IntegerConstant(Knowledge.omp_numThreads))
      val inc = IR_PreIncrement(IR_VariableAccess(IR_LoopOverDimensions.threadIdxName, IR_IntegerDatatype))
      wrappedBody = IR_ForLoop(begin, end, inc, ListBuffer(wrappedBody), IR_ParallelizationInfo(potentiallyParallel = true))
    }
    wrappedBody
  }

  override def resolveAccess(baseAccess : IR_Expression, fragment : IR_Expression, domain : IR_Expression, field : IR_Expression, level : IR_Expression, neigh : IR_Expression) : IR_Expression = {
    var access = baseAccess
    if (Knowledge.omp_enabled && Knowledge.omp_numThreads > 1)
      access = IR_ArrayAccess(access, IR_StringLiteral("omp_get_thread_num()")) // access specific element of the outer "OMP-dim" first
    super.resolveAccess(access, fragment, domain, field, level, neigh)
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << resolveAccess(resolveName(), null, null, null, null, null)
  }

  override def resolveName() : String = {
    IR_IV_LoopCarriedCSBuffer.commonPrefix + identifier + namePostfix
  }

  override def resolveDatatype() : IR_Datatype = {
    IR_PointerDatatype(baseDatatype)
  }

  override def resolveDefValue() : Option[IR_Expression] = {
    Some(0)
  }

  override def getDtor() : Option[IR_Statement] = {
    val ptrExpr = resolveAccess(resolveName(), null, null, null, null, null)
    if (freeInDtor)
      Some(wrapInLoops(
        IR_IfCondition(ptrExpr,
          ListBuffer[IR_Statement](
            IR_ArrayFree(ptrExpr),
            IR_Assignment(ptrExpr, 0)))))
    else
      Some(wrapInLoops(IR_Assignment(ptrExpr, 0)))
  }
}

/// IR_IV_LoopCarriedCSBuffer

object IR_IV_LoopCarriedCSBuffer {
  final val commonPrefix = "_lcs"
}

case class IR_IV_LoopCarriedCSBuffer(var identifier : Int, var baseDatatype : IR_Datatype, var dimSizes : IR_ExpressionIndex)
  extends IR_IV_AbstractLoopCarriedCSBuffer("", !Knowledge.data_alignFieldPointers) {

  lazy val basePtr = IR_IV_LoopCarriedCSBufferBasePtr(identifier, baseDatatype)

  override def registerIV(declarations : AbstractMap[String, IR_VariableDeclaration], ctors : AbstractMap[String, IR_Statement], dtors : AbstractMap[String, IR_Statement]) = {
    super.registerIV(declarations, ctors, dtors)
    if (Knowledge.data_alignFieldPointers) // align this buffer iff field pointers are aligned -> register corresponding base pointer
    basePtr.registerIV(declarations, ctors, dtors)
  }
}

/// IR_IV_LoopCarriedCSBufferBasePtr
case class IR_IV_LoopCarriedCSBufferBasePtr(var identifier : Int, var baseDatatype : IR_Datatype)
  extends IR_IV_AbstractLoopCarriedCSBuffer("_base", true)

/// IR_DuplicateNodesForCSEWrapper

object IR_DuplicateNodesForCSEWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.opt_conventionalCSE || Knowledge.opt_loopCarriedCSE) {
      DuplicateNodes.instances.clear()
      DuplicateNodes.printStack = false
      DuplicateNodes.apply() // FIXME: only debug
      IR_Inlining.apply(true)
      IR_CommonSubexpressionElimination.apply()
    }
  }
}
