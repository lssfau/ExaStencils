package exastencils.optimization

import java.util.IdentityHashMap

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set

import exastencils.core.Duplicate
import exastencils.core.Settings
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge.Knowledge
import exastencils.logger._

private final class Renamer(reserved : Set[String], inUse : Set[String]) {
  private final val parTempl : String = "_i%02d_%s"
  private final val nameMapping = Map[String, String]()
  def apply(s : String) : String = {
    nameMapping.getOrElseUpdate(s, {
      var nju : String = s
      if (reserved.contains(nju)) {
        var i = 0
        do {
          nju = parTempl.format(i, s)
          i += 1
        } while (reserved.contains(nju) || inUse.contains(nju))
      }
      nju
    })
  }
}

object Inlining extends CustomStrategy("Function inlining") {

  override def apply() : Unit = {
    this.apply(false)
  }

  /**
    * @param heuristics_prepareCSE
    *            If set inline only functions with exacly one `ReturnStatement` and any number of
    *            `VariableDeclarationStatement`s, otherwise perform a "normal" inlining (which respects
    *            `Knowledge.ir_maxInliningSize`).
    */
  def apply(heuristics_prepareCSE : Boolean) : Unit = {
    this.transaction()

    Logger.info("Applying strategy " + name)
    if (Settings.timeStrategies)
      StrategyTimer.startTiming(name)

    val analyzer = new Analyzer()
    this.register(analyzer)
    this.execute(new Transformation("collect", PartialFunction.empty))
    this.unregister(analyzer)

    // first, decide which functions should be inlined
    val toInline = Map[String, Int]()
    for ((func, funcStmt) <- analyzer.functions) {
      // some heuristics to identify which functions to inline
      val stmts : Buffer[Statement] = analyzer.flatFunctionBody(func)
      var inline : Boolean = stmts.length <= Knowledge.ir_maxInliningSize
      if (heuristics_prepareCSE) {
        var singleRet : Boolean = false
        for (stmt <- stmts) stmt match {
          case _ : VariableDeclarationStatement    =>
          case _ : ReturnStatement if (!singleRet) => singleRet = true
          case _                                   => inline = false // any other statement (or a second ReturnStatement) found...
        }
        inline &= singleRet
      }
      inline &= funcStmt.allowInlining
      if (inline)
        toInline(func) = 0
    }

    // second, count how many function calls to other inlinable functions are present
    for ((func, _) <- toInline)
      for ((_, _, _, inFunc) <- analyzer.calls(func))
        if (toInline.contains(inFunc))
          toInline(inFunc) += 1

    // then, inline all functions with count == 0 (and update other counts accordingly)
    val oldLvl = Logger.getLevel
    Logger.setLevel(Logger.WARNING) // a HUGE number of transformations are executed here, so shut them up
    val toRemove = new IdentityHashMap[FunctionStatement, Unit]() // used as a set; there's no need to compare thousands of nodes, reference equality is enough
    var continue : Boolean = false
    do {
      continue = false
      for ((func, i) <- toInline if (i == 0)) {
        toInline -= func // clear() removes current value while handling an iterator, too...
        continue = true // something changed, maybe we can inline even more, so go on...
        val funcStmt : FunctionStatement = analyzer.functions(func)
        val potConflicts : Set[String] = analyzer.potConflicts(func)
        var remove = true
        for ((callExpr, callStmt, callScope, inFunc) <- analyzer.calls(func)) {
          if (toInline.contains(inFunc))
            toInline(inFunc) -= 1
          val potConflToUpdate : Set[String] =
            if (callScope.isInstanceOf[FunctionStatement])
              analyzer.potConflicts(callScope.asInstanceOf[FunctionStatement].name)
            else
              null
          remove &= inline(callScope, callStmt, callExpr, funcStmt, potConflicts, potConflToUpdate)
        }
        if (remove)
          toRemove.put(funcStmt, ())
      }
    } while (continue)
    Logger.setLevel(oldLvl)

    this.execute(new Transformation("remove inlined functions", {
      case func : FunctionStatement if (toRemove.containsKey(func) && func.name != "main") => List()
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private def inline(callScope : Node, callStmt : Statement, callExpr : FunctionCallExpression, funcStmt : FunctionStatement,
    potConflicts : Set[String], potConflToUpdate : Set[String]) : Boolean = {

    // each function parameter must be given in call
    val nrPar = funcStmt.parameters.size
    if (nrPar != callExpr.arguments.size)
      return false

    // determine which variables must be renamed
    val reserved = Set[String]()
    this.execute(new Transformation("prepare", {
      case v @ VariableDeclarationStatement(_, name, _) =>
        reserved += name
        v
      case a @ VariableAccess(name, _) =>
        reserved += name
        a
      case s @ StringLiteral(name) =>
        reserved += name
        s
    }), Some(callScope))

    // rename conflicts
    val bodyWrapper = new Scope(Duplicate(funcStmt.body)) // wrap in Scope to allow removing statements
    val rename = new Renamer(reserved, potConflicts)
    var exit = false
    var retStmt : ReturnStatement = null
    this.execute(new Transformation("rename conflicts", {
      case VariableDeclarationStatement(t, name, i) if (potConflicts.contains(name)) => VariableDeclarationStatement(t, rename(name), i)
      case VariableAccess(name, t) if (potConflicts.contains(name))                  => VariableAccess(rename(name), t)
      case StringLiteral(name) if (potConflicts.contains(name))                      => StringLiteral(rename(name))
      case ret : ReturnStatement =>
        if (ret.expr.isEmpty != (funcStmt.returntype == UnitDatatype))
          exit = true
        retStmt = ret
        ret // keep ReturnStatement to ensure variables in its expression are renamed, too; it will be removed later
    }), Some(bodyWrapper))

    if (exit)
      return false
    this.execute(new Transformation("remove old return", {
      case r : ReturnStatement if (r eq retStmt) =>
        List()
    }), Some(bodyWrapper))
    val body : ListBuffer[Statement] = bodyWrapper.body

    // prepend declarations for method parameters (after renaming, as initialization must not be modified)
    body.++=:(funcStmt.parameters.zip(callExpr.arguments).map {
      case (vAcc, init) =>
        var name : String = vAcc.name
        if (potConflicts.contains(name))
          name = rename(name)
        new VariableDeclarationStatement(Duplicate(vAcc.dType.get), name, init)
    })

    if (potConflToUpdate != null) {
      // update potConflicts of function to inline in for later call to this method
      for (stmt <- body) stmt match {
        case VariableDeclarationStatement(_, name, _) => potConflToUpdate += name
        case _                                        => // ignore
      }
    }

    new CommentStatement("-.-.-.- inlined " + funcStmt.name + " -.-.-.-") +=:
      body
    if (retStmt == null)
      body +=
        new CommentStatement("=^=^=^=^= end " + funcStmt.name + " =^=^=^=^=")

    // perform actual inlining
    this.execute(new Transformation("inline", {
      case ExpressionStatement(call : FunctionCallExpression) if (call eq callExpr) =>
        body // return value is not available/used
      case stmt : Statement if (stmt eq callStmt) =>
        body += stmt
      case call : Expression if (call eq callExpr) =>
        if (retStmt == null || retStmt.expr.isEmpty)
          Logger.error("[inline]  Return type is Unit, but call is not inside an ExpressionStatement node")
        else
          retStmt.expr.get
    }), Some(callScope))

    return true
  }

  private final class Analyzer extends StackCollector {

    private[Inlining] final val functions = Map[String, FunctionStatement]()
    private[Inlining] final val flatFunctionBody = Map[String, Buffer[Statement]]((null, ArrayBuffer()))
    // value of calls: (call itself, statement containing it, statement's parent, function containing statement)
    private[Inlining] final val calls = Map[String, ListBuffer[(FunctionCallExpression, Statement, Node, String)]]()
    private[Inlining] final val potConflicts = Map[String, Set[String]]()

    private var curFunc : String = null
    private var inlinable : Boolean = false
    private var allowedReturn : Statement = null

    override def enter(node : Node) : Unit = {
      node match {
        case func : FunctionStatement =>
          curFunc = func.name
          inlinable = true
          flatFunctionBody(curFunc) = ArrayBuffer()
          val conf = Set[String]()
          for (par <- func.parameters)
            conf += par.name
          potConflicts(curFunc) = conf
          calls.getOrElseUpdate(curFunc, new ListBuffer()) // ensure curFunc has a mapping (withDefault is not suitable here, as it does not update the map...)
          allowedReturn = null
          if (!func.body.isEmpty) {
            val lastStmt = func.body.last
            if (lastStmt.isInstanceOf[ReturnStatement])
              allowedReturn = lastStmt
          }

        case call : FunctionCallExpression =>
          val callSites = calls.getOrElseUpdate(call.name, new ListBuffer())
          var it = stack.iterator
          while (it != null && it.hasNext) {
            val next = it.next()
            if (next.isInstanceOf[Statement]) {
              callSites += ((call, next.asInstanceOf[Statement], it.next(), curFunc))
              it = null // break
            }
          }

        case decl : VariableDeclarationStatement if (stack.top.isInstanceOf[FunctionStatement]) =>
          flatFunctionBody(curFunc) += decl
          potConflicts(stack.top.asInstanceOf[FunctionStatement].name) += decl.name

        case ret : ReturnStatement if (ret ne allowedReturn) =>
          flatFunctionBody(curFunc) += ret
          inlinable = false

        case StringLiteral(retStr) if (retStr.contains("return")) =>
          inlinable = false

        case stmt : Statement =>
          flatFunctionBody(curFunc) += stmt

        case _ => // ignore
      }
      super.enter(node)
    }

    override def leave(node : Node) : Unit = {
      node match {
        case func : FunctionStatement =>
          curFunc = null
          if (inlinable)
            functions(func.name) = func
          allowedReturn = null

        case _ => // ignore
      }
      super.leave(node)
    }

    override def reset() : Unit = {
      inlinable = false
      allowedReturn = null
      functions.clear()
      calls.clear()
      potConflicts.clear()
      super.reset()
    }
  }
}
