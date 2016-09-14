package exastencils.optimization

import scala.collection.mutable.{ ArrayBuffer, Buffer, ListBuffer, Map, Set }

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.core._
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
    * If set inline only functions with exacly one `ReturnStatement` and any number of
    * `VariableDeclarationStatement`s, otherwise perform a "normal" inlining (which respects
    * `Knowledge.IR_maxInliningSize`).
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
      val stmts : Buffer[IR_Statement] = analyzer.flatFunctionBody(func)
      var inline : Boolean = stmts.length <= Knowledge.ir_maxInliningSize
      if (heuristics_prepareCSE) {
        var singleRet : Boolean = false
        for (stmt <- stmts) stmt match {
          case _ : VariableDeclarationStatement =>
          case _ : IR_Return if (!singleRet)    => singleRet = true
          case _                                => inline = false // any other statement (or a second ReturnStatement) found...
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
    val toRemove = new IdentityHashMap[IR_Function, Unit]() // used as a set; there's no need to compare thousands of nodes, reference equality is enough
    var continue : Boolean = false
    do {
      continue = false
      for ((func, i) <- toInline if (i == 0)) {
        toInline -= func // clear() removes current value while handling an iterator, too...
        continue = true // something changed, maybe we can inline even more, so go on...
        val funcStmt : IR_Function = analyzer.functions(func)
        val potConflicts : Set[String] = analyzer.potConflicts(func)
        var remove = true
        for ((callExpr, callStmt, callScope, inFunc) <- analyzer.calls(func)) {
          if (toInline.contains(inFunc))
            toInline(inFunc) -= 1
          val potConflToUpdate : Set[String] =
            if (callScope.isInstanceOf[IR_Function])
              analyzer.potConflicts(callScope.asInstanceOf[IR_Function].name)
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
      case func : IR_Function if (toRemove.containsKey(func) && func.name != "main") => List()
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private def inline(callScope : Node, callStmt : IR_Statement, callExpr : FunctionCallExpression, funcStmt : IR_Function,
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
      case a @ IR_VariableAccess(name, _)               =>
        reserved += name
        a
      case s @ IR_StringLiteral(name)                   =>
        reserved += name
        s
    }), Some(callScope))

    // rename conflicts
    val bodyWrapper = IR_Scope(Duplicate(funcStmt.body)) // wrap in Scope to allow removing statements
    val rename = new Renamer(reserved, potConflicts)
    var exit = false
    var retStmt : IR_Return = null
    this.execute(new Transformation("rename conflicts", {
      case VariableDeclarationStatement(t, name, i) if (potConflicts.contains(name)) => VariableDeclarationStatement(t, rename(name), i)
      case IR_VariableAccess(name, t) if (potConflicts.contains(name))               => IR_VariableAccess(rename(name), t)
      case IR_StringLiteral(name) if (potConflicts.contains(name))                   => IR_StringLiteral(rename(name))
      case ret : IR_Return                                                           =>
        if (ret.expr.isEmpty != (funcStmt.returntype == IR_UnitDatatype))
          exit = true
        retStmt = ret
        ret // keep ReturnStatement to ensure variables in its expression are renamed, too; it will be removed later
    }), Some(bodyWrapper))

    if (exit)
      return false
    this.execute(new Transformation("remove old return", {
      case r : IR_Return if (r eq retStmt) =>
        List()
    }), Some(bodyWrapper))
    val body : ListBuffer[IR_Statement] = bodyWrapper.body

    // prepend declarations for method parameters (after renaming, as initialization must not be modified)
    body.++=:(funcStmt.parameters.zip(callExpr.arguments).map {
      case (vAcc, init) =>
        var name : String = vAcc.name
        if (potConflicts.contains(name))
          name = rename(name)
        new VariableDeclarationStatement(Duplicate(vAcc.datatype), name, init)
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
      case IR_ExpressionStatement(call : FunctionCallExpression) if (call eq callExpr) =>
        body // return value is not available/used
      case stmt : IR_Statement if (stmt eq callStmt)                                   =>
        body += stmt
      case call : IR_Expression if (call eq callExpr)                                  =>
        if (retStmt == null || retStmt.expr.isEmpty)
          Logger.error("[inline]  Return type is Unit, but call is not inside an ExpressionStatement node")
        else
          retStmt.expr.get
    }), Some(callScope))

    return true
  }

  private final class Analyzer extends StackCollector {

    private[Inlining] final val functions = Map[String, IR_Function]()
    private[Inlining] final val flatFunctionBody = Map[String, Buffer[IR_Statement]]((null, ArrayBuffer()))
    // value of calls: (call itself, statement containing it, statement's parent, function containing statement)
    private[Inlining] final val calls = Map[String, ListBuffer[(FunctionCallExpression, IR_Statement, Node, String)]]()
    private[Inlining] final val potConflicts = Map[String, Set[String]]()

    private var curFunc : String = null
    private var inlinable : Boolean = false
    private var allowedReturn : IR_Statement = null

    override def enter(node : Node) : Unit = {
      node match {
        case func : IR_Function =>
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
            if (lastStmt.isInstanceOf[IR_Return])
              allowedReturn = lastStmt
          }

        case call : FunctionCallExpression =>
          val callSites = calls.getOrElseUpdate(call.name, new ListBuffer())
          var it = stack.iterator
          while (it != null && it.hasNext) {
            val next = it.next()
            if (next.isInstanceOf[IR_Statement]) {
              callSites += ((call, next.asInstanceOf[IR_Statement], it.next(), curFunc))
              it = null // break
            }
          }

        case decl : VariableDeclarationStatement if (stack.top.isInstanceOf[IR_Function]) =>
          flatFunctionBody(curFunc) += decl
          potConflicts(stack.top.asInstanceOf[IR_Function].name) += decl.name

        case ret : IR_Return if (ret ne allowedReturn) =>
          flatFunctionBody(curFunc) += ret
          inlinable = false

        case IR_StringLiteral(retStr) if (retStr.contains("return")) =>
          inlinable = false

        case stmt : IR_Statement =>
          flatFunctionBody(curFunc) += stmt

        case _ => // ignore
      }
      super.enter(node)
    }

    override def leave(node : Node) : Unit = {
      node match {
        case func : IR_Function =>
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
