package exastencils.optimization.ir

import scala.collection.mutable.{ ArrayBuffer, Buffer, ListBuffer, Map, Set }

import java.util.IdentityHashMap

import exastencils.base.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.util.ir.IR_StackCollector

private final class Renamer(reserved : Set[String], inUse : Set[String]) {
  private val parTempl : String = "_i%02d_%s"
  private val nameMapping = Map[String, String]()
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

object IR_Inlining extends CustomStrategy("Function inlining") {

  override def apply() : Unit = {
    this.apply(false)
  }

  /**
    * @param heuristics_prepareCSE
    * If set inline only functions with exactly one `ReturnStatement` and any number of
    * `VariableDeclarationStatement`s, otherwise perform a "normal" inlining.
    * Note: both versions respect `Knowledge.IR_maxInliningSize`.
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
      var inline : Boolean = stmts.length <= Knowledge.opt_maxInliningSize
      if (heuristics_prepareCSE) {
        var singleRet : Boolean = false
        for (stmt <- stmts) stmt match {
          case _ : IR_VariableDeclaration  =>
          case _ : IR_Return if !singleRet => singleRet = true
          case _                           => inline = false // any other statement (or a second ReturnStatement) found...
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
    Logger.pushLevel(Logger.WARNING) // a HUGE number of transformations are executed here, so shut them up
    val toRemove = new IdentityHashMap[IR_Function, Unit]() // used as a set; there's no need to compare thousands of nodes, reference equality is enough
    var continue : Boolean = false
    do {
      continue = false
      for ((func, i) <- toInline if i == 0) {
        toInline -= func // should be allowed, MapLike.clear() removes current value while handling an iterator, too...
        continue = true // something changed, maybe we can inline even more, so go on...
        val funcStmt : IR_Function = analyzer.functions(func)
        val potConflicts : Set[String] = analyzer.potConflicts(func)
        var remove = true
        for ((callExpr, callStmt, callScope, inFunc) <- analyzer.calls(func)) {
          if (toInline.contains(inFunc))
            toInline(inFunc) -= 1
          val potConflToUpdate : Set[String] =
            callScope match {
              case f : IR_Function => analyzer.potConflicts(f.name)
              case _               => null
            }
          remove &= inline(callScope, callStmt, callExpr, funcStmt, potConflicts, potConflToUpdate)
        }
        if (remove)
          toRemove.put(funcStmt, ())
      }
    } while (continue)
    Logger.popLevel()

    this.execute(new Transformation("remove inlined functions", {
      case f : IR_Function if toRemove.containsKey(f) && f.name != "main" => List() // main should not have "allowInlining" be set, but...
    }))

    if (Settings.timeStrategies)
      StrategyTimer.stopTiming(name)

    this.commit()
  }

  private def inline(callScope : Node, callStmt : IR_Statement, callExpr : IR_FunctionCall, funcStmt : IR_Function,
      potConflicts : Set[String], potConflToUpdate : Set[String]) : Boolean = {

    // each function parameter must be given in call
    val nrPar = funcStmt.parameters.size
    if (nrPar != callExpr.arguments.size)
      return false

    // determine which variables must be renamed
    val reserved = Set[String]()
    this.execute(new Transformation("prepare", {
      case v @ IR_VariableDeclaration(_, vname, _, _) =>
        reserved += vname
        v
      case a @ IR_VariableAccess(vname, _)            =>
        reserved += vname
        a
      case s @ IR_StringLiteral(vname)                =>
        reserved += vname
        s
    }), Some(callScope))

    // rename conflicts
    val bodyWrapper = IR_Scope(Duplicate(funcStmt.body)) // wrap in Scope to allow removing statements
    val rename = new Renamer(reserved, potConflicts)
    var exit = false
    var retStmt : IR_Return = null
    this.execute(new Transformation("rename conflicts", {
      case IR_VariableDeclaration(t, vname, i, _) if potConflicts.contains(vname) => IR_VariableDeclaration(t, rename(vname), i)
      case IR_VariableAccess(vname, t) if potConflicts.contains(vname)            => IR_VariableAccess(rename(vname), t)
      case IR_StringLiteral(vname) if potConflicts.contains(vname)                => IR_StringLiteral(rename(vname))
      case ret : IR_Return                                                        =>
        if (ret.expr.isEmpty != (funcStmt.datatype == IR_UnitDatatype))
          exit = true
        retStmt = ret
        ret // keep IR_Return to ensure variables in its expression are renamed, too; it will be removed later
    }), Some(bodyWrapper))

    if (exit)
      return false
    this.execute(new Transformation("remove old return", {
      case r : IR_Return if r eq retStmt =>
        List()
    }), Some(bodyWrapper))
    val body : ListBuffer[IR_Statement] = bodyWrapper.body

    // prepend declarations for method parameters (after renaming, as initialization must not be modified)
    body.++=:(funcStmt.parameters.zip(callExpr.arguments).map {
      case (vAcc, init) =>
        var vname : String = vAcc.name
        if (potConflicts.contains(vname))
          vname = rename(vname)
        IR_VariableDeclaration(Duplicate(vAcc.datatype), vname, init)
    })

    if (potConflToUpdate != null) {
      // update potConflicts of function to inline in for later call to this method
      for (stmt <- body) stmt match {
        case IR_VariableDeclaration(_, vname, _, _) => potConflToUpdate += vname
        case _                                      => // ignore
      }
    }

    val cmtPre = IR_Comment("-.-.-.- inlined " + funcStmt.name + " -.-.-.-")
    cmtPre +=: body
    if (retStmt == null)
      body += IR_Comment("=^=^=^=^= end " + funcStmt.name + " =^=^=^=^=")

    // perform actual inlining
    this.execute(new Transformation("inline", {
      case IR_ExpressionStatement(call : IR_FunctionCall) if call eq callExpr => body // return value is not available/used
      case stmt : IR_Statement if stmt eq callStmt                            => body += stmt
      case call : IR_Expression if call eq callExpr                           =>
        if (retStmt == null || retStmt.expr.isEmpty)
          Logger.error("[inline]  Return type is Unit, but call is not inside an ExpressionStatement node")
        else
          retStmt.expr.get
    }), Some(callScope))

    true
  }

  private final class Analyzer extends IR_StackCollector {

    private[IR_Inlining] val functions = Map[String, IR_Function]()
    private[IR_Inlining] val flatFunctionBody = Map[String, Buffer[IR_Statement]]((null, ArrayBuffer()))
    // values of calls: (call itself, statement containing it, statement's parent, function containing statement)
    private[IR_Inlining] val calls = Map[String, ListBuffer[(IR_FunctionCall, IR_Statement, Node, String)]]()
    private[IR_Inlining] val potConflicts = Map[String, Set[String]]()

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
          calls.getOrElseUpdate(curFunc, ListBuffer()) // ensure curFunc has a mapping (withDefault is not suitable here, as it does not update the map...)
          allowedReturn = null
          if (func.body.nonEmpty) {
            val lastStmt = func.body.last
            if (lastStmt.isInstanceOf[IR_Return])
              allowedReturn = lastStmt
          }

        case call : IR_FunctionCall =>
          val callSites = calls.getOrElseUpdate(call.name, new ListBuffer())
          var it = stack.iterator
          while (it != null && it.hasNext) {
            it.next() match {
              case stmt : IR_Statement =>
                callSites += ((call, stmt, it.next(), curFunc))
                it = null // break
              case _                   => // continue
            }
          }

        case decl : IR_VariableDeclaration if stack.head.isInstanceOf[IR_Function] =>
          flatFunctionBody(curFunc) += decl
          potConflicts(stack.head.asInstanceOf[IR_Function].name) += decl.name

        case ret : IR_Return if ret ne allowedReturn =>
          flatFunctionBody(curFunc) += ret
          inlinable = false

        case IR_StringLiteral(retStr) if retStr.contains("return") =>
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
