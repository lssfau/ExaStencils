package exastencils.performance

import java.io._
import java.nio.file.{Files, Paths}

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.knowledge._
import exastencils.logger.Logger
import exastencils.strategies.SimplifyStrategy

import scala.collection.mutable.ListBuffer

/** Strategy to export kernels for kerncraft.
  *
  * Kerncraft has strict requirements on the syntax of kernel codes.
  * This strategy attempts to trim the kernel code syntax to a form
  * that is accepted by kerncraft.
  */
object KerncraftExport extends DefaultStrategy("Exporting kernels for kerncraft") {
  //  override def apply(applyAtNode : Option[Node] = None) : Unit = {

  val verbose = false
  // Skip kernels with IR constructs that cannot be transformed to kerncraft.
  // E.g. IR_InternalVariable
  val skipUntransformable = true

  val kerncraftDir = Paths.get(Settings.getBasePath, "kerncraft")
  if (!Files.exists(kerncraftDir)) {
    Files.createDirectory(kerncraftDir)
  }
  Logger.dbg("exporting kerncraft kernels to \"%s\"".format(kerncraftDir.toAbsolutePath.toString))

  var curFun : IR_Function = null
  var curFunLoopCounter = 0
  var curLoop : IR_LoopOverDimensions = null

  val loopCollector = new Collector {

    override def enter(node : Node) : Unit = {
      node match {
        case fun : IR_Function =>
          curFunLoopCounter = 0
          curFun = fun
        case loop : IR_LoopOverDimensions => curLoop = loop
        case _ =>
      }
    }

    override def leave(node : Node) : Unit = {
      node match {
        case fun : IR_Function => curFun = null
        case loop : IR_LoopOverDimensions => curLoop = null
        case _ =>
      }
    }

    override def reset() : Unit = curLoop = null
  }
  this.register(loopCollector)

  // assume a LoopOverDimension instance is a kernel.
  // Clone it and transform the clone.
  // Pretty print transformed kernel to file as simplified c++.
  this += new Transformation("Visiting LoopOverDimension", {
    case loop : IR_LoopOverDimensions => {
      val clone = Duplicate(loop)
      val kernelFilePath = kerncraftDir.resolve("%s-kernel-%04d.c".format(curFun.name, curFunLoopCounter))
      val kernelFunFilePath = kerncraftDir.resolve("%s-kernel-%04d-fun.c".format(curFun.name, curFunLoopCounter))
      logVerbose("======================================================================")
      logVerbose(s"function %s kernel %04d - ${kernelFilePath.toString}".format(curFun.name, curFunLoopCounter))
      logVerbose(s"loop.numDimensions ${ loop.numDimensions }")
      SimplifyStrategy.doUntilDoneStandalone(loop.indices)
      if (verbose) {
        val ix = loop.indices
        val ixPairs = ix.begin.zip(ix.end)
        logVerbose("loop index ranges: " + ixPairs.map(ix =>
          ix match {
            case (begin, end) => "[%s,%s]".format(begin.prettyprint, end.prettyprint)
          }).mkString("[", ",", "]"))
      }

        // transform kernel to for-loop nest
      val forLoop = buildForLoopRec(clone)
      TransformKernel.applyStandalone(forLoop)
      if (TransformKernel.hasInternalVariables) {
        val skippedNote = if (skipUntransformable) ", skipped" else ", will not be parsed by kerncraft"
        Logger.warn("%s: kernel uses IR_Internalvariable s%s".format(
          kernelFilePath.toString, skippedNote))
      }
      val skipKernel = TransformKernel.hasInternalVariables && skipUntransformable
      if (!skipKernel) {
        // extract field accesses to build array declarations, e.g. "double fieldx[1000][1000][1000];"
        val fieldAccesses = TransformKernel.fields.distinct
        val fieldDecls = buildFieldDeclarations(TransformKernel.fields.toSet, false)

        fieldDecls.foreach(d => logVerbose(d))
        logVerbose(forLoop.prettyprint())

        // print to file

        val printer = new PrintWriter(kernelFilePath.toFile)
        fieldDecls.foreach(d => printer.println(d))
        printer.println(forLoop.prettyprint())
        printer.close()

        // print to file, wrapped in a main() function
        val printerFun = new PrintWriter(kernelFunFilePath.toFile)
        printerFun.println("int main() {")
        fieldDecls.foreach(d => printerFun.println(d))
        printerFun.println(forLoop.prettyprint())
        printerFun.println("return 0;")
        printerFun.println("}")
        printerFun.close()

        curFunLoopCounter += 1
      }
      loop
    }
  })

  def buildFieldDeclarations(fieldAccesses : Set[Field], placeHolderDim : Boolean) : List[String] = {
    val dimSizeConst = Array[String]("R", "S", "M", "N")
    var declidx = 0
    fieldAccesses.map(field => {
      val scalarDataType = field.resolveBaseDatatype
      val idname = "f" + field.identifier
      //      val size = (0 until dim).map(dim => field.fieldSelection.field.fieldLayout.idxById("TOT", dim))

      val dimLayout = field.fieldLayout.layoutsPerDim

      val declBuf = new StringBuilder()
      declBuf.append(scalarDataType.prettyprint())
      declBuf.append(" ")
      declBuf.append(idname)


      (0 until dimLayout.length).foreach(d => {
        val dimsz = field.fieldLayout.defTotal(d)
        declBuf.append("[")
        if (placeHolderDim) {
          val placeHolderIdx = dimSizeConst.length - dimLayout.length + d
          declBuf.append(dimSizeConst(placeHolderIdx))
          declBuf.append(declidx.toString)
        }
        else
          declBuf.append(dimsz)
        declBuf.append("]")
        //        logVerbose(s"field layout ${idname}[$d]: ${ dimsz}")
      })
      declBuf.append(";")

      declidx += 1

      declBuf.toString()
    }

    ).toList
  }

  def buildForLoopRec(loop : IR_LoopOverDimensions) : IR_ForLoop = {

    val begin = IR_LoopOverDimensions.evalMinIndex(loop.indices.begin, loop.numDimensions, true)
    val end = IR_LoopOverDimensions.evalMaxIndex(loop.indices.end, loop.numDimensions, true)

    //logVerbose(begin.map(x => x.toString).toList)
    //logVerbose(end.map(x => x.toString).toList)

    def buildRec(d : Integer, outer : Option[IR_ForLoop]) : Option[IR_ForLoop] = {
      if (d < loop.numDimensions) {

        def it = IR_VariableAccess(dimToString(d), Some(IR_IntegerDatatype))
        val decl = IR_VariableDeclaration(IR_IntegerDatatype, dimToString(d), Some(IR_IntegerConstant(begin(d))))
        val cond = IR_LowerExpression(it, IR_IntegerConstant(end(d)))
        val incr = IR_Assignment(it, loop.stepSize(d), "+=")

        val forloop = IR_ForLoop(decl, cond, incr, ListBuffer[IR_Statement]())

        outer match {
          case Some(outer) => outer.body.append(forloop)
          case _           =>
        }

        buildRec(d + 1, Some(forloop))

        Some(forloop)
      } else {
        outer.get.body = loop.body
        None
      }
    }

    //    val begin = VariableDeclarationStatement(IntegerDatatype, "i", Some(IntegerConstant(0)))
    //    val end = LowerExpression(new IR_VariableAccess(begin), IntegerConstant(0))
    //    val inc = ExpressionStatement(PreIncrementExpression(new IR_VariableAccess(begin)))
    //
    //    val forLoop = IR_ForLoop(begin, end, inc, ListBuffer[Statement]())
    //    logVerbose("*****")

    //      val forLoop = new IR_ForLoop(/**/,0,0,null)
    //    forLoop

    buildRec(0, None).get
  }

  def logVerbose(s: AnyRef) : Unit = {
    if (verbose) {
      Logger.debug(s)
    }
  }
}

private object TransformKernel extends DefaultStrategy("Kernel Transformation") {

  val fields = ListBuffer[Field]()
  var hasInternalVariables = false

  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    fields.clear()
    hasInternalVariables = false
    super.apply(applyAtNode)
  }

  override def applyStandalone(node : Node) : Unit = {
    fields.clear()
    hasInternalVariables = false
    super.applyStandalone(node)
  }

  this += new Transformation("Transforming kernels", {
    case fa : IR_MultiDimFieldAccess =>
      //      logVerbose("-----")
      //      logVerbose(fa)
      //      logVerbose(fa.fieldSelection.field.identifier, fa.index)
      fields += fa.fieldSelection.field
      val idname = "f" + fa.fieldSelection.field.identifier
      val ident = new IR_VariableAccess(idname, Some(fa.fieldSelection.field.resolveBaseDatatype))
      //      val aaidx = fa.index.map( ix => ix)

      val aa = new IR_MultiDimArrayAccess(ident, fa.index)

      //      logVerbose(aa.prettyprint())
      aa

    case bs : IR_BoundedScalar =>
      logVerbose("----" + bs)
      bs

    case va : IR_InternalVariable =>
      hasInternalVariables = true
      va
    case x                     =>
      //      logVerbose("xxxxx not handled: " + x.toString())
      x

  })

  def logVerbose(s:AnyRef): Unit = {
    KerncraftExport.logVerbose(s)
  }
}
