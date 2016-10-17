package exastencils.performance

import scala.collection.mutable.ListBuffer

import java.io._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.function._

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Settings
import exastencils.core._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.performance.ir.IR_SlotAccessAsConst

trait LogVerbose {
  val verbose = true

  def logVerbose(s : AnyRef) : Unit = {
    if (verbose) {
      println(s)
    }
  }
}

/** Field used in loop kernel with slot mapped to integer constant. */
private case class FieldWithSlotId(val field : IR_Field, val slotId : Int) {
  def identifierName : String = field.identifier + slotId.toString
  def multiDimArrayAccess(index : IR_ExpressionIndex) = {
    val ident = IR_VariableAccess(identifierName, Some(field.resolveBaseDatatype))
    IR_MultiDimArrayAccess(ident, index)
  }

}

/** Strategy to export kernels for kerncraft.
  *
  * Kerncraft has strict requirements on the syntax of kernel codes.
  * This strategy attempts to trim the kernel code syntax to a form
  * that is accepted by kerncraft.
  */
object KerncraftExport extends DefaultStrategy("Exporting kernels for kerncraft") with LogVerbose {

  // Skip kernels with IR constructs that cannot be transformed to kerncraft.
  // E.g. IR_InternalVariable
  val skipUntransformable = true

  val kerncraftDir = Paths.get(Settings.getBasePath, "kerncraft")
  setupExportDirectory()
  Logger.dbg("exporting kerncraft kernels to \"%s\"".format(kerncraftDir.toAbsolutePath.toString))

  var curFun : IR_Function = null
  var curFunLoopCounter = 0
  var curLoop : IR_LoopOverDimensions = null

  val loopCollector = new Collector {

    override def enter(node : Node) : Unit = {
      node match {
        case fun : IR_Function            =>
          curFunLoopCounter = 0
          curFun = fun
        case loop : IR_LoopOverDimensions => curLoop = loop
        case _                            =>
      }
    }

    override def leave(node : Node) : Unit = {
      node match {
        case fun : IR_Function            => curFun = null
        case loop : IR_LoopOverDimensions => curLoop = null
        case _                            =>
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
      logVerbose(s"function %s kernel %04d - ${ kernelFilePath.toString }".format(curFun.name, curFunLoopCounter))
      logVerbose(s"loop.numDimensions ${ loop.numDimensions }")
      IR_GeneralSimplify.doUntilDoneStandalone(loop.indices)
      if (verbose) {
        val ix = loop.indices
        val ixPairs = ix.begin.zip(ix.end)
        logVerbose("loop index ranges: " + ixPairs.map(ix =>
          ix match {
            case (begin, end) => "[%s,%s]".format(begin.prettyprint, end.prettyprint)
          }).mkString("[", ",", "]"))
      }

      // transform kernel to for-loop nest
      val forLoop = buildForLoopNest(clone)
      TransformKernel.applyStandalone(forLoop)
      if (TransformKernel.hasInternalVariables) {
        val skippedNote = if (skipUntransformable) ", skipped" else ", will not be parsed by kerncraft"
        Logger.warn("%s: kernel uses IR_Internalvariable s%s".format(
          kernelFilePath.toString, skippedNote))
      }
      val skipKernel = TransformKernel.hasInternalVariables && skipUntransformable
      if (!skipKernel) {
        // extract field accesses to build array declarations, e.g. "double fieldx[1000][1000][1000];"
        val fieldDecls = buildFieldDeclarations(TransformKernel.fields, true)

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

  def buildFieldDeclarations(fieldAccesses : Seq[FieldWithSlotId], placeHolderDim : Boolean) : Seq[String] = {
    val dimSizeConst = Array[String]("K", "L", "M", "N")
    var declidx = 0
    var seen = Set[String]()
    val decls = ListBuffer[String]()
    fieldAccesses.foreach(fieldWithSlotId => {
      if (!seen(fieldWithSlotId.identifierName)) {
        seen += fieldWithSlotId.identifierName
        val scalarDataType = fieldWithSlotId.field.resolveBaseDatatype
        val idname = fieldWithSlotId.identifierName
        //      val size = (0 until dim).map(dim => field.fieldSelection.field.fieldLayout.idxById("TOT", dim))

        val ndims = fieldWithSlotId.field.fieldLayout.numDimsData
        val dimsz = { d : Int => fieldWithSlotId.field.fieldLayout.defTotal(d) }

        val declBuf = new ListBuffer[String]
        declBuf += (scalarDataType.prettyprint(), " ", idname)
        val literalSizeDecl = (0 until ndims).reverse.flatMap(d => List("[", dimsz(d).toString, "]"))

        (0 until ndims).reverse.foreach(d => {
          if (placeHolderDim) declBuf += ("[", dimSizeConst(d), declidx.toString, "]")
          else declBuf ++= literalSizeDecl
        })
        declBuf += ";"
        if (placeHolderDim) {
          declBuf += " // " + idname
          declBuf ++= literalSizeDecl
        }
        declidx += 1
        decls += declBuf.mkString("")
      }
    })
    decls
  }

  def buildForLoopNest(loop : IR_LoopOverDimensions) : IR_ForLoop = {

    val begin = IR_LoopOverDimensions.evalMinIndex(loop.indices.begin, loop.numDimensions, true)
    val end = IR_LoopOverDimensions.evalMaxIndex(loop.indices.end, loop.numDimensions, true)

    def createForLoop(d : Int, body : ListBuffer[IR_Statement]) : IR_ForLoop = {
      def it = IR_VariableAccess(IR_DimToString(d), Some(IR_IntegerDatatype))
      val decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_DimToString(d), Some(IR_IntegerConstant(begin(d))))
      val cond = IR_Lower(it, IR_IntegerConstant(end(d)))
      val incr = IR_Assignment(it, loop.stepSize(d), "+=")

      val forloop = IR_ForLoop(decl, cond, incr, ListBuffer[IR_Statement]())
      forloop.body = body
      return forloop
    }

    var forLoop : Option[IR_ForLoop] = None
    (0 to loop.numDimensions - 1).foreach { d =>
      if (d == 0) {
        forLoop = Some(createForLoop(d, loop.body))
      } else {
        val outer = createForLoop(d, ListBuffer(forLoop.get))
        forLoop = Some(outer)
      }
    }
    //logVerbose("buildForLoopRec() loop:")
    //logVerbose("%s".format(forLoop.get.prettyprint()))
    return forLoop.get

  }

  def setupExportDirectory() : Unit = {
    if (!Files.exists(kerncraftDir)) {
      Files.createDirectory(kerncraftDir)
    } else {
      deleteKernelFiles
    }
  }

  private def deleteKernelFiles : Unit = {
    val matcher = new BiPredicate[Path, BasicFileAttributes] {
      val pattern = """.*kernel-(\d){1,}(-fun){0,1}.c$""".r
      def test(p : Path, att : BasicFileAttributes) : Boolean = {
        val isMatch = pattern.findFirstIn(p.toString) != None
        //println("file " + p.toString + " match: " + isMatch)
        return isMatch
      }
    }
    val files = Files.find(kerncraftDir, 1, matcher)

    files.forEach(new Consumer[Path] {
      override def accept(t : Path) : Unit = {
        Files.delete(t)
      }
    })
  }
}

/** Strategy to analyze and transform variable / field accesses in loop kernels.
  *
  * Field accesses are enumerated by slot access expressions.
  */
private object TransformKernel
  extends DefaultStrategy("Transforming variable/field accesses in loop kernel")
    with LogVerbose {
  val fields = ListBuffer[FieldWithSlotId]()
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

  val trafo : PartialFunction[Node, Transformation.OutputType] = {
    case fa : IR_MultiDimFieldAccess =>

      val slotId : Int =
        fa.fieldSelection.slot match {
          case sa : IR_SlotAccess           =>
            IR_SlotAccessAsConst(fa) match {
              case Some(id) =>
                logVerbose("XX SlotAccessAsConst: %d".format(id.v))
                id.v.toInt
              case _        => throw new Exception("IR_MultiDimFieldAccess wit IR_SlotAccess did not resolve to IR_IntegerConstant")
            }
          case IR_IntegerConstant(v)        => v.toInt
          case IR_VariableAccess("slot", _) => 99 // FIXME HACK, do we have some more info on slot variable?
          case _                            =>
            throw new Exception("KerncraftExport: unhandled IR_SlotAccess expression: %s".format(fa.fieldSelection.slot))
        }

      val fieldWithSlotId = FieldWithSlotId(fa.fieldSelection.field, slotId)

      fields += fieldWithSlotId

      val idname = fa.fieldSelection.field.identifier + slotId.toString
      val ident = new IR_VariableAccess(idname, Some(fa.fieldSelection.field.resolveBaseDatatype))

      fieldWithSlotId.multiDimArrayAccess(fa.index)

    case bs : IR_BoundedScalar =>
      Logger.warning("KerncraftExport: IR_BoundedScalar in kernel: " + bs.prettyprint())
      bs

    case va : IR_InternalVariable =>
      Logger.warning("KerncraftExport: IR_InternalVariable in kernel: " + va.prettyprint())
      hasInternalVariables = true
      va

    case x =>
      //logVerbose("XXXXX not handled: " + x.toString())
      x
  }

  this += new Transformation("Transforming kernels", trafo, true)

}
