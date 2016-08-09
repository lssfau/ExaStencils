package exastencils.performance

import java.io.{File, PrintWriter}

import exastencils.core.Duplicate
import exastencils.datastructures.ir._
import exastencils.datastructures.{DefaultStrategy, Node, Transformation}
import exastencils.knowledge.{Field, FieldLayout, Mapping, dimToString}
import exastencils.logger.Logger
import exastencils.prettyprinting.PrettyPrintable
import exastencils.core.Settings
import exastencils.util.SimplifyExpression

import scala.collection.mutable.ListBuffer

/** Strategy to export kernels to kerncraft.
  *
  * Kerncraft has strict requirements on the syntax of kernel codes.
  * This strategy attempts to trim the kernel code syntax to a form
  * that is accepted by kerncraft.
  */
object KerncraftExport extends DefaultStrategy("Exporting kernels to kerncraft") {
  //  override def apply(applyAtNode : Option[Node] = None) : Unit = {

  var kernelId = 0

  val kerncraftDirStr = Settings.getBasePath + "/kerncraft"
  val kerncraftDir = new File(kerncraftDirStr)
  kerncraftDir.mkdir()

  // assume a LoopOverDimension instance is a kernel.
  // Clone it and transform the clone.
  // Pretty print transformed kernel to file as simplified c++.
  this += new Transformation("Visiting LoopOverDimension", {
    case loop : LoopOverDimensions => {
      val clone = Duplicate(loop)
//      val kernelFileStr = kerncraftDir.toString() + s"/kernel-${kernelId%4d}.c"
      val kernelFileStr = kerncraftDir.toString() + "/kernel-%04d.c".format(kernelId)
      val kernelFunFileStr = kerncraftDir.toString() + "/kernel-%04d-fun.c".format(kernelId)
      println("======================================================================")
      println(s"kernel %04d - $kernelFileStr".format(kernelId))
      println(s"loop.indices ${loop.indices}")
      println(s"loop.numDimensions ${loop.numDimensions}")

      // transform kernel to for-loop nest
      val forLoop = buildForLoopRec(clone)
      TransformKernel.applyStandalone(forLoop)
      // extract field accesses to build array declarations, e.g. "double fieldx[1000][1000][1000];"
      val fieldAccesses = TransformKernel.fields.distinct
      val fieldDecls = buildFieldDeclarations(TransformKernel.fields.toSet, false)

      fieldDecls.foreach(d => println(d))
      println(forLoop.prettyprint())

      // print to file

      val printer = new PrintWriter(kernelFileStr)
      fieldDecls.foreach(d => printer.println(d))
      printer.println(forLoop.prettyprint())
      printer.close()

      // print to file, wrapped in a main() function
      val printerFun = new PrintWriter(kernelFunFileStr)
      printerFun.println("int main() {")
      fieldDecls.foreach(d => printerFun.println(d))
      printerFun.println(forLoop.prettyprint())
      printerFun.println("return 0;")
      printerFun.println("}")
      printerFun.close()

      kernelId += 1

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
        //        println(s"field layout ${idname}[$d]: ${ dimsz}")
      })
      declBuf.append(";")

      declidx += 1

      declBuf.toString()

      //      field.fieldSelection.field.fieldLayout.layoutsPerDim.foreach()
      //      println("field index: " + field.fieldSelection.field.fieldLayout.layoutsPerDim


      //      val arrdt = ArrayDatatype_VS(scalarDataType, )
      //      VariableDeclarationStatement(scalarDataType, idname)
    }

    ).toList
  }

  def buildOrdinaryLoop(loop : LoopOverDimensions) : ForLoopStatement = {
    (0 until loop.indices.size).foreach(i => {
      println(i)

    })

    val begin = VariableDeclarationStatement(IntegerDatatype, "i", Some(IntegerConstant(0)))
    val end = LowerExpression(new VariableAccess(begin), IntegerConstant(0))
    val inc = ExpressionStatement(PreIncrementExpression(new VariableAccess(begin)))

    val forLoop = ForLoopStatement(begin, end, inc, ListBuffer[Statement]())
    println("*****")
    println(forLoop.prettyprint())
    //    println(">>>>>")
    //    loop.expandSpecial.foreach(s => println(s.prettyprint()))
    //      val forLoop = new ForLoopStatement(/**/,0,0,null)
    forLoop
  }

  def buildForLoopRec(loop : LoopOverDimensions) : ForLoopStatement = {

    val begin = LoopOverDimensions.evalMinIndex(loop.indices.begin, loop.numDimensions, true)
    val end = LoopOverDimensions.evalMaxIndex(loop.indices.end, loop.numDimensions, true)

    println(begin.map(x => x.toString).toList)
    println(end.map(x => x.toString).toList)

    def buildRec(d : Integer, outer : Option[ForLoopStatement]) : Option[ForLoopStatement] = {
      if (d < loop.numDimensions) {

        def it = VariableAccess(dimToString(d), Some(IntegerDatatype))
        val decl = VariableDeclarationStatement(IntegerDatatype, dimToString(d), Some(IntegerConstant(begin(d))))
        val cond = LowerExpression(it, IntegerConstant(end(d)))
        val incr = AssignmentStatement(it, loop.stepSize(d), "+=")

        val forloop = ForLoopStatement(decl, cond, incr, ListBuffer())

        outer match {
          case Some(outer) => outer.body.append(forloop)
          case _ =>
        }

        buildRec(d + 1, Some(forloop))

        Some(forloop)
      } else {
        outer.get.body = loop.body
        None
      }
    }

    //    val begin = VariableDeclarationStatement(IntegerDatatype, "i", Some(IntegerConstant(0)))
    //    val end = LowerExpression(new VariableAccess(begin), IntegerConstant(0))
    //    val inc = ExpressionStatement(PreIncrementExpression(new VariableAccess(begin)))
    //
    //    val forLoop = ForLoopStatement(begin, end, inc, ListBuffer[Statement]())
    //    println("*****")


    //      val forLoop = new ForLoopStatement(/**/,0,0,null)
    //    forLoop

    buildRec(0, None).get
  }


}


object TransformKernel extends DefaultStrategy("Kernel Transformation") {

  val fields = ListBuffer[Field]()


  override def apply(applyAtNode : Option[Node] = None) : Unit = {
    fields.clear()
    super.apply(applyAtNode)
  }

  override def applyStandalone(node : Node) : Unit = {
    fields.clear()
    super.applyStandalone(node)
  }

  this += new Transformation("Transforming kernels", {
    case fa : FieldAccessLike =>
      //      println("-----")
      //      println(fa)
      //      println(fa.fieldSelection.field.identifier, fa.index)
      fields += fa.fieldSelection.field
      val idname = "f" + fa.fieldSelection.field.identifier
      val ident = new VariableAccess(idname, fa.fieldSelection.field.resolveBaseDatatype)
//      val aaidx = fa.index.map( ix => ix)

      val aa = new ArrayAccessMultiDim(ident, fa.index)

      //      println(aa.prettyprint())
      aa

    case be : BoundedExpression =>
      println("----" + be)
      be
    case x =>
      //      println("xxxxx not handled: " + x.toString())
      x

  })
}
