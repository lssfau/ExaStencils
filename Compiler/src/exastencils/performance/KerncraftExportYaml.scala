package exastencils.performance

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import java.io.PrintWriter
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.text.SimpleDateFormat
import java.util.Date
import java.util.function._

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.field.ir._
import exastencils.logger.Logger

// FIXME: irAccess should be of type IR_Access, but field accesses are not derived from IR_Access

/** Helper class to distinct [[exastencils.base.ir.IR_Access]] into read and write accesses for kerncraft YAML export.
  */
abstract class Access {
  def irAccess : IR_Expression
}

private case class ReadAccess(irAccess : IR_Expression) extends Access

private case class WriteAccess(irAccess : IR_Expression) extends Access

/** Summary information on a loop nest/kernel for kerncraft YAML export.
  *
  * @param loopIdx Linear index to the IR_LoopOverDimensions in the function body, i.e. the n-th loop in the function.
  */
private class KernelInfo(val loop : IR_LoopOverDimensions, val function : IR_Function, val loopIdx : Int) {

  val accesses = mutable.HashSet[Access]()

  var nIntAdd = 0
  var nIntMul = 0
  var nIntDiv = 0

  var nDoubleAdd = 0
  var nDoubleMul = 0
  var nDoubleDiv = 0

  var nFloatAdd = 0
  var nFloatMul = 0
  var nFloatDiv = 0

  def incrementNIntAdd(n : Int) : Unit = nIntAdd += n
  def incrementNIntMul(n : Int) : Unit = nIntMul += n
  def incrementNIntDiv(n : Int) : Unit = nIntDiv += n
  def incrementNFloatAdd(n : Int) : Unit = nFloatAdd += n
  def incrementNFloatMul(n : Int) : Unit = nFloatMul += n
  def incrementNFloatDiv(n : Int) : Unit = nFloatDiv += n
  def incrementNDoubleAdd(n : Int) : Unit = nDoubleAdd += n
  def incrementNDoubleMul(n : Int) : Unit = nDoubleMul += n
  def incrementNDoubleDiv(n : Int) : Unit = nDoubleDiv += n

}

private object KernelInfo {
  def apply(loop : IR_LoopOverDimensions, function : IR_Function, loopIdx : Int) : KernelInfo =
    new KernelInfo(loop, function, loopIdx)
}

object KerncraftExportYaml extends DefaultStrategy("Kernkraft YAML") {

  var curFun : IR_Function = null
  var curFunLoopCounter = 0
  var curLoop : IR_LoopOverDimensions = null
  var exportDir : Option[String] = None
  var debug : Boolean = false

  def export(pathToDir : String = Paths.get(Settings.getBasePath, "kerncraft").toString) : Unit = {
    val oldExportDir = exportDir
    try {
      exportDir = Some(pathToDir)
      apply()
    } finally {
      exportDir = oldExportDir
    }
  }

  val collector = new Collector {

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

  this.register(collector)

  this += new Transformation("Visiting LoopOverDimension", {
    // Assumes that IR_LoopOverDimensions are not nested.
    case loop : IR_LoopOverDimensions =>
      println("======================================================================")
      println("function %s kernel".format(curFun.name))
      //println("loop AST: %s".format(loop.toString()))
      val analysis = KernelAnalysis(loop, curFun, curFunLoopCounter)

      if (debug) {
        println(KernelInfoToYaml(analysis.kernelInfo))
      }

      if (exportDir != None) {
        val exportPath = Paths.get(exportDir.get)
        val kernelFilePath = exportPath.resolve("%s-kernel-%04d.yml".format(curFun.name, curFunLoopCounter))

        val printer = new PrintWriter(kernelFilePath.toFile)
        try {
          printer.print(KernelInfoToYaml(analysis.kernelInfo))
        } finally {
          printer.close()
        }
      }

      curFunLoopCounter += 1
      // FIXME we would really like to stop traversing the loop's tree here
      loop
  })

  def setupExportDirectory() : Unit = {
    val exportPath = Paths.get(exportDir.get)
    if (!Files.exists(exportPath)) {
      Files.createDirectory(exportPath)
    } else {
      deleteYamlFiles()
    }
  }

  private def deleteYamlFiles() : Unit = {
    val matcher = new BiPredicate[Path, BasicFileAttributes] {
      val pattern = """.*kernel-(\d){1,}(-fun){0,1}.yml$""".r
      def test(p : Path, att : BasicFileAttributes) : Boolean = {
        val isMatch = pattern.findFirstIn(p.toString).isDefined
        //println("file " + p.toString + " match: " + isMatch)
        isMatch
      }
    }
    val exportPath = Paths.get(exportDir.get)
    val files = Files.find(exportPath, 1, matcher)

    files.forEach(new Consumer[Path] {
      override def accept(t : Path) : Unit = {
        Files.delete(t)
      }
    })
  }
}

// FIXME: CompoundAssignments are not handled. Ideally fix IR_CompoundAssignment implementation before.
private class KernelAnalysis(loop : IR_LoopOverDimensions, function : IR_Function, loopIdx : Int)
  extends DefaultStrategy("Analyzing loop kernel") {

  private val fields = ListBuffer[FieldWithSlotId]()
  var hasInternalVariables = false

  val kernelInfo = KernelInfo(loop, function, loopIdx)

  // id for next slot expression for each field
  var nextSlotId = mutable.HashMap[IR_Field, Int]()
  // map per-field slot expressions to id
  val slotExprId = mutable.HashMap[(IR_Field, IR_Expression), Int]()

  val collector = new Collector {
    var assignmentDest : Option[IR_Node] = None
    var assignmentSrc : Option[IR_Node] = None
    // true if the current node is part of the destination sub-tree of an assignment.
    var inDest = false
    // true if the current node is part of the source sub-tree of an assignment.
    var inSrc = false

    def inDestInt = if (inDest) 1 else 0

    def inSrcInt = if (inSrc) 1 else 0

    override def leave(node : Node) : Unit = {
      node match {
        case IR_Assignment(dest, src, op)         =>
          //println("leave IR_Assignment")
          assignmentDest = None
          assignmentSrc = None
        case IR_CompoundAssignment(dest, src, op) =>
          //println("leave IR_CompoundAssignment")
          assignmentDest = None
          assignmentSrc = None
        case _                                    =>
      }
      if (assignmentDest.isDefined && node == assignmentDest.get) {
        inDest = false
      } else if (assignmentSrc.isDefined && node == assignmentSrc.get) {
        inSrc = false
      }
    }

    override def enter(node : Node) : Unit = {
      node match {
        case IR_Assignment(dest, src, op)         =>
          //println("enter IR_Assignment")
          assignmentDest = Some(dest)
          assignmentSrc = Some(src)
        case IR_CompoundAssignment(dest, src, op) =>
          //println("enter IR_CompoundAssignment")
          assignmentDest = Some(dest)
          assignmentSrc = Some(src)
        case _                                    =>
      }

      if (assignmentDest.isDefined && node == assignmentDest.get) {
        inDest = true
      } else if (assignmentSrc.isDefined && node == assignmentSrc.get) {
        inSrc = true
      }

      if (false) println(
        "enter node=%s | dest=%b node==dest=%b -> inDest=%b |  src=%b node==src=%b -> inSrc=%b".format(
          node.getClass.getSimpleName,
          assignmentDest.isDefined,
          assignmentDest.isDefined && node == assignmentDest.get,
          inDest,
          assignmentSrc.isDefined,
          assignmentSrc.isDefined && node == assignmentSrc.get,
          inSrc
        ))

      node match {
        case fa : IR_MultiDimFieldAccess =>
          recordAccess(fa)
          if (false) println("XXX %s  inDest=%d  inSrc=%d   dest=%b src=%b ".format(
            fa.fieldSelection.field.codeName,
            inDestInt,
            inSrcInt,
            assignmentDest.isDefined,
            assignmentSrc.isDefined))
        case ac : IR_Access              =>
          recordAccess(ac)

        case IR_Assignment(dest, src, op)         =>
        case IR_CompoundAssignment(dest, src, op) => ???
        case op : IR_Addition                     =>
          countOp(op, op.datatype, op.summands.length - 1,
            kernelInfo.incrementNIntAdd, kernelInfo.incrementNFloatAdd, kernelInfo.incrementNDoubleAdd);
        case op : IR_Subtraction                  =>
          countOp(op, op.datatype, 1,
            kernelInfo.incrementNIntAdd, kernelInfo.incrementNFloatAdd, kernelInfo.incrementNDoubleAdd);
        case op : IR_Multiplication               =>
          countOp(op, op.datatype, op.factors.length - 1,
            kernelInfo.incrementNIntMul, kernelInfo.incrementNFloatMul, kernelInfo.incrementNDoubleMul);
        case op : IR_Division                     =>
          countOp(op, op.datatype, 1,
            kernelInfo.incrementNIntMul, kernelInfo.incrementNFloatMul, kernelInfo.incrementNDoubleMul);
        case _                                    =>
      }

      def handleAssignment(dest : IR_Expression, src : IR_Expression, op : Option[IR_BinaryOperators.BinaryOperators]) : Unit = {
        op match {
          case None    => // noop
          case Some(x) =>

        }
      }
    }

    override def reset() : Unit = {
      assignmentDest = None
      assignmentSrc = None
      inDest = false
      inSrc = false
    }
  }

  this.register(collector)

  override def applyStandalone(node : Node) : Unit = {
    super.applyStandalone(node)
  }

  def recordAccess(access : IR_Expression) : Unit = {
    if (collector.inDest) {
      kernelInfo.accesses += WriteAccess(access)
    } else if (collector.inSrc) {
      kernelInfo.accesses += ReadAccess(access)
    }
  }

  def countOp(op : IR_Expression, datatype : IR_Datatype, nops : Int,
      intInc : (Int) => Unit, floatInc : (Int) => Unit, doubleInc : (Int) => Unit) : Unit = {
    assert(nops > 0)

    datatype match {
      case IR_IntegerDatatype               => intInc(nops)
      case IR_FloatDatatype                 => floatInc(nops)
      case IR_DoubleDatatype                => doubleInc(nops)
      case IR_RealDatatype                  =>
        if (Knowledge.useDblPrecision) doubleInc(nops)
        else floatInc(nops)
      case h : IR_HigherDimensionalDatatype =>
        val baseDatatype = h.resolveBaseDatatype
        countOp(op, baseDatatype, nops, intInc, floatInc, doubleInc)
      case x                                => Logger.warn(this.getClass.getName + ".countOp(): " +
        "unexpected datatype %s. Expression: %s".format(x.toString, op.prettyprint()))
    }
  }

  this += new Transformation("Kernkraft YAML analysis", PartialFunction.empty)
}

private object KernelAnalysis {
  def apply(loop : IR_LoopOverDimensions, function : IR_Function, loopIdx : Int) : KernelAnalysis = {

    val a = new KernelAnalysis(loop, function, loopIdx)
    // KernelAnalysis only looks at the loop body, loop control instructions are ignored!
    a.applyStandalone(loop.body)
    a
  }
}

private class KernelInfoToYaml(val kernelInfo : KernelInfo) {
  val yaml = new StringBuilder()

  val fieldsWithSlotId : mutable.HashMap[IR_MultiDimFieldAccess, FieldWithSlotId] = {
    val mdfas = ListBuffer[IR_MultiDimFieldAccess]()
    kernelInfo.accesses.foreach({ acc =>
      acc.irAccess match {
        case mdfa : IR_MultiDimFieldAccess => mdfas += mdfa
        //println("yaml field: %s".format(mdfa.fieldSelection.field.codeName))
        case x =>
        //println("yaml fields: ignoring %s".format(x.getClass.getCanonicalName()))
      }
    })
    EnumerateFieldsWithSlots(mdfas)
  }

  // TODO: general section
  // DONE: loop variable info sectio
  // n

  yaml ++= yamlGeneral()
  yaml ++= yamlArrays()
  yaml ++= yamlLoops()
  yaml ++= yamlData()
  yaml ++= yamlFlops()

  // TODO: data sources/destinations, (fl)ops

  def yindent(level : Int) = (0 until level).map(_ => "  ").mkString("")

  def yamlGeneral() : String = {
    val buf = new StringBuilder()
    buf ++= "general:\n"
    buf ++= yindent(1) + "analysis:\n"
    buf ++= yindent(2) + "tool: ScalaExaStencils\n"
    //RFC 3339 Timestamp
    buf ++= yindent(2) + "date: %s\n".format(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX").format(new Date()))

    buf ++= yindent(1) + "kernel:\n"
    buf ++= yindent(2) + "function: %s\n".format(kernelInfo.function.name)
    buf ++= yindent(2) + "kernel: %s-kernel-%04d\n".format(
      kernelInfo.function.name, kernelInfo.loopIdx)
    buf.toString()
  }

  def yamlArrays() : String = {
    val buf = new StringBuilder()
    buf ++= "arrays:\n"

    var seen = Set[String]()
    fieldsWithSlotId.values.toSet[FieldWithSlotId].foreach(fieldWithSlotId => {
      if (!seen(fieldWithSlotId.identifierName)) {
        seen += fieldWithSlotId.identifierName
        val scalarDataType = fieldWithSlotId.field.resolveBaseDatatype
        val idname = fieldWithSlotId.identifierName

        val ndims = fieldWithSlotId.field.fieldLayout.numDimsData
        val dimsz = { d : Int => fieldWithSlotId.field.fieldLayout.defTotal(d) }

        buf ++= yindent(1)
        buf ++= fieldWithSlotId.identifierName + ":\n"

        buf ++= yindent(2)
        buf ++= "type: " + scalarDataType.prettyprint() + "\n"

        buf ++= yindent(2)
        buf ++= "dimension: " + "[" +
          (0 until ndims).reverse.map(d => dimsz(d).toString).mkString(", ") + "]\n"

      }
    })
    buf.toString()
  }

  def yamlLoops() : String = {
    val buf = new StringBuilder()
    buf ++= "loops:\n"
    val loop = kernelInfo.loop

    val begin = IR_LoopOverDimensions.evalMinIndex(loop.indices.begin, loop.numDimensions, true)
    val end = IR_LoopOverDimensions.evalMaxIndex(loop.indices.end, loop.numDimensions, true)

    // TODO: is dimension order corect
    (0 until loop.numDimensions).reverse.foreach { d =>
      buf ++= yindent(1)
      buf ++= "-\n"

      val varName = IR_DimToString(d)

      buf ++= yindent(2) + "index: " + varName + "\n"
      buf ++= yindent(2) + "start: " + begin(d) + "\n"
      buf ++= yindent(2) + "stop: " + end(d) + "\n"
      buf ++= yindent(2) + "step: " + loop.stepSize(d).prettyprint + "\n"

    }
    buf.toString()
  }

  // FIXME: print set of access exprns instead of list
  //
  def yamlData() : String = {

    // split access list in sources and destinations
    val sources = ListBuffer[IR_MultiDimFieldAccess]()
    val destinations = ListBuffer[IR_MultiDimFieldAccess]()
    kernelInfo.accesses.foreach({ a =>
      a match {
        case ReadAccess(ira : IR_MultiDimFieldAccess)  =>
          sources += ira
        case WriteAccess(ira : IR_MultiDimFieldAccess) =>
          destinations += ira
        case _                                         => // don't care about scalar variables
      }
    })

    // map each field id to a list of index expression tuples
    def buildExprList(accesses : ListBuffer[IR_MultiDimFieldAccess])
    : mutable.HashMap[String, ListBuffer[List[String]]] = {
      val exprMap = mutable.HashMap[String, ListBuffer[List[String]]]()
      accesses.foreach({ a =>
        val fieldId = fieldsWithSlotId(a).identifierName
        // TODO reverse?
        val exprList = exprMap.getOrElse(fieldId, ListBuffer[List[String]]())
        exprList += a.index.map(ie => ie.prettyprint).toList
        exprMap.update(fieldId, exprList)
      })
      exprMap
    }

    val sourceExprns = buildExprList(sources)
    val destinationExprns = buildExprList(destinations)

    val buf = new StringBuilder()
    def yamlExprns(yamlKey : String, exprMap : mutable.HashMap[String, ListBuffer[List[String]]]) : Unit = {
      buf ++= yamlKey + ":\n"
      exprMap.keySet.foreach(fieldId => {
        buf ++= yindent(1)
        buf ++= fieldId + ":\n"
        exprMap(fieldId).foreach(exprList => {
          buf ++= yindent(2)
          buf ++= "- [" + exprList.reverse.mkString(", ") + "]\n"
        })
      })
    }
    yamlExprns("data sources", sourceExprns)
    yamlExprns("data destinations", destinationExprns)
    buf.toString()
  }

  def yamlFlops() : String = {
    val buf = new StringBuilder()

    def flopEntry(sym : String, v : Int) = {
      buf ++= yindent(1)
      buf ++= "\"%s\": %d\n".format(sym, v)
    }

    buf ++= "flops:\n"

    flopEntry("+", kernelInfo.nIntAdd)
    flopEntry("*", kernelInfo.nIntMul)
    flopEntry("/", kernelInfo.nIntDiv)
    flopEntry("f+", kernelInfo.nFloatAdd + kernelInfo.nDoubleAdd)
    flopEntry("f*", kernelInfo.nFloatMul + kernelInfo.nDoubleMul)
    flopEntry("f/", kernelInfo.nFloatDiv + kernelInfo.nDoubleDiv)
    buf.toString()
  }
}

private object KernelInfoToYaml {
  def apply(kernelInfo : KernelInfo) : String = {
    val o = new KernelInfoToYaml(kernelInfo)
    o.yaml.mkString("")
  }
}

/** Field accesses are enumerated by slot access expressions. Assumes slot expressions are constant inside the IR_LoopOverDimensions.
  *
  * @return A mapping from IR_MultiDimFieldAccess to FieldWithSlotId
  */
private object EnumerateFieldsWithSlots {
  def apply(accesses : Seq[IR_MultiDimFieldAccess]) : mutable.HashMap[IR_MultiDimFieldAccess, FieldWithSlotId] = {
    val fieldsWithSlotId = mutable.HashMap[IR_MultiDimFieldAccess, FieldWithSlotId]()

    // id for next slot expression for each field
    val nextSlotId = mutable.HashMap[IR_Field, Int]()
    // map per-field slot expressions to id
    val slotExprId = mutable.HashMap[(IR_Field, IR_Expression), Int]()

    accesses.foreach({ fa =>
      val slotId : Int = {
        slotExprId.get((fa.fieldSelection.field, fa.fieldSelection.slot)) match {
          case Some(id) => id
          case None     =>
            val id = nextSlotId.getOrElse(fa.fieldSelection.field, 0)
            nextSlotId(fa.fieldSelection.field) = id + 1
            slotExprId((fa.fieldSelection.field, fa.fieldSelection.slot)) = id
            id
        }
      }

      val fieldWithSlotId = FieldWithSlotId(fa.fieldSelection.field, slotId)
      fieldsWithSlotId.update(fa, fieldWithSlotId)
    })
    fieldsWithSlotId
  }
}
