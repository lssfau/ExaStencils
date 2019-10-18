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

package meta

import scala.collection.mutable._

/// CodeMuncher

object CodeMuncher {
  // package -> classNames
  val metaClassList = HashMap[String, ListBuffer[String]]()

  var handledFiles = HashSet[String]()

  def generateGeneratorList() = {
    val printer = new Printer

    printer <<< "package meta"
    printer <<< ""

    printer <<< "import scala.collection.mutable.ListBuffer"
    printer <<< ""

    for (packageName <- metaClassList.keys)
      printer <<< s"import $packageName.meta._"
    printer <<< ""

    printer <<< "/// GeneratorList"
    printer <<< ""

    printer <<< "object GeneratorList {"
    printer <<< "  val entries = ListBuffer[Generatable]()"
    printer <<< ""

    for ((packageName, classList) <- metaClassList) {
      printer <<< s"  // in $packageName"
      printer <<< ""
      for (className <- classList)
        printer <<< s"  entries += $className"
      printer <<< ""
    }

    printer <<< "}"

    Generatable.printToFile(s"./Meta/src/meta/GeneratorList.scala", printer.toString)
  }

  def process(entry : MuncherList.Entry) : Unit = process(entry.filePackage, entry.className, entry.layers)
  def process(filePackage : String, className : String, layers : ListBuffer[Layer]) : Unit = {
    def innerFilename = s"src/${ filePackage.replace('.', '/') }/|LAYER_LC|/|LAYER_UC|_$className.scala"
    def metaFilename = s"./Compiler/$innerFilename"
    def outFile = Printer.process(s"./Meta/$innerFilename", MetaLayer)

    val codeVariants = HashMap[String, ListBuffer[Layer]]()

    (Layer.all -- layers).foreach(layer => {
      var filename = metaFilename
      filename = Printer.process(filename, layer)
      if (new java.io.File(filename).exists)
        println(s"Unregistered file for $className in $filePackage found (${ layer.lc })")
    })

    layers.foreach(layer => {
      var filename = metaFilename
      filename = Printer.process(filename, layer)
      handledFiles += filename

      var string = scala.io.Source.fromFile(filename).mkString

      // unify line endings
      string = string.replace("\r\n", "\n")

      if (Config.wrapProgressFunctionsWithLocation)
        string = WrapWithProgressLocation(string)

      def replaceIn(input : String, prefixes : List[String], postfixes : List[String]) = {
        var processed = input
        prefixes.foreach(pre => postfixes.foreach(post => {
          processed = processed.replaceAllLiterally(pre + layer.uc + post, pre + "|LAYER_UC|" + post)
          processed = processed.replaceAllLiterally(pre + layer.lc + post, pre + "|LAYER_LC|" + post)
          if (layer.hasNext) {
            processed = processed.replaceAllLiterally(pre + layer.next.uc + post, pre + "|NEXT_UC|" + post)
            processed = processed.replaceAllLiterally(pre + layer.next.lc + post, pre + "|NEXT_LC|" + post)
          }
        }))
        processed
      }

      val none = List("")
      val separators = List(".", "_")
      val spacers = List(" ", "\t", "\n")

      // replace occurrences in class names, etc.
      string = replaceIn(string, separators, none)
      string = replaceIn(string, none, separators)

      // replace freestanding occurrences
      string = replaceIn(string, spacers, spacers)

      // replace triple quotes
      string = string.replaceAllLiterally("\"\"\"", "\"\"\" + \"\\\"\\\"\\\"\" + \"\"\"")

      if (codeVariants.contains(string))
        codeVariants(string) += layer
      else
        codeVariants += ((string, ListBuffer(layer)))
    } : Unit)

    val codeVariantsSorted = codeVariants.toList.sortBy(_._2.head.lc)

    val printer = new Printer

    printer <<< s"package $filePackage.meta"
    printer <<< ""

    printer <<< "import scala.collection.mutable.ListBuffer"
    printer <<< ""

    printer <<< "import meta._"
    printer <<< ""

    printer <<< s"object ME_$className extends Generatable {"
    printer <<< s"  override def validLayers() = ListBuffer(${ layers.map(_.uc).mkString(", ") })"
    printer <<< ""

    printer <<< "  override def filenameForLayer(layer : Layer) = s\"./Compiler/src/" + filePackage.replace('.', '/') + "/|LAYER_LC|/|LAYER_UC|_" + className + ".scala\""
    printer <<< ""

    if (1 == codeVariantsSorted.size)
      printGenerateFunctionSingle(printer, codeVariantsSorted.head)
    else if (true)
      printGenerateFunctionPerLineSplit(printer, codeVariantsSorted)
    else
      printGenerateFunctionFctSplit(printer, codeVariantsSorted)

    printer <<< "}"

    Generatable.printToFile(outFile, printer.toString)

    if (metaClassList.contains(filePackage))
      metaClassList(filePackage) += s"ME_$className"
    else
      metaClassList += ((filePackage, ListBuffer(s"ME_$className")))
  }

  def printGenerateFunctionSingle(printer : Printer, codeVariant : (String, ListBuffer[Layer])) = {
    printer <<< "  override def generateForLayer(layer : Layer) = {"
    printer << "    \"\"\""
    printer << codeVariant._1
    printer <<< "\"\"\""
    printer <<< "  }"
  }

  def printGenerateFunctionPerLineSplit(printer : Printer, codeVariants : List[(String, ListBuffer[Layer])]) = {
    case class Variant(var lines : List[String], layers : ListBuffer[Layer]) {
      def isEmpty = lines.isEmpty
      def nonEmpty = lines.nonEmpty
    }

    def printBlock(lines : List[String], layers : ListBuffer[Layer]) = {
      printer <<< "    if (" + layers.map(layer => s"${ layer.uc } == layer").mkString(" || ") + ") {"
      for (line <- lines)
        printer <<< "      printer <<< \"\"\"" + line + "\"\"\""
      printer <<< "    }"
    }

    printer <<< "  override def generateForLayer(layer : Layer) = {"
    printer <<< "    val printer = new Printer"

    var variantList = codeVariants.map(v => Variant(v._1.split("\n").toList, v._2)).to[ListBuffer]
    var depth = 0

    while (variantList.nonEmpty) {
      if (1 == variantList.size) {
        // if there is only one variant left print it
        printBlock(variantList(0).lines, variantList(0).layers)
        variantList(0).lines = List()
      } else if (1 == variantList.map(_.lines(0)).distinct.length) {
        // if all variants have the same next line print it
        printer <<< "    printer <<< \"\"\"" + variantList(0).lines(0) + "\"\"\""
        variantList.transform(v => { v.lines = v.lines.tail; v })
      } else {
        // check respective distances for next line in other variants
        val distMap =
          variantList.indices.map(i => {
            variantList.indices.map(j => {
              variantList(i).lines.indexOf(variantList(j).lines(0))
            })
          })

        if (!distMap.flatten.exists(_ > 0)) {
          // no match in other blocks -> print one line of each block (grouped for multiple blocks sharing identical lines)
          val lineSquash = HashMap[String, ListBuffer[Layer]]().withDefaultValue(ListBuffer())
          variantList.foreach(v => lineSquash.update(v.lines(0), lineSquash(v.lines(0)) ++ v.layers))

          lineSquash.foreach { case (line, layers) =>
            printBlock(List(line), layers)
          }

//          printer <<< "    layer match {"
//          lineSquash.foreach { case (line, layers) =>
//            printer <<< "      case " + layers.map(_.uc).mkString(" | ") + " =>"
//            printer <<< "        printer <<< \"\"\"" + line + "\"\"\""
//          }
//          printer <<< "    }"

          variantList.foreach({ v => v.lines = v.lines.tail; v })
        }
        else {
          // match in other block -> find smallest block and print it
          var (iMin, jMin) = (0, 0)
          var minVal = Int.MaxValue
          variantList.indices.foreach(i => {
            variantList.indices.foreach(j => {
              if (distMap(i)(j) > 0 && distMap(i)(j) < minVal) {
                iMin = i
                jMin = j
                minVal = distMap(i)(j)
              }
            })
          })

          printBlock(variantList(iMin).lines.take(minVal), variantList(iMin).layers)
          variantList(iMin).lines = variantList(iMin).lines.drop(minVal)
        }
      }

      // remove completed variants
      val completed = variantList.filter(_.isEmpty)
      if (completed.nonEmpty && completed.length < variantList.length) {
        printer <<< "    if (!(" + completed.flatMap(_.layers).map(layer => s"${ layer.uc } == layer").mkString(" || ") + ")) {"
        depth += 1
      }

      variantList = variantList.filter(_.nonEmpty)
    }

    for (_ <- 0 until depth)
      printer <<< "    }"

    printer <<< "    printer.toString"
    printer <<< "  }"
  }

  def printGenerateFunctionFctSplit(printer : Printer, codeVariants : List[(String, ListBuffer[Layer])]) = {
    printer <<< "  override def generateForLayer(layer : Layer) = {"
    printer <<< "    layer match {"
    codeVariants.map(_._2).foreach(layers => {
      printer <<< "      case " + layers.map(_.uc).mkString(" | ") + " =>"
      printer <<< "        generateFor" + layers.map(_.uc).mkString("") + "()"
    })
    printer <<< "    }"
    printer <<< "  }"
    printer <<< ""

    codeVariants.foreach {
      case (code, assocLayers) =>
        printer <<< "  def generateFor" + assocLayers.map(_.uc).mkString("") + "()" + " = {"
        printer << "    \"\"\""
        printer << code
        printer <<< "\"\"\""
        printer <<< "  }"
        printer <<< ""
    }
    printer.getBuffer.setLength(printer.getBuffer.length - 1)
  }

  def checkForUnhandledFiles() = {
    val files = UpdateMuncherList.collectApplicableFiles()

    val unhandled = files./*map(_.getPath.replace('\\', '/')).*/ filterNot(handledFiles.contains)
    unhandled.foreach(u => println(s"Found unhandled file $u"))
  }
}