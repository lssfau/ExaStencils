import scala.collection.mutable.HashMap

import configEvaluator.Printer

object ConfigEvaluator {
  var sumMap = HashMap[String, HashMap[String, String]]()

  def main(args : Array[String]) : Unit = {
    if (args.isEmpty) {
      println("A path to the timing files has to be provided")
      return
    }

    val files = recursivelyListFiles(new java.io.File(args(0)))
    println(s"Found ${ files.length } applicable files")

    files.foreach(processFile)

    printSumMap(args(0) + "sum.dat")
  }

  def recursivelyListFiles(f : java.io.File) : Array[java.io.File] = {
    val (files, folders) = f.listFiles.partition(_.isFile)
    files.filter(_.getName.endsWith(".csv")) ++ folders.flatMap(recursivelyListFiles)
  }

  def processFile(file : java.io.File) = {
    val configName = file.getName

    val bufferedSource = io.Source.fromFile(file)

    for (line <- bufferedSource.getLines) {
      val Array(name, sum, avg) = line.split(";").map(_.trim)

      if (!sumMap.contains(configName))
        sumMap += (configName -> HashMap())

      sumMap(configName) += (name -> sum)
    }

    bufferedSource.close()
  }

  def printSumMap(fileName : String) = {
    val timerNames = sumMap.values.flatMap(_.keys).toList.distinct.sorted
    val printer = new Printer()

    printer << ";" <<< timerNames.mkString(";")
    for ((configName, timings) <- sumMap.toList.sortBy(_._1)) {
      printer << configName << ";"
      printer <<< timerNames.map(timings.getOrElse(_, "")).mkString(";")
    }

    printer.printToFile(fileName)
  }
}




