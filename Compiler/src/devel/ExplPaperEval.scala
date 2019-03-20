package devel

import scala.collection.mutable
import scala.io.Source

import exastencils.polyhedron.Isl

case class ExplCfg(idStr : String, bandsStr : String, scheduleStr : String, vectorStr : String,
    carriedDepStr : String, vectableStr : String, var perf : Double, var perfID : Int) {
  def key() : String = scheduleStr
}

object ExplCfg {
  def apply(a : Array[String]) = new ExplCfg(a(0), a(1), a(2), a(3), a(4), a(5), 0.0, -1)
}

object ExplPaperEval {

  private def readExplCfgs(path : String, cfgs : mutable.Map[String, (ExplCfg, Int)], level : Int) : Unit = {
    var file : java.io.File = null
    for (i <- 0 until 10) {
      val f = new java.io.File(path.replace('?', ('0' + i).toChar))
      if (f.exists())
        file = f
    }
    val lines : Iterator[String] = Source.fromFile(file).getLines()
    // first three lines are domain, dependencies and a flag for extended exploration...
    val domain = isl.UnionSet.readFromStr(Isl.ctx, lines.next())
    val deps = isl.UnionMap.readFromStr(Isl.ctx, lines.next())
    val extExpl = java.lang.Boolean.parseBoolean(lines.next())
    // .. followed by an empty line
    lines.next()

    var i = 0
    val insertAll : Boolean = cfgs.isEmpty
    while (lines.hasNext) {
      val cfg = ExplCfg(lines.next().filter(_ != '"').split("\t"))
      val old = cfgs.get(cfg.key)
      if (insertAll || old.isDefined) {
        val njuCfg = old.getOrElse((cfg, 42))._1
        cfgs += ((njuCfg.key, (njuCfg, level)))
      } else
        i += 1
    }
    println("nr unused configs: " + i)
  }

  private def readPerf(path : String, cfgs : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    val lines : Iterator[String] = Source.fromFile(path).getLines()

    var i = 0
    var j = 0
    while (lines.hasNext) {
      val line = lines.next().replace("\";", "\t").filter(_ != '"').split("\t")
      if (line.length == 1)
        i += 1
      else if (line.length >= 2) {
        val Array(key, perfStr) = line
        if (cfgs.contains(key))
          cfgs(key)._1.perf = perfStr.replace(',', '.').toDouble
        else
          j += 1
      }
    }
    println("nr cfgs without runtime: " + i)
    println("perf for not available cfgs: " + j)
  }

  private def readPerfDirect(perfPath : String, cfgPath : String, cfgs : mutable.Map[String, (ExplCfg, Int)], perfRefPath : String) : Unit = {
    val perfLines : Iterator[String] = Source.fromFile(perfPath).getLines()
    var file : java.io.File = null
    for (i <- 0 until 10) {
      val f = new java.io.File(cfgPath.replace('?', ('0' + i).toChar))
      if (f.exists())
        file = f
    }
    val cfgLines : Iterator[String] = Source.fromFile(file).getLines()

    // some unneded lines at the beginning of the file
    val sep = perfLines.next()
    val header = perfLines.next()
    val reference = perfLines.next()
    val base = perfLines.next()

    if (perfRefPath != null) {
      val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(perfRefPath)))
      out.write(reference.split(";")(0))
      out.write("\n")
      out.flush()
      out.close()
    }

    // first three lines are domain, dependencies and a flag for extended exploration...
    val domain = isl.UnionSet.readFromStr(Isl.ctx, cfgLines.next())
    val deps = isl.UnionMap.readFromStr(Isl.ctx, cfgLines.next())
    val extExpl = java.lang.Boolean.parseBoolean(cfgLines.next())
    // .. followed by an empty line
    cfgLines.next()

    var i = 0
    var j = 0
    while (perfLines.hasNext && cfgLines.hasNext) try {
      val perfLine = perfLines.next()
      val cfgLine = cfgLines.next()

      val perfEntries = perfLine.split(";")
      val cfgEntries = cfgLine.split("\t")

      val perfID = perfEntries(1).toInt
      val cfgID = cfgEntries.head.toInt
      if (perfID != cfgID)
        throw new Error(s"whoot?! file lines out of sync:  $perfID vs $cfgID")

      val perf = perfEntries(0).toDouble
      val sched = cfgEntries.apply(2)

      if (cfgs.contains(sched)) {
        val tup = cfgs(sched)._1
        tup.perf = perf
        tup.perfID = perfID
      } else
        j += 1
    } catch {
      case _ : NumberFormatException =>
        i += 1
    }
    if (perfLines.hasNext)
      println("ERROR:  there are more performance values than configs?!")
    if (cfgLines.hasNext)
      println("ERROR:  there are more configs than performance values?!")
    println("nr perf entries without runtime: " + i)
    println("perf for not available cfgs: " + j)
  }

  private def evaluate(cfgs : mutable.Map[String, (ExplCfg, Int)]) : Array[(Int, Double, Double)] = {
    val res = Array.fill(8)((0, -1.0, 1000000.0))
    for ((_, (cfg, level)) <- cfgs)
      for (l <- 0 to level) {
        val (nr, maxPerf, minPerf) = res(l)
        res(l) = (nr + 1, math.max(maxPerf, cfg.perf), math.min(minPerf, cfg.perf))
      }
    return res
  }

  private def writeEval(path : String, cfgs1 : mutable.Map[String, (ExplCfg, Int)], cfgs2 : mutable.Map[String, (ExplCfg, Int)] = null) : Unit = {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(path)))
    var maxPerf : Double = -1.0
    if (cfgs2 == null)
      for (((size1, maxPerf1, minPerf1), level) <- evaluate(cfgs1).zipWithIndex) {
        if (maxPerf < 0)
          maxPerf = maxPerf1
        val maxPerf1_r = math.round(maxPerf1 / maxPerf * 1000) / 10.0
        val minPerf1_r = math.round(minPerf1 / maxPerf * 1000) / 10.0
        out.write(level + ";" + size1 + ";" + minPerf1_r + ";" + maxPerf1_r + ";" + size1 + ";-;-\n")
      }
    else
      for ((((size1, maxPerf1, minPerf1), (size2, maxPerf2, minPerf2)), level) <- evaluate(cfgs1).zip(evaluate(cfgs2)).zipWithIndex) {
        if (maxPerf < 0)
          maxPerf = math.max(maxPerf1, maxPerf2)
        val maxPerf1_r = math.round(maxPerf1 / maxPerf * 1000) / 10.0
        val maxPerf2_r = math.round(maxPerf2 / maxPerf * 1000) / 10.0
        val minPerf1_r = math.round(minPerf1 / maxPerf * 1000) / 10.0
        val minPerf2_r = math.round(minPerf2 / maxPerf * 1000) / 10.0
        if (level < 2)
          out.write(level + ";" + size1 + ";" + minPerf1_r + ";" + maxPerf1_r + ";" + size2 + ";-;-\n")
        else
          out.write(level + ";" + size1 + ";" + minPerf1_r + ";" + maxPerf1_r + ";" + size2 + ";" + minPerf2_r + ";" + maxPerf2_r + "\n")
    }
    out.flush()
    out.close()
  }

  private def compare(path : String, cfgs1 : mutable.Map[String, (ExplCfg, Int)], cfgs2 : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(path)))
    var maxDiff : Double = 0.0
    var max1 : ExplCfg = null
    var max2 : ExplCfg = null
    for ((key, (cfg1, level)) <- cfgs1; if cfgs2.contains(key)) {
      val cfg2 = cfgs2(key)._1
      val perf1 = math.round(cfg1.perf)
      val perf2 = math.round(cfg2.perf)
      if (perf1 > 0 && perf2 > 0) {
        val diff = math.abs(perf1 - perf2)
        out.write(cfg1.idStr + "\t" + diff + "\t" + perf1 + "\t" + perf2 + "\n")
        if (diff >= maxDiff) {
          maxDiff = diff
          max1 = cfg1
          max2 = cfg2
        }
      }
    }
    println(s"max diff:  $maxDiff  for id (${max1.perfID}, ${max2.perfID}): ${max1.perf} - ${max2.perf};  fraction: ${math.round(maxDiff/max1.perf*100)} - ${math.round(maxDiff/max2.perf*100)}")
    out.flush()
    out.close()
  }

  private def fixPerf(cfgs1 : mutable.Map[String, (ExplCfg, Int)], cfgs2 : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    for ((k, e) <- cfgs2)
      if (cfgs1.contains(k) && e._1.perf > 0.0)
        cfgs2(k)._1.perf = cfgs1(k)._1.perf
  }

  private def writeCfgs(path : String, cfgs : mutable.Map[String, (ExplCfg, Int)], start : Int) : Unit = {
    for (i <- start to 7) {
      val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(path.format(i))))
      for ((key, (cfg, level)) <- cfgs)
        if (level >= i && cfg.perf > 0.0)
          out.write(cfg.perf + "\n")
      out.flush()
      out.close()
    }
  }

  private def mergeCfgs(cfgs1 : mutable.Map[String, (ExplCfg, Int)], cfgs2 : mutable.Map[String, (ExplCfg, Int)]) : mutable.Map[String, (ExplCfg, Int)] = {
    val res = new mutable.HashMap[String, (ExplCfg, Int)]()
    for ((k, v) <- cfgs2)
      if (v._1.perf > 0.0)
        res(k) = v
    for ((k, v) <- cfgs1)
      if (v._1.perf > 0.0)
        res(k) = v
    return res
  }

  private def findMax(cfgs : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    var max : ExplCfg = cfgs.head._2._1
    var maxLevel : Int = 0
    for ((_, (cfg, level)) <- cfgs)
      if (cfg.perf > max.perf) {
        max = cfg
        maxLevel = level
      }
    println(s"best: ${max.perf} for ${max.perfID} up to level ${maxLevel}  (${max.scheduleStr})")
  }

  private def findMaxPerLevel(cfgs : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    val max : Array[ExplCfg] = new Array(8)
    for ((_, (cfg, lev)) <- cfgs)
      if (max(lev) == null || cfg.perf > max(lev).perf)
        max(lev) = cfg
    println("best per level (note that these level tags are disjoint):")
    for (i <- 7 to 0 by -1)
      if (max(i) != null)
        println(s"  Level $i:  ${max(i).perf} for ${max(i).perfID}")
  }

  private def process_single(id : String, start : Int = 0) : Unit = {
    val idShort = id.replace("jacobi_", "j").replace("rbgs_", "r").replace("_f0", "").replace("_f2", "")
    val cfgs = new mutable.HashMap[String, (ExplCfg, Int)]()
    val path = base_cfgs + s"\\${ id }\\o2_configs.?.txt"
    val path_all = base_cfgs + s"\\${ id }\\o2_configs_f%d.?.txt"
    val perf = base_cfgs + s"\\${ id }\\o6_results.csv"
    val o_eval_path = base_res + s"\\eval_${ idShort }.csv"
    val o_plot_dat = base_res + s"\\plot1_${ idShort }_f%d.csv"
    val o_perf_ref = base_res + s"\\perf_ref_${ idShort }.txt"

    println(id)
    println()
    println("full expl")
//    readExplCfgs(path, cfgs, 0)
//    println(cfgs.size)
    for (i <- 0 to 7) {
      readExplCfgs(path_all.format(i), cfgs, i)
      println(cfgs.size)
    }
    readPerfDirect(perf, path, cfgs, o_perf_ref)
    println()

    writeEval(o_eval_path, cfgs, null)
    findMax(cfgs)
    println()

    println("max per level for f0:")
    findMaxPerLevel(cfgs)
    println()

    writeCfgs(o_plot_dat, cfgs, start)
    println()
    println()
    println()
  }

  private def process_f0_f2(id : String) : Unit = {
    val id2 = id.replace("f0", "f2")
    val idShort = id.replace("jacobi_", "j").replace("rbgs_", "r").replace("_f0", "")
    val cfgs_f0 = new mutable.HashMap[String, (ExplCfg, Int)]()
    val cfgs_f2 = new mutable.HashMap[String, (ExplCfg, Int)]()
    val path_f0 = base_cfgs + s"\\${ id }\\o2_configs.?.txt"
    val path_f2 = base_cfgs + s"\\${ id2 }\\o2_configs.?.txt"
    val path_all = base_cfgs + s"\\${ id2 }\\o2_configs_f%d.?.txt"
    val perf_f0 = base_cfgs + s"\\${ id }\\o6_results.csv"
    val perf_f2 = base_cfgs + s"\\${ id2 }\\o6_results.csv"
    val o_cmp_path = base_res + s"\\perf_cmp_${ idShort }.csv"
    val o_eval_path = base_res + s"\\eval_${ idShort }.csv"
    val o_plot_dat_f0 = base_res + s"\\plot1_${ idShort }_f%d.csv"
    val o_plot_dat_f2 = base_res + s"\\plot2_${ idShort }_f%d.csv"
    val o_plot_dat_me = base_res + s"\\plotM_${ idShort }_f%d.csv"
    val o_perf_ref = base_res + s"\\perf_ref_${ idShort }.txt"

    println(id)
    println()
    println("full expl")
    readExplCfgs(path_f0, cfgs_f0, 0)
    println(cfgs_f0.size)
    for (i <- 1 to 7) {
      readExplCfgs(path_all.format(i), cfgs_f0, i)
      println(cfgs_f0.size)
    }
    readPerfDirect(perf_f0, path_f0, cfgs_f0, o_perf_ref)
    println()

    println("filtered expl")
    for (i <- 0 to 7) {
      readExplCfgs(path_all.format(i), cfgs_f2, i)
      println(cfgs_f2.size)
    }
    readPerfDirect(perf_f2, path_f2, cfgs_f2, null)

    compare(o_cmp_path, cfgs_f0, cfgs_f2)
    fixPerf(cfgs_f0, cfgs_f2)

    writeEval(o_eval_path, cfgs_f0, cfgs_f2)
    findMax(mergeCfgs(cfgs_f0, cfgs_f2))
    println()

    println("max per level for f0:")
    findMaxPerLevel(cfgs_f0)
    println()
    println("max per level for f2 (and extended):")
    findMaxPerLevel(cfgs_f2)
    println()

    writeCfgs(o_plot_dat_f0, cfgs_f0, 0)
    writeCfgs(o_plot_dat_f2, cfgs_f2, 2)
    writeCfgs(o_plot_dat_me, mergeCfgs(cfgs_f0, cfgs_f2), 0)
    println()
    println()
    println()
  }

  val base_cfgs = "S:\\work\\diss-eval\\poly_expl"
  val base_res = "C:\\Users\\Stefan\\OneDrive\\Uni\\PolyExpl-Results-Diss"

  def main(args : Array[String]) : Unit = {

    val jBase = List("cc1", "cc2", "ccd", "vc1")
    val rBase = List("cc1", "vc1")
    ////// Chimaira
    //// novec
    // 2D
//    for (b <- jBase)
//      process_single("jacobi_2D_" + b + "_f0")
//    for (b <- rBase) {
//      process_single("rbgs_2D_" + b + "_f0")
//      process_single("rbgs_2D_" + b + "_f0_cs")
//    }
//    // 3D
//    for (b <- jBase)
//      process_f0_f2("jacobi_3D_" + b + "_f0")
//    for (b <- rBase) {
//      process_f0_f2("rbgs_3D_" + b + "_f0")
//      process_f0_f2("rbgs_3D_" + b + "_f0_cs")
//    }
//    //// vec
//    for ((d, f) <- List(("2D", 0), ("3D", 2))) {
//      for (b <- jBase)
//        process_single("jacobi_" + d + "_" + b + "_f" + f + "_vec", f)
//      for (b <- rBase)
//        process_single("rbgs_" + d + "_" + b + "_f" + f + "_cs_vec", f)
//    }
//    ////// Pontipine
//    for ((d, f) <- List(("2D", 0), ("3D", 2))) {
//      for (b <- jBase)
//        process_single("jacobi_" + d + "_" + b + "_f" + f + "_pp", f)
//      for (b <- rBase)
//        process_single("rbgs_" + d + "_" + b + "_f" + f + "_cs_pp", f)
//    }

    import sys.process._
    val shPath = base_res.replaceAll("\\\\", "/").replace("C:", "/mnt/c")
    s"wsl.exe $shPath/0_plotMake.sh $shPath".!
  }
}
