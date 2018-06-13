package devel

import scala.collection.mutable
import scala.io.Source

import exastencils.polyhedron.Isl

case class ExplCfg(idStr : String, bandsStr : String, scheduleStr : String, vectorStr : String,
    carriedDepStr : String, vectableStr : String, var perf : Double) {
  def key() : String = scheduleStr
}

object ExplCfg {
  def apply(a : Array[String]) = new ExplCfg(a(0), a(1), a(2), a(3), a(4), a(5), 0.0)
}

object ExplPaperEval {

  private def readExplCfgs(path : String, cfgs : mutable.Map[String, (ExplCfg, Int)], level : Int) : Unit = {
    val lines : Iterator[String] = Source.fromFile(path).getLines()
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
        cfgs += ((cfg.key, (njuCfg, level)))
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

  private def readPerfDirect(perfPath : String, cfgPath : String, cfgs : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    val perfLines : Iterator[String] = Source.fromFile(perfPath).getLines()
    val cfgLines : Iterator[String] = Source.fromFile(cfgPath).getLines()

    // some unneded lines at the beginning of the file
    val sep = perfLines.next()
    val header = perfLines.next()
    val reference = perfLines.next()
    val base = perfLines.next()

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

      val perfID = perfEntries.head.toInt
      val cfgID = cfgEntries.head.toInt
      if (perfID != cfgID)
        throw new Error(s"whoot?! file lines out of sync:  $perfID vs $cfgID")

      val perf = perfEntries.view.slice(1, 6).map(_.replace(',', '.').toDouble).sorted.apply(2)
      val sched = cfgEntries.apply(2)

      if (cfgs.contains(sched))
        cfgs(sched)._1.perf = perf
      else
        j += 1
    } catch {
      case _ : NumberFormatException =>
        i += 1
    }
    if (perfLines.hasNext)
      println("ERROR:  there are more performance values than configs?!")
    if (cfgLines.hasNext)
      println("ERROR:  there are more configs than performance values?!")
    println("nr cfgs without runtime: " + i)
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

  private def writeEval(path : String, cfgs1 : mutable.Map[String, (ExplCfg, Int)], cfgs2 : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(path)))
    var maxPerf : Double = -1.0
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
        if (diff > maxDiff) {
          maxDiff = diff
          max1 = cfg1
          max2 = cfg2
        }
      }
    }
    println(s"max diff:  $maxDiff  for id ${max1.idStr}: ${max1.perf} - ${max2.perf};  fraction: ${math.round(maxDiff/max1.perf*100)} - ${math.round(maxDiff/max2.perf*100)}")
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
    println(s"best: ${max.perf} for ${max.idStr.toInt} up to level ${maxLevel}  (${max.scheduleStr})")
  }

  private def findMaxPerLevel(cfgs : mutable.Map[String, (ExplCfg, Int)]) : Unit = {
    val max : Array[ExplCfg] = new Array(8)
    for ((_, (cfg, lev)) <- cfgs)
      if (max(lev) == null || cfg.perf > max(lev).perf)
        max(lev) = cfg
    println("best per level (note that these level tags are disjoint):")
    for (i <- 7 to 0 by -1)
      if (max(i) != null)
        println(s"  Level $i:  ${max(i).perf} for ${max(i).idStr.toInt}")
  }

  def main(args : Array[String]) : Unit = {
    val base_cfgs = "T:\\work\\poly_expl_1803"
    val base_res = "C:\\Users\\Stefan\\OneDrive\\Uni\\PolyExpl-Results-1803"

    val ids2D = List("jacobi_2D_cc1", "jacobi_2D_cc2", "jacobi_2D_ccd", "jacobi_2D_vc1", "rbgs_2D_cc1", "rbgs_2D_vc1")
    val ids3D = ids2D.map(_.replace("2D", "3D"))
    val ids = ids2D ++ ids3D
//    val ids = List("jacobi_3D_ccd")
    for (id <- ids) {
      val idShort = id.replace("jacobi_", "j").replace("rbgs_", "r").replace("_dense", "d").replace("_unal", "u")
      val cfgs_f0 = new mutable.HashMap[String, (ExplCfg, Int)]()
      val cfgs_f2 = new mutable.HashMap[String, (ExplCfg, Int)]()
      val path_f0 = base_cfgs + s"\\${ id }\\o2_configs.txt"
      val path_f2 = base_cfgs + s"_f2\\${ id }\\o2_configs.txt"
      val path_all = base_cfgs + s"_fAll\\${ id }\\o2_configs_f%d.txt"
      val perf_f0 = base_cfgs + s"\\${ id }\\o6_results.csv"
      val perf_f2 = base_cfgs + s"_f2\\${ id }\\o6_results.csv"
      val o_cmp_path = base_res + s"\\perf_cmp_${ idShort }.csv"
      val o_eval_path = base_res + s"\\eval_${ idShort }.csv"
      val o_plot_dat_f0 = base_res + s"\\plot1_${ idShort }_f%d.csv"
      val o_plot_dat_f2 = base_res + s"\\plot2_${ idShort }_f%d.csv"
      val o_plot_dat_me = base_res + s"\\plotM_${ idShort }_f%d.csv"

      println(id)
      println()
      println("full expl")
      readExplCfgs(path_f0, cfgs_f0, 0)
      println(cfgs_f0.size)
      for (i <- 1 to 7) {
        readExplCfgs(path_all.format(i), cfgs_f0, i)
        println(cfgs_f0.size)
      }
      readPerfDirect(perf_f0, path_f0, cfgs_f0)
      println()

      println("filtered expl")
      readExplCfgs(path_all.format(0), cfgs_f2, 0)
      println(cfgs_f2.size)
      for (i <- 1 to 7) {
        readExplCfgs(path_all.format(i), cfgs_f2, i)
        println(cfgs_f2.size)
      }
      readPerfDirect(perf_f2, path_f2, cfgs_f2)

//      compare(o_cmp_path, cfgs_f0, cfgs_f2)
      fixPerf(cfgs_f0, cfgs_f2)

//      writeEval(o_eval_path, cfgs_f0, cfgs_f2)
      findMax(mergeCfgs(cfgs_f0, cfgs_f2))
      println()

      println("max per level for f0:")
      findMaxPerLevel(cfgs_f0)
      println()
      println("max per level for f2 (and extended):")
      findMaxPerLevel(cfgs_f2)
      println()

//      writeCfgs(o_plot_dat_f0, cfgs_f0, 0)
//      writeCfgs(o_plot_dat_f2, cfgs_f2, 2)
      //      writeCfgs(o_plot_dat_me, mergeCfgs(cfgs_f0, cfgs_f2), 0)
      println()
      println()
      println()
    }
  }
}
