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

  private def readExplCfgs(path : String, cfgs : mutable.HashMap[String, (ExplCfg, Int)], level : Int) : Unit = {
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
        val njuCfg = old.getOrElse((cfg, ()))._1
        cfgs += ((cfg.key, (njuCfg, level)))
      } else
        i += 1
    }
    println("nr unused configs: " + i)
  }

  private def readPerf(path : String, cfgs : mutable.HashMap[String, (ExplCfg, Int)]) : Unit = {
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

  private def evaluate(cfgs : mutable.HashMap[String, (ExplCfg, Int)]) : Array[(Int, Double, Double)] = {
    val res = Array.fill(8)((0, -1.0, 1000000.0))
    for ((_, (cfg, level)) <- cfgs)
      for (l <- 0 to level) {
        val (nr, maxPerf, minPerf) = res(l)
        res(l) = (nr + 1, math.max(maxPerf, cfg.perf), math.min(minPerf, cfg.perf))
      }
    return res
  }

  private def writeEval(path : String, cfgs1 : mutable.HashMap[String, (ExplCfg, Int)], cfgs2 : mutable.HashMap[String, (ExplCfg, Int)]) : Unit = {
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

  private def compare(path : String, cfgs1 : mutable.HashMap[String, (ExplCfg, Int)], cfgs2 : mutable.HashMap[String, (ExplCfg, Int)]) : Unit = {
    val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(path)))
    var maxDiff : Double = 0.0
    var max1 : Double = 0.0
    var max2 : Double = 0.0
    for ((key, (cfg1, level)) <- cfgs1; if cfgs2.contains(key)) {
      val perf1 = math.round(cfg1.perf)
      val perf2 = math.round(cfgs2(key)._1.perf)
      if (perf1 > 0 && perf2 > 0) {
        val diff = math.abs(perf1 - perf2)
        out.write(diff + "\t" + perf1 + "\t" + perf2 + "\n")
        if (diff > maxDiff) {
          maxDiff = diff
          max1 = perf1
          max2 = perf2
        }
      }
    }
    println("max diff:  " + maxDiff + "  for: " + max1 + " - " + max2)
    out.flush()
    out.close()
  }

  private def fixPerf(cfgs1 : mutable.HashMap[String, (ExplCfg, Int)], cfgs2 : mutable.HashMap[String, (ExplCfg, Int)]) : Unit = {
    for ((k, _) <- cfgs2)
      if (cfgs1.contains(k))
        cfgs2(k) = cfgs1(k)
  }

  private def writeCfgs(path : String, cfgs : mutable.HashMap[String, (ExplCfg, Int)], start : Int) : Unit = {
    for (i <- start to 7) {
      val out = new java.io.BufferedWriter(new java.io.FileWriter(new java.io.File(path.format(i))))
      for ((key, (cfg, level)) <- cfgs)
        if (level >= i && cfg.perf > 0.0)
          out.write(cfg.perf + "\n")
      out.flush()
      out.close()
    }
  }

  private def mergeCfgs(cfgs1 : mutable.HashMap[String, (ExplCfg, Int)], cfgs2 : mutable.HashMap[String, (ExplCfg, Int)]) : mutable.HashMap[String, (ExplCfg, Int)] = {
    val res = new mutable.HashMap[String, (ExplCfg, Int)]()
    for ((k, v) <- cfgs1)
      if (v._1.perf > 0.0)
        res(k) = v
    for ((k, v) <- cfgs2)
      if (v._1.perf > 0.0)
        res(k) = v
    return res
  }

  def main(args : Array[String]) : Unit = {
    val base_cfgs = "D:\\poly_expl_paper_confs\\poly_expl"
    val base_res = "C:\\Users\\Stefan\\OneDrive\\Uni\\PolyExpl-Results"

    val ids2D = List("jacobi_2D_cc", "jacobi_2D_cc2", "jacobi_2D_cc_dense", "jacobi_2D_cc_dense_unal", "jacobi_2D_vc", "rbgs_2D_cc", "rbgs_2D_vc")
    val ids3D = ids2D.map(_.replace("2D", "3D"))
    val ids = ids2D ++ ids3D
    for (id <- ids) {
      val idShort = id.replace("jacobi_", "j").replace("rbgs_", "r").replace("_dense", "d").replace("_unal", "u")
      val cfgs_f0 = new mutable.HashMap[String, (ExplCfg, Int)]()
      val cfgs_f2 = new mutable.HashMap[String, (ExplCfg, Int)]()
      val path_f0 = base_cfgs + s"\\${ id }\\o2_configs.txt"
      val path_all = base_cfgs + s"3\\${ id }\\o2_configs_f%d.txt"
      val perf_f0 = base_res + s"\\in\\f0_${ idShort }.csv"
      val perf_f2 = base_res + s"\\in\\f2_${ idShort }.csv"
      val cmp_path = base_res + s"\\perf_cmp_${ idShort }.csv"
      val eval_path = base_res + s"\\eval_${ idShort }.csv"
      val plot_dat_f0 = base_res + s"\\plot1_${ idShort }_f%d.csv"
      val plot_dat_f2 = base_res + s"\\plot2_${ idShort }_f%d.csv"
      val plot_dat_me = base_res + s"\\plotM_${ idShort }_f%d.csv"

      println(id)
      println()
      println("full expl")
      readExplCfgs(path_f0, cfgs_f0, 0)
      println(cfgs_f0.size)
      for (i <- 1 to 7) {
        readExplCfgs(path_all.format(i), cfgs_f0, i)
        println(cfgs_f0.size)
      }
      readPerf(perf_f0, cfgs_f0)
      println()

      println("filtered expl")
      for (i <- 0 to 7) {
        readExplCfgs(path_all.format(i), cfgs_f2, i)
        println(cfgs_f2.size)
      }
      readPerf(perf_f2, cfgs_f2)

      compare(cmp_path, cfgs_f0, cfgs_f2)
      fixPerf(cfgs_f0, cfgs_f2)

      writeEval(eval_path, cfgs_f0, cfgs_f2)

      writeCfgs(plot_dat_f0, cfgs_f0, 0)
      writeCfgs(plot_dat_f2, cfgs_f2, 2)
//      writeCfgs(plot_dat_me, mergeCfgs(cfgs_f0, cfgs_f2), 0)
      println()
      println()
      println()
    }
  }
}
