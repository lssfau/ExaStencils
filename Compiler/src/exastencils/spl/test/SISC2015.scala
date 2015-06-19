package exastencils.spl.test

import exastencils.spl._
import exastencils.spl.util._
import scala.util.Random
import exastencils.spl.samplingStrategies.doe.PlackettBurmanDesign
import exastencils.spl.samplingStrategies.heuristics.FWHeuristic
import exastencils.spl.samplingStrategies.heuristics.PWHeuristic
import java.io.FileWriter
import java.io._
import exastencils.prettyprinting.JobScriptGenerator
import exastencils.knowledge.Knowledge
import exastencils.spl.samplingStrategies.doe.RandomDesign
import exastencils.spl.learning._

object SICS2015 {

  var featuresToConsider : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

  var num_points_per_dim2D = 32768
  var num_points_per_dim3D = 1024
  var num_points_per_dim = 0

  var dimToConsider = 2 //2 3
  var ccVSvc = "cc" // cc vc

  import exastencils.core.Settings

  val baseCaseStudyDir = "E:/Fallstudien/SISC_Paper/"
  val generationTargetDir = "E://ScalaExaStencil/configsSiSC/"

  def main(args : Array[String]) : Unit = {

    //    DomainKnowledgeTests.main(args)
    //    return

    if (dimToConsider == 2) {
      num_points_per_dim = num_points_per_dim2D
      featureToConsider2D()
    }
    if (dimToConsider == 3) {
      num_points_per_dim = num_points_per_dim3D
      featureToConsider3D()
    }

    println("Case study dim =" + dimToConsider + " with " + ccVSvc)

    val file = Settings.basePathPrefix + "/Compiler/src/exastencils/knowledge/Knowledge.scala"
    VariabilityParser.parseKnowlegdeFile(file)

    print("for filter ")
    println(FeatureModel.allFeatures.size)
    FeatureModel.filter(featuresToConsider)

    var x = FeatureModel.get("poly_optLevel_fine")
    // update upper value of firstDim and secDim 
    if (dimToConsider == 3) {
      FeatureModel.get("firstDim").maxValue = 2
      FeatureModel.get("secDim").maxValue = 2
    }

    print("after filter ")
    println(FeatureModel.allFeatures.size)

    var features : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => features.add(x._2))

    //    nonRunningTestExtraction()
    //    return

    //    lfaTests()
    //    return

    samplingWithPartitionedFeatureSpace()
    //    orgTest()
    return

    var configs : Array[Configuration] = null
    var configsValidation : Array[Configuration] = null

    if (dimToConsider == 2 && ccVSvc.equals("cc")) {
      configs = Util.createConfigsBasedOnMeasurementOneFile(baseCaseStudyDir + "2dCC_NoRandom\\SISC_2_cc_modified.csv", featuresToConsider).toArray
      configsValidation = Util.createConfigsBasedOnMeasurementOneFile(baseCaseStudyDir + "2dCC_Random\\SISC_2_cc_validation_modified.csv", featuresToConsider).toArray

    } else if (dimToConsider == 2 && ccVSvc.equals("vc")) {
      configs = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "2dVC_NoRandom", featuresToConsider).toArray
      configsValidation = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "2dVC_Random", featuresToConsider).toArray
    } else if (dimToConsider == 3 && ccVSvc.equals("cc")) {
      var conf = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "3dCC_NoRandom", featuresToConsider)
      //      conf.++=(createConfigsBasedOnMeasurements("E:\\Fallstudien\\SISC_Paper\\3dCC_to512Nodes_NoRandom"))
      configs = conf.toArray
      var confValid = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "3dCC_Random", featuresToConsider)
      //      confValid.++=(createConfigsBasedOnMeasurements("E:\\Fallstudien\\SISC_Paper\\3dCC_to512Nodes_Random"))
      configsValidation = confValid.toArray
    } else if (dimToConsider == 3 && ccVSvc.equals("vc")) {
      configs = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "3dVC_NoRandom", featuresToConsider).toArray
      configsValidation = Util.createConfigsBasedOnMeasurements(baseCaseStudyDir + "3dVC_Random", featuresToConsider).toArray
    }

    //    var nfps : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    //
    //    //    nfps.add("preSmoothing")
    //    //    nfps.add("correction")
    //    //    nfps.add("cycle")
    //    //    nfps.add("postSmoothing")
    //    //
    //    //    nfps.add("residualUpdate")
    //    //    nfps.add("restriction")
    //    //    nfps.add("settingSolution")
    //    //    nfps.add("setup")
    //    //    nfps.add("timeToSolve")
    //
    //    nfps.add("allTime")

    var lfaConfigs = IO.JSONParsing(baseCaseStudyDir + "LFA_version1\\", dimToConsider) // bei LFA_version2 ist das constains bei 16384
    var lfaConfigsCaseStudy : scala.collection.mutable.Set[LFAConfig] = scala.collection.mutable.Set()
    if (dimToConsider == 2 && ccVSvc.equals("cc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 2 && x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy.add(x) }

    if (dimToConsider == 2 && ccVSvc.equals("vc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 2 && !x.constCoeff && x.block_size == 4 && x.name.contains("16384")) lfaConfigsCaseStudy.add(x) }

    if (dimToConsider == 3 && ccVSvc.equals("cc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 3 && x.constCoeff && x.block_size == 4 && x.name.contains("512")) lfaConfigsCaseStudy.add(x) }

    if (dimToConsider == 3 && ccVSvc.equals("vc"))
      lfaConfigs.foreach { x => if (x.dimensionality == 3 && !x.constCoeff && x.block_size == 4 && x.name.contains("512")) lfaConfigsCaseStudy.add(x) }

    println(lfaConfigsCaseStudy.size)

    var toConsider : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    toConsider.add("postSmoothing")
    toConsider.add("correction")
    toConsider.add("settingSolution")
    toConsider.add("restriction")
    toConsider.add("preSmoothing")
    toConsider.add("residualUpdate")
    toConsider.add("cgsTime")

    //    printConfigurationsOneFile(configs, lfaConfigsCaseStudy, toConsider)

    //        printAllResultsToOneFile(configs, configsValidation, toConsider)
    //    return

    //    var models : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    //    var ffss : scala.collection.mutable.Map[String, Tuple2[ForwardFeatureSelection, Tuple2[Jama.Matrix, Array[FFS_Expression]]]] = scala.collection.mutable.Map()
    //
    //    toConsider.par.foreach { x =>
    //      {
    //
    //        var ffs : ForwardFeatureSelection = new ForwardFeatureSelection(features, 20, configs, x)
    //        println(x)
    //
    //        ffs.apply()
    //
    //        //        ffs.setValidationSet(configsValidation)
    //        ffs.perform()
    //
    //        var orgExpressions = ffs.solutionSet;
    //        var y = ffs.getModelWithConstants(orgExpressions.toArray[FFS_Expression])
    //
    //        var learnedModel = ffs.printModelWithConstants(y._1, y._2)
    //
    //        models.put(x, learnedModel)
    //        ffss.put(x, new Tuple2(ffs, y))
    //
    //        println("error model " + ffs.computeErrorForCombination(y._2, configs))
    //
    //        //
    //        var resultFile = "E:/Fallstudien/SISC_Paper/" + dimToConsider + "_" + ccVSvc + "_ModelAndPredictionFor_" + x + ".csv"
    //        var writer = new PrintWriter(new File(resultFile))
    //        var sb : StringBuilder = new StringBuilder()
    //
    //        println("MODEL for " + x)
    //        sb.append(ffs.printModelWithConstants(y._1, y._2) + " \n")
    //        sb.append("measured;predicted;name;LFATimeMeasured;PredictedTimesMeasured;measuredIterations;LFAIterations\n")
    //        configs.foreach { conf =>
    //          {
    //            var iterations : Int = conf.getLFAConfig(lfaConfigsCaseStudy).iterationsNeeded
    //            var newMatrix = new Jama.Matrix(y._2.size, 1)
    //            for (i <- 0 to y._2.size - 1) {
    //              var value = y._1.get(i, 0)
    //              newMatrix.set(i, 0, value * iterations)
    //            }
    //            sb.append(conf.nfpValues(x) + ";" + ffs.predictConfig(y._1, y._2, conf) + ";" + conf.measurementName + ";" + iterations * conf.nfpValues(x) + ";" + ffs.predictConfig(newMatrix, y._2, conf) + ";" + conf.nfpValues("NumIterations") + ";" + iterations + "\n")
    //          }
    //        }
    //        configsValidation.foreach { conf =>
    //          {
    //            var iterations : Int = conf.getLFAConfig(lfaConfigsCaseStudy).iterationsNeeded
    //            var newMatrix = new Jama.Matrix(y._2.size, 1)
    //            for (i <- 0 to y._2.size - 1) {
    //              var value = y._1.get(i, 0)
    //              newMatrix.set(i, 0, value * iterations)
    //            }
    //
    //            sb.append(conf.nfpValues(x) + ";" + ffs.predictConfig(y._1, y._2, conf) + ";" + conf.measurementName + ";" + iterations * conf.nfpValues(x) + ";" + ffs.predictConfig(newMatrix, y._2, conf) + ";" + conf.nfpValues("NumIterations") + ";" + iterations + "\n")
    //          }
    //        }
    //        writer.append(sb.toString())
    //        writer.flush()
    //        writer.close()
    //
    //      }
    //    }

    //     generating the model for the whole problem based on the models for the different parts
    var learnedModel = ""
    //    models.foreach(x => {
    //      if (learnedModel.size > 0)
    //        learnedModel + " + "
    //      learnedModel = x._2
    //    })
    //
    //    configs.foreach { conf =>
    //      {
    //        var iterations : Int = conf.getLFAConfig(lfaConfigsCaseStudy).iterationsNeeded
    //
    //        var sumMeasued = 0.0
    //        var sumPredicted = 0.0
    //        toConsider.foreach { nfp =>
    //          {
    //            var ffsAndY = ffss(nfp)
    //            var ffs = ffsAndY._1
    //            var y = ffsAndY._2
    //            sumMeasued += conf.nfpValues(nfp)
    //
    //            sumPredicted += ffs.predictConfig(y._1, y._2, conf)
    //          }
    //        }
    //
    //        println(sumMeasued + ";" + sumPredicted + ";" + conf.measurementName + ";" + conf.nfpValues("NumIterations") + ";" + iterations)
    //      }
    //    }
    //    configsValidation.foreach { conf =>
    //      {
    //        var sumMeasued = 0.0
    //        var sumPredicted = 0.0
    //
    //        var iterations : Int = conf.getLFAConfig(lfaConfigsCaseStudy).iterationsNeeded
    //
    //        toConsider.foreach { nfp =>
    //          {
    //            var ffsAndY = ffss(nfp)
    //            var ffs = ffsAndY._1
    //            var y = ffsAndY._2
    //            sumMeasued += conf.nfpValues(nfp)
    //
    //            sumPredicted += ffs.predictConfig(y._1, y._2, conf)
    //
    //          }
    //        }
    //
    //        println(sumMeasued + ";" + sumPredicted + ";" + conf.measurementName + ";" + conf.nfpValues("NumIterations") + ";" + iterations)
    //      }
    //    }

    println("model for one iteration : ")
    println(learnedModel)
    var ffs : ForwardFeatureSelection = new ForwardFeatureSelection(features, 20, configs, "")
    ffs.apply()

    if (dimToConsider == 2 && ccVSvc.equals("cc"))
      //      learnedModel = "-0.0064394757097425586 * domain_fragmentLength_y * maxLevel + -0.004560257721061133 * l3tmp_numPost + -13.2126718663305725 * maxLevel +-0.008528945840465041 * Jac * domain_rect_numFragsPerBlock_y + -0.03328437545956467 * poly_optLevel_fine * minLevel + -0.854715884099006 * l3tmp_numRecCycleCalls * minLevel + -0.009586647036392825 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -0.050558898392768975 * poly_optLevel_fine * domain_rect_numFragsPerBlock_y + -0.013129280512490205 * opt_useAddressPrecalc + -55.517712095487373 * 1 + -0.3676108446596291 * l3tmp_numRecCycleCalls * maxLevel + -0.06749139660287894 * domain_fragmentLength_x * domain_rect_numFragsPerBlock_x + -0.07543094123475347 * domain_fragmentLength_y * minLevel * domain_fragmentLength_x + -0.06696492508953497 * opt_vectorize * domain_fragmentLength_x + -0.010878746310418214 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * maxLevel + -11.880296564311788 * l3tmp_numPost * minLevel + -2.467508096254454 * Jac * minLevel * l3tmp_numPost * domain_rect_numFragsPerBlock_y + -0.10841560691455333 * domain_fragmentLength_y * RBGS * l3tmp_numPost * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * maxLevel + -3.1394728651733486 * domain_rect_numFragsPerBlock_y * l3tmp_numPost + -22.258384105581424 * l3tmp_numRecCycleCalls + -1.235078187301263 * RBGS * l3tmp_numPost * maxLevel + -0.03285389023851206 * mpi_useCustomDatatypes * l3tmp_numPost * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -3.0806189910770523 * domain_rect_numFragsPerBlock_y * l3tmp_numPre + -1.522668021839599 * l3tmp_useSlotsForJac + -23.477816009352015 * l3tmp_numRecCycleCalls + -2.2038793634551954 * Jac * minLevel * l3tmp_numPre * domain_rect_numFragsPerBlock_y + -0.08817599420458942 * poly_optLevel_fine * l3tmp_numPre * minLevel * domain_rect_numFragsPerBlock_x + -11.083562597795163 * l3tmp_numPre * minLevel + -1.134838312447672 * RBGS * l3tmp_numPre * maxLevel + -0.9304377836025013 * domain_fragmentLength_y * RBGS * l3tmp_numPre * l3tmp_numRecCycleCalls * domain_rect_numFragsPerBlock_y + -0.011561652800814939 * domain_fragmentLength_y * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * domain_fragmentLength_x * maxLevel + -12.7843982489679058 * minLevel + -0.7825257384886356 * domain_fragmentLength_x + -0.01828358548009792 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * domain_fragmentLength_y * maxLevel + -0.10942136448727624 * domain_rect_numFragsPerBlock_x + -9.696997790735045 * l3tmp_numRecCycleCalls * minLevel + -0.29237588074632226 * l3tmp_numPost * RBGS * poly_optLevel_fine * minLevel + -5.1094697785167861 * comm_useFragmentLoopsForEachOp + -0.01966492411798817 * mpi_useCustomDatatypes * domain_rect_numFragsPerBlock_x + -0.013805416554945587 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -0.011998083001936896 * l3tmp_numPre * poly_optLevel_fine * maxLevel + -0.006796431037699057 * l3tmp_numPost * domain_fragmentLength_y + -0.53047694288809 * l3tmp_numRecCycleCalls * maxLevel + -0.005382118168399348 * opt_unroll * maxLevel * l3tmp_numRecCycleCalls + -1.3947987734237583E-4 * poly_optLevel_fine * poly_tileSize_x * maxLevel + -0.08729778090514007 * domain_fragmentLength_y * l3tmp_numRecCycleCalls + -0.14979872814703596 * domain_fragmentLength_x * minLevel + -0.0024628079401805285 * minLevel * domain_rect_numFragsPerBlock_x"
      learnedModel = "0.0064394757097425586 * domain_fragmentLength_y * maxLevel + 0.004560257721061133 * l3tmp_numPost + 13.2126718663305725 * maxLevel +-0.008528945840465041 * Jac * domain_rect_numFragsPerBlock_y + 0.03328437545956467 * poly_optLevel_fine * minLevel + -0.854715884099006 * l3tmp_numRecCycleCalls * minLevel + 0.009586647036392825 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 0.050558898392768975 * poly_optLevel_fine * domain_rect_numFragsPerBlock_y + 0.013129280512490205 * opt_useAddressPrecalc + -55.517712095487373 * 1 + -0.3676108446596291 * l3tmp_numRecCycleCalls * maxLevel + -0.06749139660287894 * domain_fragmentLength_x * domain_rect_numFragsPerBlock_x + 0.07543094123475347 * domain_fragmentLength_y * minLevel * domain_fragmentLength_x + 0.06696492508953497 * opt_vectorize * domain_fragmentLength_x + 0.010878746310418214 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * maxLevel + 11.880296564311788 * l3tmp_numPost * minLevel + -2.467508096254454 * Jac * minLevel * l3tmp_numPost * domain_rect_numFragsPerBlock_y + -0.10841560691455333 * domain_fragmentLength_y * RBGS * l3tmp_numPost * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * maxLevel + 3.1394728651733486 * domain_rect_numFragsPerBlock_y * l3tmp_numPost + -22.258384105581424 * l3tmp_numRecCycleCalls + 1.235078187301263 * RBGS * l3tmp_numPost * maxLevel + 0.03285389023851206 * mpi_useCustomDatatypes * l3tmp_numPost * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 3.0806189910770523 * domain_rect_numFragsPerBlock_y * l3tmp_numPre + -1.522668021839599 * l3tmp_useSlotsForJac + -23.477816009352015 * l3tmp_numRecCycleCalls + -2.2038793634551954 * Jac * minLevel * l3tmp_numPre * domain_rect_numFragsPerBlock_y + 0.08817599420458942 * poly_optLevel_fine * l3tmp_numPre * minLevel * domain_rect_numFragsPerBlock_x + 11.083562597795163 * l3tmp_numPre * minLevel + 1.134838312447672 * RBGS * l3tmp_numPre * maxLevel + -0.9304377836025013 * domain_fragmentLength_y * RBGS * l3tmp_numPre * l3tmp_numRecCycleCalls * domain_rect_numFragsPerBlock_y + -0.011561652800814939 * domain_fragmentLength_y * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * domain_fragmentLength_x * maxLevel + 12.7843982489679058 * minLevel + 0.7825257384886356 * domain_fragmentLength_x + 0.01828358548009792 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * domain_fragmentLength_y * maxLevel + -0.10942136448727624 * domain_rect_numFragsPerBlock_x + -9.696997790735045 * l3tmp_numRecCycleCalls * minLevel + 0.29237588074632226 * l3tmp_numPost * RBGS * poly_optLevel_fine * minLevel + -5.1094697785167861 * comm_useFragmentLoopsForEachOp + 0.01966492411798817 * mpi_useCustomDatatypes * domain_rect_numFragsPerBlock_x + 0.013805416554945587 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 0.011998083001936896 * l3tmp_numPre * poly_optLevel_fine * maxLevel + -0.006796431037699057 * l3tmp_numPost * domain_fragmentLength_y + -0.53047694288809 * l3tmp_numRecCycleCalls * maxLevel + -0.005382118168399348 * opt_unroll * maxLevel * l3tmp_numRecCycleCalls + 1.3947987734237583E-4 * poly_optLevel_fine * poly_tileSize_x * maxLevel + 0.08729778090514007 * domain_fragmentLength_y * l3tmp_numRecCycleCalls + 0.14979872814703596 * domain_fragmentLength_x * minLevel + 0.0024628079401805285 * minLevel * domain_rect_numFragsPerBlock_x"
    //      learnedModel = "0.0026598580417355982 * domain_fragmentLength_x * maxLevel + 0.0037796176680069604 * domain_fragmentLength_y * maxLevel + 0.004560257721061133 * l3tmp_numPost + 0.8652450803851031 * maxLevel + 0.762071560955187 * minLevel + -0.008528945840465041 * Jac * domain_rect_numFragsPerBlock_y + 0.03328437545956467 * poly_optLevel_fine * minLevel + -0.854715884099006 * l3tmp_numRecCycleCalls * minLevel + 0.009586647036392825 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -8.01330503146351 * 1 + 0.050558898392768975 * poly_optLevel_fine * domain_rect_numFragsPerBlock_y + 0.013129280512490205 * opt_useAddressPrecalc + -14.96313827766639 * 1 + -0.3676108446596291 * l3tmp_numRecCycleCalls * maxLevel + -0.06749139660287894 * domain_fragmentLength_x * domain_rect_numFragsPerBlock_x + 0.6067769811810128 * minLevel + 0.07543094123475347 * domain_fragmentLength_y * minLevel * domain_fragmentLength_x + 0.06696492508953497 * opt_vectorize * domain_fragmentLength_x + 0.010878746310418214 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * maxLevel + 2.2439422650090224 * maxLevel + 11.880296564311788 * l3tmp_numPost * minLevel + -2.467508096254454 * Jac * minLevel * l3tmp_numPost * domain_rect_numFragsPerBlock_y + -0.10841560691455333 * domain_fragmentLength_y * RBGS * l3tmp_numPost * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * maxLevel + 22.384219592112647 * 1 + 3.1394728651733486 * domain_rect_numFragsPerBlock_y * l3tmp_numPost + -22.258384105581424 * l3tmp_numRecCycleCalls + 1.235078187301263 * RBGS * l3tmp_numPost * maxLevel + 0.03285389023851206 * mpi_useCustomDatatypes * l3tmp_numPost * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 3.0806189910770523 * domain_rect_numFragsPerBlock_y * l3tmp_numPre + -1.522668021839599 * l3tmp_useSlotsForJac + -23.477816009352015 * l3tmp_numRecCycleCalls + -2.2038793634551954 * Jac * minLevel * l3tmp_numPre * domain_rect_numFragsPerBlock_y + 0.08817599420458942 * poly_optLevel_fine * l3tmp_numPre * minLevel * domain_rect_numFragsPerBlock_x + 11.083562597795163 * l3tmp_numPre * minLevel + 1.134838312447672 * RBGS * l3tmp_numPre * maxLevel + -0.9304377836025013 * domain_fragmentLength_y * RBGS * l3tmp_numPre * l3tmp_numRecCycleCalls * domain_rect_numFragsPerBlock_y + 27.56747433295817 * 1 + -0.011561652800814939 * domain_fragmentLength_y * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * domain_fragmentLength_x * maxLevel + -43.75616953340436 * 1 + 0.7825257384886356 * domain_fragmentLength_x + 0.01828358548009792 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * domain_fragmentLength_y * maxLevel + -0.10942136448727624 * domain_rect_numFragsPerBlock_x + -9.696997790735045 * l3tmp_numRecCycleCalls * minLevel + 5.124355062632224 * maxLevel + 0.29237588074632226 * l3tmp_numPost * RBGS * poly_optLevel_fine * minLevel + -0.1094697785167861 * comm_useFragmentLoopsForEachOp + 11.415549706831706 * minLevel + 0.01966492411798817 * mpi_useCustomDatatypes * domain_rect_numFragsPerBlock_x + 0.013805416554945587 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 0.011998083001936896 * l3tmp_numPre * poly_optLevel_fine * maxLevel + -0.006796431037699057 * l3tmp_numPost * domain_fragmentLength_y + -0.53047694288809 * l3tmp_numRecCycleCalls * maxLevel + -0.005382118168399348 * opt_unroll * maxLevel * l3tmp_numRecCycleCalls + -1.3947987734237583 * poly_optLevel_fine * poly_tileSize_x * maxLevel + 0.08729778090514007 * domain_fragmentLength_y * l3tmp_numRecCycleCalls + 0.14979872814703596 * domain_fragmentLength_x * minLevel + 0.0024628079401805285 * minLevel * domain_rect_numFragsPerBlock_x + -38.73679317802393 * 1 + 4.979129458304223 * maxLevel"

    //      learnedModel = "0.0026598580417355982 * domain_fragmentLength_x * maxLevel + 0.0037796176680069604 * domain_fragmentLength_y * maxLevel + 0.004560257721061133 * l3tmp_numPost + 0.8652450803851031 * maxLevel + 0.762071560955187 * minLevel + -0.008528945840465041 * Jac * domain_rect_numFragsPerBlock_y + 0.03328437545956467 * poly_optLevel_fine * minLevel + -0.854715884099006 * l3tmp_numRecCycleCalls * minLevel + 0.009586647036392825 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -8.01330503146351 * 1 + 0.050558898392768975 * poly_optLevel_fine * domain_rect_numFragsPerBlock_y + 0.013129280512490205 * opt_useAddressPrecalc + -14.96313827766639 * 1 + -0.3676108446596291 * l3tmp_numRecCycleCalls * maxLevel + -0.06749139660287894 * domain_fragmentLength_x * domain_rect_numFragsPerBlock_x + 0.6067769811810128 * minLevel + 0.07543094123475347 * domain_fragmentLength_y * minLevel * domain_fragmentLength_x + 0.06696492508953497 * opt_vectorize * domain_fragmentLength_x + 0.010878746310418214 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * maxLevel + 2.2439422650090224 * maxLevel + 11.880296564311788 * l3tmp_numPost * minLevel + -2.467508096254454 * Jac * minLevel * l3tmp_numPost * domain_rect_numFragsPerBlock_y + -0.10841560691455333 * domain_fragmentLength_y * RBGS * l3tmp_numPost * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * maxLevel + 22.384219592112647 * 1 + 3.1394728651733486 * domain_rect_numFragsPerBlock_y * l3tmp_numPost + -22.258384105581424 * l3tmp_numRecCycleCalls + 1.235078187301263 * RBGS * l3tmp_numPost * maxLevel + 0.03285389023851206 * mpi_useCustomDatatypes * l3tmp_numPost * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 3.0806189910770523 * domain_rect_numFragsPerBlock_y * l3tmp_numPre + -1.522668021839599 * l3tmp_useSlotsForJac + -23.477816009352015 * l3tmp_numRecCycleCalls + -2.2038793634551954 * Jac * minLevel * l3tmp_numPre * domain_rect_numFragsPerBlock_y + 0.08817599420458942 * poly_optLevel_fine * l3tmp_numPre * minLevel * domain_rect_numFragsPerBlock_x + 11.083562597795163 * l3tmp_numPre * minLevel + 1.134838312447672 * RBGS * l3tmp_numPre * maxLevel + -0.9304377836025013 * domain_fragmentLength_y * RBGS * l3tmp_numPre * l3tmp_numRecCycleCalls * domain_rect_numFragsPerBlock_y + 27.56747433295817 * 1 + -0.011561652800814939 * domain_fragmentLength_y * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * domain_fragmentLength_x * maxLevel + -43.75616953340436 * 1 + 0.7825257384886356 * domain_fragmentLength_x + 0.01828358548009792 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * domain_fragmentLength_y * maxLevel + -0.10942136448727624 * domain_rect_numFragsPerBlock_x + -9.696997790735045 * l3tmp_numRecCycleCalls * minLevel + 5.124355062632224 * maxLevel + 0.29237588074632226 * l3tmp_numPost * RBGS * poly_optLevel_fine * minLevel + -0.1094697785167861 * comm_useFragmentLoopsForEachOp + 11.415549706831706 * minLevel + 0.01966492411798817 * mpi_useCustomDatatypes * domain_rect_numFragsPerBlock_x + 0.013805416554945587 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 0.011998083001936896 * l3tmp_numPre * poly_optLevel_fine * maxLevel + -0.006796431037699057 * l3tmp_numPost * domain_fragmentLength_y + -0.53047694288809 * l3tmp_numRecCycleCalls * maxLevel + -0.005382118168399348 * opt_unroll * maxLevel * l3tmp_numRecCycleCalls + 1.3947987734237583E-4 * poly_optLevel_fine * poly_tileSize_x * maxLevel + 0.08729778090514007 * domain_fragmentLength_y * l3tmp_numRecCycleCalls + 0.14979872814703596 * domain_fragmentLength_x * minLevel + 0.0024628079401805285 * minLevel * domain_rect_numFragsPerBlock_x + -38.73679317802393 * 1 + 4.979129458304223 * maxLevel"
    //      learnedModel = "0.0026598580417355982 * 2 * 9 + 0.0037796176680069604 * domain_fragmentLength_y * 9 + 0.004560257721061133 * l3tmp_numPost + 0.8652450803851031 * 9 + 0.762071560955187 * minLevel + -0.008528945840465041 * Jac * 1 + 0.03328437545956467 * poly_optLevel_fine * minLevel + -0.854715884099006 * 1 * minLevel + 0.009586647036392825 * 1 * 64 + -8.01330503146351 * 1 + 0.050558898392768975 * poly_optLevel_fine * 1 + 0.013129280512490205 * opt_useAddressPrecalc + -14.96313827766639 * 1 + -0.3676108446596291 * 1 * 9 + -0.06749139660287894 * 2 * 64 + 0.6067769811810128 * minLevel + 0.07543094123475347 * domain_fragmentLength_y * minLevel * 2 + 0.06696492508953497 * opt_vectorize * 2 + 0.010878746310418214 * 1 * 64 * 9 + 2.2439422650090224 * 9 + 11.880296564311788 * l3tmp_numPost * minLevel + -2.467508096254454 * Jac * minLevel * l3tmp_numPost * 1 + -0.10841560691455333 * domain_fragmentLength_y * RBGS * l3tmp_numPost * 1 * 1 * 9 + 22.384219592112647 * 1 + 3.1394728651733486 * 1 * l3tmp_numPost + -22.258384105581424 * 1 + 1.235078187301263 * RBGS * l3tmp_numPost * 9 + 0.03285389023851206 * mpi_useCustomDatatypes * l3tmp_numPost * 1 * 64 + 3.0806189910770523 * 1 * l3tmp_numPre + -1.522668021839599 * l3tmp_useSlotsForJac + -23.477816009352015 * 1 + -2.2038793634551954 * Jac * minLevel * l3tmp_numPre * 1 + 0.08817599420458942 * poly_optLevel_fine * l3tmp_numPre * minLevel * 64 + 11.083562597795163 * l3tmp_numPre * minLevel + 1.134838312447672 * RBGS * l3tmp_numPre * 9 + -0.9304377836025013 * domain_fragmentLength_y * RBGS * l3tmp_numPre * 1 * 1 + 27.56747433295817 * 1 + -0.011561652800814939 * domain_fragmentLength_y * 1 * 1 * 2 * 9 + -43.75616953340436 * 1 + 0.7825257384886356 * 2 + 0.01828358548009792 * 1 * 64 * domain_fragmentLength_y * 9 + -0.10942136448727624 * 64 + -9.696997790735045 * 1 * minLevel + 5.124355062632224 * 9 + 0.29237588074632226 * l3tmp_numPost * RBGS * poly_optLevel_fine * minLevel + -0.1094697785167861 * 1 + 11.415549706831706 * minLevel + 0.01966492411798817 * mpi_useCustomDatatypes * 64 + 0.013805416554945587 * 1 * 64 + 0.011998083001936896 * l3tmp_numPre * poly_optLevel_fine * 9 + -0.006796431037699057 * l3tmp_numPost * domain_fragmentLength_y + -0.53047694288809 * 1 * 9 + -0.005382118168399348 * 5 * 9 * 1 + 1.3947987734237583E-4 * poly_optLevel_fine * poly_tileSize_x * 9 + 0.08729778090514007 * domain_fragmentLength_y * 1 + 0.14979872814703596 * 2 * minLevel + 0.0024628079401805285 * minLevel * 64 + -38.73679317802393 * 1 + 4.979129458304223 * 9"

    //      learnedModel = "0.0026598580417355982 * domain_fragmentLength_x * maxLevel + 0.0037796176680069604 * domain_fragmentLength_y * maxLevel + 0.004560257721061133 * l3tmp_numPost + 0.8652450803851031 * maxLevel + 0.762071560955187 * minLevel + -0.008528945840465041 * Jac * domain_rect_numFragsPerBlock_y + 0.03328437545956467 * poly_optLevel_fine * minLevel + -0.854715884099006 * l3tmp_numRecCycleCalls * minLevel + 0.009586647036392825 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -8.01330503146351 * 1 + 0.050558898392768975 * poly_optLevel_fine * domain_rect_numFragsPerBlock_y + 0.013129280512490205 * opt_useAddressPrecalc + -14.96313827766639 * 1 + -0.3676108446596291 * l3tmp_numRecCycleCalls * maxLevel + -0.06749139660287894 * domain_fragmentLength_x * domain_rect_numFragsPerBlock_x + 0.6067769811810128 * minLevel + 0.07543094123475347 * domain_fragmentLength_y * minLevel * domain_fragmentLength_x + 0.06696492508953497 * opt_vectorize * domain_fragmentLength_x + 0.010878746310418214 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * maxLevel + 2.2439422650090224 * maxLevel + 11.880296564311788 * l3tmp_numPost * minLevel + -2.467508096254454 * Jac * minLevel * l3tmp_numPost * domain_rect_numFragsPerBlock_y + -0.10841560691455333 * domain_fragmentLength_y * RBGS * l3tmp_numPost * domain_rect_numFragsPerBlock_y * l3tmp_numRecCycleCalls * maxLevel + 22.384219592112647 * 1 + 3.1394728651733486 * domain_rect_numFragsPerBlock_y * l3tmp_numPost "

    //        if (dimToConsider == 2 && ccVSvc.equals("vc"))
    //          learnedModel = "-9.823367356467688 * opt_unroll + -22.80475638447024 * l3tmp_numPre + 7.493748952680161E15 * Jac + -1.2526630438708684E13 * domain_rect_numFragsPerBlock_x + -56.238135860072 * domain_rect_numFragsPerBlock_y + 110.28963279742973 * domain_fragmentLength_y + 7.493748952680136E15 * RBGS + 0.08598701397960785 * poly_tileSize_x + -1.2944184786690056E14 * domain_fragmentLength_x + 66.85352734598186 * l3tmp_useSlotsForJac + 1.2944184786702181E14 * minLevel * domain_rect_numFragsPerBlock_x * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x + -3.468525038377703E15 * 1 + 53.836070645135244 * opt_unroll_interleave + -6.757703723101701 * comm_useFragmentLoopsForEachOp + 1.252663043869541E13 * minLevel * domain_rect_numFragsPerBlock_x + 22.619229118613518 * l3tmp_numRecCycleCalls + -109.69185136603845 * opt_vectorize + -27.632309023854358 * mpi_useCustomDatatypes + -1.2526630439211836E13 * minLevel + -7.083445898075203 * l3tmp_numPost + 1.9094717635660984E16 * maxLevel + -43.77875702806896 * poly_optLevel_fine + -1.9483043179260616E16 * maxLevel + -1.2944184786698339E14 * minLevel * domain_rect_numFragsPerBlock_x * domain_rect_numFragsPerBlock_y + 30.595173302766916 * opt_useAddressPrecalc"
    //    
    //        if (dimToConsider == 3 && ccVSvc.equals("cc"))
    //          learnedModel = "131.55559748049566 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x + -1.751020673673363E14 * l3tmp_numPost + 4.743233982404961E14 * domain_rect_numFragsPerBlock_y + 7.2110074980131165 * poly_optLevel_fine + -46.41936048625558 * opt_useAddressPrecalc + -2.6379995867751705E15 * minLevel + 32.90171559255296 * domain_fragmentLength_y * domain_fragmentLength_z + 8.081242281711642 * l3tmp_numPre + 0.09015896558654206 * poly_tileSize_x + 12.788380233765182 * l3tmp_useSlotsForJac + 20.773964067740852 * opt_unroll + -11.298155360693727 * domain_rect_numFragsPerBlock_z + -4.743233982406324E14 * domain_rect_numFragsPerBlock_y + 75.70405586108987 * Jac + -0.4149395050669495 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -9.88743152809839E14 * domain_fragmentLength_y + 1.7419329820950004E13 * maxLevel + 1.7510206736733525E14 * l3tmp_numPost + 70.82783734084664 * comm_useFragmentLoopsForEachOp + 20.03377492939528 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x * domain_rect_numFragsPerBlock_z + -2.8737370970461216 * opt_unroll_interleave + -30.064521863607258 * l3tmp_numRecCycleCalls + 9.887431528098819E14 * domain_fragmentLength_y + -67.57806757006787 * opt_vectorize + -5.995430946572485 * mpi_useCustomDatatypes + -1.045159789254908E14 * 1 + 2.6379995867750915E15 * minLevel + 0.3695740885770345 * poly_tileSize_y + -38.22668765266216 * minLevel * domain_fragmentLength_y"
    //    
    //        if (dimToConsider == 3 && ccVSvc.equals("vc"))
    //          learnedModel = "-62.09165073622925 * comm_useFragmentLoopsForEachOp + -1.0342112095861066E14 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * domain_rect_numFragsPerBlock_z * domain_rect_numFragsPerBlock_x + -48.792733456881024 * RBGS + 23.045996087336192 * poly_optLevel_fine + -0.04308536610671082 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * domain_rect_numFragsPerBlock_z * domain_rect_numFragsPerBlock_x * maxLevel * l3tmp_numPre + -7.788249110930933E11 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * maxLevel + 5.30120034841732 * l3tmp_numPost + 1.7236853493103059E13 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * domain_rect_numFragsPerBlock_z * domain_rect_numFragsPerBlock_x * maxLevel + 29.81350182824211 * opt_useAddressPrecalc + -0.07657100783278278 * poly_tileSize_x + -0.014420060012994982 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * maxLevel * opt_unroll + 1.6513419330539802 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * domain_rect_numFragsPerBlock_z * domain_rect_numFragsPerBlock_x * maxLevel * domain_fragmentLength_z + 0.16618697592986034 * poly_tileSize_y + 821.1466345880615 * 1 + -127.7193135808727 * domain_rect_numFragsPerBlock_y + 38.788258427732345 * mpi_useCustomDatatypes + -26.05262251160333 * opt_vectorize + 74.41313571563397 * minLevel + -1.0809377694258684 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x * domain_rect_numFragsPerBlock_z + 45.11499822758881 * l3tmp_numRecCycleCalls + 26.92400060874349 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_y + 48.745004605327644 * opt_unroll_interleave + 63.941488494300884 * l3tmp_useSlotsForJac + 4.672949466662614E12 * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x"

    //      learnedModel = " 55.437596366293455 * opt_unroll_interleave + -11.228519945896176 * l3tmp_numPre + 3.3393671033953973E13 * Jac + 69.86442732338648 * l3tmp_useSlotsForJac + -66.38508382833251 * domain_rect_numFragsPerBlock_y + 6.794111014475234 * opt_unroll + 3.339367103393321E13 * RBGS + -107.88244285358304 * opt_vectorize + -228.72448328135903 * domain_fragmentLength_x + -6.937417330095363 * l3tmp_numPost + 97.54144410402726 * domain_fragmentLength_y + 344.8638923945346 * minLevel * domain_rect_numFragsPerBlock_x * domain_rect_numFragsPerBlock_y * domain_fragmentLength_x + -3.33936710363456E13 * 1 + -8.405498889385166 * comm_useFragmentLoopsForEachOp + -14.490325915455138 * minLevel * domain_rect_numFragsPerBlock_x + 25.426787687775423 * l3tmp_numRecCycleCalls + 34.026819223184525 * opt_useAddressPrecalc + -21.13654568361571 * mpi_useCustomDatatypes + -441.8873233527695 * minLevel + -2.0718727994675705E15 * maxLevel + 0.08573150144798243 * poly_tileSize_x + -43.287065742565254 * poly_optLevel_fine + 2.0718727994679252E15 * maxLevel + -306.877280068173 * minLevel * domain_rect_numFragsPerBlock_x * domain_rect_numFragsPerBlock_y"

    //    var learnedModel = "244.84377061122203 * maxLevel * l3tmp_numRecCycleCalls + -0.010304503952783218 * maxLevel * poly_tileSize_x + 6.11945949776492 * maxLevel * domain_fragmentLength_x + 1953.7117775214467 * 1 * l3tmp_numPost + -33.585793125311206 * domain_rect_numFragsPerBlock_y + -14274.709397211633 * 1 + 4.408791246221592E15 * 1 * maxLevel + -0.8338247717230979 * 1 * l3tmp_numPost * domain_rect_numFragsPerBlock_x + -4.408791246220202E15 * maxLevel + -231.16807633099333 * maxLevel * l3tmp_numPost + 165.44353484765193 * domain_rect_numFragsPerBlock_y * Jac + -3.4375488679948316 * 1 * l3tmp_numPost * domain_rect_numFragsPerBlock_x * RBGS + 25.88438999077382 * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x"

    //    var learnedModel = "23.167584587322995 * l3tmp_numPre + 5.020280919564926 * opt_unroll + 2.5856856374644672 * 1 * maxLevel * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + 8.935684845635278E14 * 1 * maxLevel + -24.6741795003621 * comm_useFragmentLoopsForEachOp + -14.99365293536718 * 1 * maxLevel * domain_rect_numFragsPerBlock_y * RBGS + -2.8213934014418114 * 1 * l3tmp_numPost * domain_rect_numFragsPerBlock_x + -232.2348811477041 * 1 * maxLevel * l3tmp_numPost + -8.981845132160192 * opt_vectorize + 81.94363357137048 * poly_optLevel_fine + 12.018778770795691 * 1 * maxLevel * domain_rect_numFragsPerBlock_y + 47.55920346198764 * mpi_useCustomDatatypes + -15.785304666013298 * minLevel + 1966.263516352193 * 1 * l3tmp_numPost + -14179.480256659297 * 1 + 6.113152980154068 * 1 * maxLevel * domain_fragmentLength_x + -8.935684845621485E14 * 1 * maxLevel + 251.31838860558167 * 1 * maxLevel * l3tmp_numRecCycleCalls + -6.807704054919183 * opt_unroll_interleave + 0.8254332677309988 * domain_fragmentLength_y + -23.220241959069504 * opt_useAddressPrecalc + -0.045049738164375505 * poly_tileSize_x + -177.93200612985237 * 1 * l3tmp_useSlotsForJac"

    //    var learnedModel = "23.167584587322995 * l3tmp_numPre + 5.020280919564926 * opt_unroll + 2.5856856374644672 * 1 * maxLevel * domain_rect_numFragsPerBlock_y * domain_rect_numFragsPerBlock_x + -24.6741795003621 * comm_useFragmentLoopsForEachOp + -14.99365293536718 * 1 * maxLevel * domain_rect_numFragsPerBlock_y * RBGS + -2.8213934014418114 * 1 * l3tmp_numPost * domain_rect_numFragsPerBlock_x + -232.2348811477041 * 1 * maxLevel * l3tmp_numPost + -8.981845132160192 * opt_vectorize + 81.94363357137048 * poly_optLevel_fine + 12.018778770795691 * 1 * maxLevel * domain_rect_numFragsPerBlock_y + 47.55920346198764 * mpi_useCustomDatatypes + -15.785304666013298 * minLevel + 1966.263516352193 * 1 * l3tmp_numPost + -14179.480256659297 * 1 + 6.113152980154068 * 1 * maxLevel * domain_fragmentLength_x + 251.31838860558167 * 1 * maxLevel * l3tmp_numRecCycleCalls + -6.807704054919183 * opt_unroll_interleave + 0.8254332677309988 * domain_fragmentLength_y + -23.220241959069504 * opt_useAddressPrecalc + -0.045049738164375505 * poly_tileSize_x + -177.93200612985237 * 1 * l3tmp_useSlotsForJac"

    println("--------------------------------")
    var i = 0

    //        y._2.foreach { x =>
    //          {
    //            println("--------")
    //            println(y._1.get(i, 0))
    //            i += 1
    //            println(x.toOSiL_syntax(ffs.nameToFeatureAndID))
    //          }
    //        }
    var derivedDomainPart : scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
    derivedDomainPart.put("help_domain_rect_numFragsPerBlock_x", ffs.nameToFeatureAndID.size + derivedDomainPart.size)
    derivedDomainPart.put("help_domain_rect_numFragsPerBlock_y", ffs.nameToFeatureAndID.size + derivedDomainPart.size)
    derivedDomainPart.put("help_domain_rect_numFragsPerBlock_z", ffs.nameToFeatureAndID.size + derivedDomainPart.size)

    derivedDomainPart.put("help_domain_rect_numBlocks_x", ffs.nameToFeatureAndID.size + derivedDomainPart.size)
    derivedDomainPart.put("help_domain_rect_numBlocks_y", ffs.nameToFeatureAndID.size + derivedDomainPart.size)
    derivedDomainPart.put("help_domain_rect_numBlocks_z", ffs.nameToFeatureAndID.size + derivedDomainPart.size)

    var numbers = 0

    var identifierAndTime : StringBuilder = new StringBuilder()

    var bestValue = Double.MaxValue
    var bestIdentifier = ""

    for (ifPart <- 1 to 3) {
      lfaConfigsCaseStudy.foreach { currLFA =>
        {
          var currModel = currLFA.iterationsNeeded + " * " + learnedModel.replace("+", "+ " + currLFA.iterationsNeeded + " * ")

          var featuresDefined : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
          var newFeaturesFound = true
          var identifier = ""
          while (featuresToConsider.size > featuresDefined.size && newFeaturesFound) {

            identifier = dimToConsider + "_" + ccVSvc + "_" + currLFA.numPre + "_" + currLFA.numPost + "_JAC-" + currLFA.Smoother.equals("Jac") + "_ifPart-" + ifPart + "_" + featuresDefined.size

            // replace variables considered in lfa with values
            currModel = currModel.replace(" l3tmp_numPost ", "" + currLFA.numPost)
            currModel = currModel.replace(" l3tmp_numPre ", "" + currLFA.numPre)
            if (currLFA.Smoother.equals("\"Jac\"")) {
              currModel = currModel.replace(" Jac ", "" + 1)
              currModel = currModel.replace(" RBGS ", "" + 0)
            } else {
              //              currModel = currModel.replace(" l3tmp_useSlotsForJac ", "" + 0)
              currModel = currModel.replace(" Jac ", "" + 0)
              currModel = currModel.replace(" RBGS ", "" + 1)
            }

            var allExpr : FFS_Expression = new FFS_Expression(ffs.featuresOfDomain, currModel)
            var stringBuild : StringBuilder = new StringBuilder()

            if (featuresDefined.size > 0)
              printOSiLSyntax(ffs, allExpr, ifPart, identifier, featuresDefined, derivedDomainPart)
            else
              printOSiLSyntax(ffs, allExpr, ifPart, identifier, derivedDomainPart)

            //        printGAMSSyntax(ffs, allExpr, 3)

            featuresDefined.foreach(x => {
              currModel = currModel.replace(" " + x._1 + " ", " " + x._2 + " ")
            })

            //            printGAMSSyntax(allExpr, 3)

            var result = startOptimizer(identifier)

            //        var result = optimium - 1

            numbers += 1

            println("round nr " + numbers)

            var beforeNumber = featuresDefined.size

            featuresDefined ++= getOptimalKnowledgeFile(currLFA, result, "" + numbers)

            val objvar = getObjVar(result)

            if (objvar < bestValue) {
              bestValue = objvar
              bestIdentifier = identifier
            }

            if (featuresDefined.size == beforeNumber) {
              newFeaturesFound = false;
              identifierAndTime.append(identifier + ";" + objvar + "\n")
            }
            identifierAndTime.append(identifier + ";" + objvar + "\n")
            println("defined features" + featuresDefined.size)
            newFeaturesFound = false;
            println("")
          }
          printOptimalKnowledgeFile(currLFA, featuresDefined, identifier, currModel)
          print("")
        }
      }
    }
    println(identifierAndTime.toString())
    println("the best one is: " + bestIdentifier)

  }

  def getObjVar(optimumContent : scala.collection.mutable.Set[String]) : Double = {
    optimumContent.foreach { x =>
      {
        var content = x.split(" ")
        if ((content(0).equals("objvar"))) {
          val name = (content(0))
          for (a <- 1 to content.length - 1) {
            if (content(a).length() > 0) {
              return content(a).toDouble
            }
          }
        }
      }
    }
    return 0.0
  }

  def getOptimalKnowledgeFile(lfaConfig : LFAConfig, optimumContent : scala.collection.mutable.Set[String], suffix : String) : scala.collection.mutable.Map[String, String] = {

    var config : Configuration = new Configuration()

    var features : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    var featureMap : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    featuresToConsider.foreach { x => features.add(x) }

    featureMap.put("l3tmp_numPre", lfaConfig.numPre + "")
    features.remove("l3tmp_numPre")

    featureMap.put("l3tmp_numPost", lfaConfig.numPost + "")
    features.remove("l3tmp_numPost")

    featureMap.put("l3tmp_smoother", lfaConfig.Smoother)
    features.remove("l3tmp_smoother")

    optimumContent.foreach { x =>
      {
        var content = x.split(" ")
        if (featuresToConsider.contains(content(0))) {
          val name = (content(0))
          var notPrinted = true
          for (a <- 1 to content.length - 1) {
            if (content(a).length() > 0 && notPrinted) {
              if (!FeatureModel.get(name).isNumerical) {
                if (content(a).equals("1")) {
                  featureMap.put(name, "true")
                } else {
                  featureMap.put(name, "false")
                }
                features.remove(name)

              } else {
                featureMap.put(name, Math.round(content(a).toDouble).toString())
                features.remove(name)
              }
              notPrinted = false
            }
          }
        }
      }
    }

    return featureMap
  }

  def printOptimalKnowledgeFile(lfaConfig : LFAConfig, featuresDefined : scala.collection.mutable.Map[String, String], identifier : String, model : String) = {

    var config : Configuration = new Configuration()

    var features : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    var definedFeatureMap : scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()

    featuresToConsider.foreach { x => features.add(x) }
    definedFeatureMap.put("l3tmp_numPre", lfaConfig.numPre + "")
    features.remove("l3tmp_numPre")
    featuresDefined.remove("l3tmp_numPre")

    definedFeatureMap.put("l3tmp_numPost", lfaConfig.numPost + "")
    features.remove("l3tmp_numPost")
    featuresDefined.remove("l3tmp_numPost")

    definedFeatureMap.put("l3tmp_smoother", lfaConfig.Smoother)
    features.remove("l3tmp_smoother")
    featuresDefined.remove("l3tmp_smoother")

    featuresDefined.foreach { x =>
      {
        var name = x._1
        if (featuresToConsider.contains(name)) {
          val value = x._2
          if (!FeatureModel.get(name).isNumerical) {
            if (value.equals("true")) {
              definedFeatureMap.put(name, "true")
            } else {
              definedFeatureMap.put(name, "false")
            }
            features.remove(name)

          } else {
            definedFeatureMap.put(name, Math.round(value.toInt).toString())
            features.remove(name)
          }

        }
      }
    }

    // definedFeatureMap = definedNonSelectedFeatures(definedFeatureMap, features, model)

    features.foreach(featName => {
      if (!FeatureModel.get(featName).isNumerical) {
        definedFeatureMap.put(featName, "false")
      } else {

        definedFeatureMap.put(featName, FeatureModel.get(featName).getMinValue().toString())
      }

    })

    config.readSolutionUnseparated(definedFeatureMap)

    var problemDefinition : scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()

    if (dimToConsider == 2 && ccVSvc.equals("cc"))
      problemDefinition2D_ConstCoeff(problemDefinition)
    if (dimToConsider == 2 && ccVSvc.equals("vc"))
      problemDefinition2D_VarCoeff(problemDefinition)
    if (dimToConsider == 3 && ccVSvc.equals("cc"))
      problemDefinition3D_ConstCoeff(problemDefinition)
    if (dimToConsider == 3 && ccVSvc.equals("vc"))
      problemDefinition3D_VarCoeff(problemDefinition)
    config.partialBaseConfig.++=(problemDefinition)

    derivedParameters(config)

    var configToKnowFile : scala.collection.mutable.Set[String] = config.getKnowledgeFileContent(scala.collection.mutable.Set())

    configToKnowFile.add(partialBaseConfigInformation())

    var scriptFile = basePathSolver + "/optimum/optimum_" + identifier + ".knowledge"
    var writer = new PrintWriter(new File(scriptFile))

    var stringBuild : StringBuilder = new StringBuilder()
    configToKnowFile.foreach { x => stringBuild.append(x + "\n") }

    writer.write(stringBuild.toString())

    writer.flush()
    writer.close()

  }

  val basePathSolver = "E:\\MixedIntegerNonLinearProgrammingSolver\\solver"

  def startOptimizer(identifier : String) : scala.collection.mutable.Set[String] = {
    var resultFile : File = new File(basePathSolver + "/sol" + identifier + ".txt")
    if (resultFile.exists())
      resultFile.delete()

    val startBatContent = " cd " + basePathSolver + "/\n scip-3.1.1.1.win.x86_64.msvc.opt.spx.ld.exe -b " + basePathSolver + "/autoCommands_" + identifier + ".bat" //\n exit "
    var scriptFile = basePathSolver + "/start" + identifier + ".bat"
    var writer = new PrintWriter(new File(scriptFile))
    writer.write(startBatContent)
    writer.flush()
    writer.close()

    val autoCommandContent = "read " + basePathSolver + "/osil_script_" + identifier + ".osil\n optimize\n write solution\n " + basePathSolver + "/sol" + identifier + ".txt" //\n quit"
    var commandFile = basePathSolver + "/autoCommands_" + identifier + ".bat"
    writer = new PrintWriter(new File(commandFile))
    writer.write(autoCommandContent)
    writer.flush()
    writer.close()

    var p : Process = Runtime.getRuntime().exec("cmd /c start " + basePathSolver + "/start" + identifier + ".bat");
    //    var p : Process = Runtime.getRuntime().exec("cmd /c start /B " + basePathSolver + "/start" + identifier + ".bat");
    p.waitFor()

    var stdin : java.io.InputStream = p.getInputStream();
    var isr : java.io.InputStreamReader = new java.io.InputStreamReader(stdin);
    var br : java.io.BufferedReader = new java.io.BufferedReader(isr);
    var line : String = null;
    var break = false
    while (!break) {
      line = br.readLine()
      if (line == null) {
        break = true
      }
    }

    //    while (!resultFile.exists()) {
    //      println("not finished")
    //    }
    p.destroy()
    var contentFromFile = scala.io.Source.fromFile(basePathSolver + "/sol" + identifier + ".txt").getLines()
    // copy content to other set
    var content : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()

    contentFromFile.foreach { x => content.add(x) }

    resultFile.delete()
    return content
  }

  def printOSiLSyntax(ffs : ForwardFeatureSelection, expr : FFS_Expression, idDefPat : Int, identifier : String, derivedDomainParts : scala.collection.mutable.Map[String, Int]) = {
    var stringBuild : StringBuilder = new StringBuilder()

    println(expr.toString())

    stringBuild.append("<osil xmlns=\"os.optimizationservices.org\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"os.optimizationservices.org http://www.optimizationservices.org/schemas/2.0/OSiL.xsd\">\n" +
      "<instanceHeader>\n" +
      "<name>SISC</name>\n" +
      //      "<source>Computing Journal 3:175-184, 1960</source>\n" +
      //      "<description>Rosenbrock problem with constraints</description>\n" +
      "</instanceHeader>\n")

    stringBuild.append("<instanceData>\n")

    // variables
    stringBuild.append("<variables numberOfVariables=\"" + (ffs.nameToFeatureAndID.size + derivedDomainParts.size) + "\">\n")

    for (a <- 0 to ffs.nameToFeatureAndID.size) {
      ffs.nameToFeatureAndID.foreach(x => {
        if (x._2._2 == a) {
          if (!x._2._1.isNumerical || x._2._1.isXorFeature)
            stringBuild.append("<var lb=\"0\" name=\"" + x._1 + "\" type=\"I\" ub=\"1.0\"/> \n")
          else {
            if (x._1.equals("poly_tileSize_x") || x._1.equals("poly_tileSize_y"))
              stringBuild.append("<var lb=\"" + x._2._1.minValue + "\" ub=\"1120\" name=\"" + x._1 + "\" type=\"I\"/> \n")
            else
              stringBuild.append("<var lb=\"" + x._2._1.minValue + "\" ub=\"" + x._2._1.maxValue + "\" name=\"" + x._1 + "\" type=\"I\"/> \n")
          }
        }
      })
    }

    for (a <- ffs.nameToFeatureAndID.size - 1 to ffs.nameToFeatureAndID.size + derivedDomainParts.size) {
      derivedDomainParts.foreach(x => {
        if (x._2 == a) {
          stringBuild.append("<var lb=\"" + 0 + "\" ub=\"" + 20 + "\" name=\"" + x._1 + "\" type=\"I\"/> \n")
        }
      })
    }

    stringBuild.append("</variables>\n")

    // objectives
    stringBuild.append("<objectives numberOfObjectives=\"1\">\n" +
      "<obj maxOrMin=\"min\" name=\"minCost\" numberOfObjCoef=\"1\">\n" +
      "</obj>\n" +
      "</objectives>\n")

    // constraints  
    var numberOfConstraints = 0
    var osil : OSILSyntax = null

    if (dimToConsider == 2)
      osil = new OSILSyntax(ffs.nameToFeatureAndID, dimToConsider, num_points_per_dim2D, idDefPat, derivedDomainParts)
    else
      osil = new OSILSyntax(ffs.nameToFeatureAndID, dimToConsider, num_points_per_dim3D, idDefPat, derivedDomainParts)

    var constraints = osil.getConstraints()

    stringBuild.append("<constraints numberOfConstraints=\"" + constraints.size + "\">\n")
    constraints.foreach(x => stringBuild.append(x._1))

    stringBuild.append("</constraints>\n")

    stringBuild.append("<nonlinearExpressions numberOfNonlinearExpressions=\"" + (1 + constraints.size) + "\">\n")
    stringBuild.append("<nl idx=\"-1\">\n")
    expr.toOSiL_syntax(ffs.nameToFeatureAndID, stringBuild)
    stringBuild.append("</nl>\n")

    constraints.foreach(x => stringBuild.append(x._2))
    stringBuild.append("</nonlinearExpressions>\n" +

      "</instanceData>\n" +
      "</osil>\n")

    var scriptFile = basePathSolver + "/osil_script_" + identifier + ".osil"
    var writer = new PrintWriter(new File(scriptFile))

    writer.write(stringBuild.toString())

    writer.flush()
    writer.close()

  }

  def printOSiLSyntax(ffs : ForwardFeatureSelection, expr : FFS_Expression, ifDefPart : Int, identifier : String, featuresDefined : scala.collection.mutable.Map[String, String], derivedDomainParts : scala.collection.mutable.Map[String, Int]) = {
    var stringBuild : StringBuilder = new StringBuilder()

    println(expr.toString())

    stringBuild.append("<osil xmlns=\"os.optimizationservices.org\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"os.optimizationservices.org http://www.optimizationservices.org/schemas/2.0/OSiL.xsd\">\n" +
      "<instanceHeader>\n" +
      "<name>SISC</name>\n" +
      //      "<source>Computing Journal 3:175-184, 1960</source>\n" +
      //      "<description>Rosenbrock problem with constraints</description>\n" +
      "</instanceHeader>\n")

    stringBuild.append("<instanceData>\n")

    // variables
    stringBuild.append("<variables numberOfVariables=\"" + (ffs.nameToFeatureAndID.size + derivedDomainParts.size) + "\">\n")

    for (a <- 0 to ffs.nameToFeatureAndID.size) {
      ffs.nameToFeatureAndID.foreach(x => {
        if (x._2._2 == a) {
          if (featuresDefined.contains(x._2._1.identifier) && !x._2._1.identifier.equals("l3tmp_smoother")) {
            var value = featuresDefined(x._2._1.identifier)
            if (value.equals("true"))
              stringBuild.append("<var lb=\"1\" name=\"" + x._1 + "\" type=\"I\" ub=\"1.0\"/> \n")
            else if (value.equals("false"))
              stringBuild.append("<var lb=\"0\" name=\"" + x._1 + "\" type=\"I\" ub=\"0\"/> \n")
            else
              stringBuild.append("<var lb=\"" + value + "\" name=\"" + x._1 + "\" type=\"I\" ub=\"" + value + "\"/> \n")
          } else {

            if (!x._2._1.isNumerical || x._2._1.isXorFeature)
              stringBuild.append("<var lb=\"0\" name=\"" + x._1 + "\" type=\"I\" ub=\"1.0\"/> \n")
            else {
              if (x._1.equals("poly_tileSize_x") || x._1.equals("poly_tileSize_y"))
                stringBuild.append("<var lb=\"" + x._2._1.minValue + "\" ub=\"1120\" name=\"" + x._1 + "\" type=\"I\"/> \n")
              else
                stringBuild.append("<var lb=\"" + x._2._1.minValue + "\" ub=\"" + x._2._1.maxValue + "\" name=\"" + x._1 + "\" type=\"I\"/> \n")
            }
          }
        }
      })
    }

    for (a <- ffs.nameToFeatureAndID.size - 1 to ffs.nameToFeatureAndID.size + derivedDomainParts.size) {
      derivedDomainParts.foreach(x => {
        if (x._2 == a) {
          stringBuild.append("<var lb=\"" + 0 + "\" ub=\"" + 20 + "\" name=\"" + x._1 + "\" type=\"I\"/> \n")
        }
      })
    }

    stringBuild.append("</variables>\n")

    // objectives
    stringBuild.append("<objectives numberOfObjectives=\"1\">\n" +
      "<obj maxOrMin=\"min\" name=\"minCost\" numberOfObjCoef=\"1\">\n" +
      "</obj>\n" +
      "</objectives>\n")

    // constraints  
    var numberOfConstraints = 0
    var osil : OSILSyntax = null

    if (dimToConsider == 2)
      osil = new OSILSyntax(ffs.nameToFeatureAndID, dimToConsider, num_points_per_dim2D, ifDefPart, derivedDomainParts)
    else
      osil = new OSILSyntax(ffs.nameToFeatureAndID, dimToConsider, num_points_per_dim3D, ifDefPart, derivedDomainParts)

    var constraints = osil.getConstraints()

    stringBuild.append("<constraints numberOfConstraints=\"" + constraints.size + "\">\n")
    constraints.foreach(x => stringBuild.append(x._1))

    stringBuild.append("</constraints>\n")

    stringBuild.append("<nonlinearExpressions numberOfNonlinearExpressions=\"" + (1 + constraints.size) + "\">\n")
    stringBuild.append("<nl idx=\"-1\">\n")
    expr.toOSiL_syntax(ffs.nameToFeatureAndID, stringBuild)
    stringBuild.append("</nl>\n")

    constraints.foreach(x => stringBuild.append(x._2))
    stringBuild.append("</nonlinearExpressions>\n" +

      "</instanceData>\n" +
      "</osil>\n")

    var scriptFile = basePathSolver + "/osil_script_" + identifier + ".osil"

    var writer = new PrintWriter(new File(scriptFile))
    writer.write(stringBuild.toString())

    writer.flush()
    writer.close()

  }

  def samplingWithPartitionedFeatureSpace() : Boolean = {

    //  splitting feature in binary and numeric (numeric features with less than 5 values are used in the binary samplings)  
    var featuresBinary : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var featuresNumeric : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    FeatureModel.allFeatures.foreach(x => {
      if (x._2.isXorFeature || !x._2.isNumerical)
        featuresBinary.add(x._2)
      else featuresNumeric.add(x._2)
    });

    println("numeric:::: " + featuresNumeric.size)
    featuresNumeric.foreach(x => { println(x.identifier) })
    println("--------------------------------")

    println("binary:::: " + featuresBinary.size)
    featuresBinary.foreach(x => { println(x.identifier) })
    println("--------------------------------")

    // partition numeric options to domain_ALL,maxLevel and other features
    var numericOptionsFirstSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var numericOptionsSecondSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    var numericOptionsThirdSampling : scala.collection.mutable.Set[Feature] = scala.collection.mutable.Set()
    featuresNumeric.foreach(x =>
      {
        if (x.identifier.startsWith("domain") || x.identifier.equals("numNodes") // || x.identifier.startsWith("numOMP_")
          || x.identifier.equals("ranksPerNode") || x.identifier.startsWith("aro_")) {
          numericOptionsFirstSampling.add(x)
          println("1: " + x)
        } else if (x.identifier.startsWith("numOMP_") || x.identifier.startsWith("opt_unroll") || x.identifier.startsWith("l3tmp") || x.identifier.startsWith("minLevel") || x.identifier.startsWith("poly_tileSize_x")) {
          numericOptionsThirdSampling.add(x)
          println("3: " + x)
        } else {
          numericOptionsSecondSampling.add(x)
          println("2: " + x)
        }
      })
    println("Numeric First  Set size " + numericOptionsFirstSampling.size)
    println("Numeric Second Set size " + numericOptionsSecondSampling.size)

    // end numeric option splitting

    //// Start FW Sampling ////////////////////////////////////////////////////

    var binaryConfigs : scala.collection.mutable.Set[scala.collection.mutable.Map[Feature, String]] = scala.collection.mutable.Set()
    // binary Sampling
    var fwSampling = new FWHeuristic(featuresBinary)
    binaryConfigs ++= fwSampling.getPoints()
    println("binary FW " + binaryConfigs.size)

    var problemDefinition : scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()

    if (dimToConsider == 2 && ccVSvc.equals("cc"))
      problemDefinition2D_ConstCoeff(problemDefinition)
    if (dimToConsider == 2 && ccVSvc.equals("vc"))
      problemDefinition2D_VarCoeff(problemDefinition)
    if (dimToConsider == 3 && ccVSvc.equals("cc"))
      problemDefinition3D_ConstCoeff(problemDefinition)
    if (dimToConsider == 3 && ccVSvc.equals("vc"))
      problemDefinition3D_VarCoeff(problemDefinition)

    // numeric Sampling for domain partition
    var pbd = new PlackettBurmanDesign(numericOptionsFirstSampling)
    pbd.initSeeds()
    if (dimToConsider == 2)
      pbd.setSeed(3, 9)
    if (dimToConsider == 3)
      pbd.setSeed(5, 25)
    var numericSamplings = pbd.getPoints()

    var configurationsPreFiltered : scala.collection.mutable.Set[Configuration] = (new Configuration).generateConfigurations(binaryConfigs, numericSamplings)
    println("configsBeforeFiltering " + configurationsPreFiltered.size)
    //       
    var configurationsFiltered : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach(x =>
      {
        if (dimToConsider == 2 && ccVSvc.equals("cc"))
          problemDefinition2D_ConstCoeff(x.partialBaseConfig)
        if (dimToConsider == 2 && ccVSvc.equals("vc"))
          problemDefinition2D_VarCoeff(x.partialBaseConfig)
        if (dimToConsider == 3 && ccVSvc.equals("cc"))
          problemDefinition3D_ConstCoeff(x.partialBaseConfig)
        if (dimToConsider == 3 && ccVSvc.equals("vc"))
          problemDefinition3D_VarCoeff(x.partialBaseConfig)

        configurationsFiltered.add(x)
      })

    println("configsAfterFiltering  " + configurationsFiltered.size)

    pbd = new PlackettBurmanDesign(numericOptionsSecondSampling)
    pbd.initSeeds()
    if (dimToConsider == 2)
      pbd.setSeed(5, 25)
    if (dimToConsider == 3)
      pbd.setSeed(5, 25)
    numericSamplings = pbd.getPoints()

    var configurationsWithSecondSampling : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          configurationsWithSecondSampling.add(configCopy)

        })

      }
    }

    pbd = new PlackettBurmanDesign(numericOptionsThirdSampling)
    pbd.initSeeds()
    if (dimToConsider == 2)
      pbd.setSeed(5, 125)
    if (dimToConsider == 3)
      pbd.setSeed(5, 125)

    numericSamplings = pbd.getPoints()
    println("second sampling combinations " + numericSamplings.size)

    var configurationsWiththirdSampling : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsWithSecondSampling.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if (this.derivedParameters2(configCopy)) {
            configurationsWiththirdSampling.add(configCopy)
          }
        })

      }
    }
    println("configsCombined  " + configurationsWiththirdSampling.size)

    ////////////////////////////////////// end feature wise sampling in combination with pdb

    var pwSampling = new PWHeuristic(featuresBinary)
    binaryConfigs = pwSampling.getPoints()
    println("----------binary PW " + binaryConfigs.size)

    // numeric Sampling for domain partition
    pbd = new PlackettBurmanDesign(numericOptionsFirstSampling)
    pbd.initSeeds()
    if (dimToConsider == 2)
      pbd.setSeed(3, 9)
    if (dimToConsider == 3)
      pbd.setSeed(3, 9)
    numericSamplings = pbd.getPoints()

    configurationsPreFiltered = (new Configuration).generateConfigurations(binaryConfigs, numericSamplings)
    println("configsBeforeFiltering " + configurationsPreFiltered.size)
    //       
    configurationsFiltered = scala.collection.mutable.Set()
    configurationsPreFiltered.foreach(x =>
      {
        if (dimToConsider == 2 && ccVSvc.equals("cc"))
          problemDefinition2D_ConstCoeff(x.partialBaseConfig)
        if (dimToConsider == 2 && ccVSvc.equals("vc"))
          problemDefinition2D_VarCoeff(x.partialBaseConfig)
        if (dimToConsider == 3 && ccVSvc.equals("cc"))
          problemDefinition3D_ConstCoeff(x.partialBaseConfig)
        if (dimToConsider == 3 && ccVSvc.equals("vc"))
          problemDefinition3D_VarCoeff(x.partialBaseConfig)

        configurationsFiltered.add(x)
      })

    println("configsAfterFiltering  " + configurationsFiltered.size)

    pbd = new PlackettBurmanDesign(numericOptionsSecondSampling)
    pbd.initSeeds()
    if (dimToConsider == 2)
      pbd.setSeed(5, 25)
    if (dimToConsider == 3)
      pbd.setSeed(3, 9)
    numericSamplings = pbd.getPoints()
    println("second sampling combinations " + numericSamplings.size)

    var configurationsWithSecondSamplingPW : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsFiltered.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          configurationsWithSecondSamplingPW.add(configCopy)

        })

      }
    }

    pbd = new PlackettBurmanDesign(numericOptionsThirdSampling)
    pbd.initSeeds()
    if (dimToConsider == 2)
      pbd.setSeed(5, 125)
    if (dimToConsider == 3)
      pbd.setSeed(3, 9)
    numericSamplings = pbd.getPoints()
    println("second sampling combinations " + numericSamplings.size)

    var configurationsWithThirdSamplingPW : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    configurationsWithSecondSamplingPW.foreach { x =>
      {
        numericSamplings.foreach(y => {
          var configCopy = x.copy();
          configCopy.addNumericOptions(y)
          if (this.derivedParameters2(configCopy)) {
            configurationsWithThirdSamplingPW.add(configCopy)
          }
        })

      }
    }

    println("configsCombined PW  " + configurationsWithThirdSamplingPW.size)

    configurationsWiththirdSampling ++= configurationsWithThirdSamplingPW

    println("configsCombined FW & PW " + configurationsWiththirdSampling.size)

    /////////////////////////////////////////////////////// RANDOM ////////////

    //    fwSampling = new FWHeuristic(featuresBinary)
    //    binaryConfigs = fwSampling.getPoints()
    //    println("----------binary FW(used as Random) " + binaryConfigs.size)
    //
    //    // numeric Sampling for domain partition
    //    var rDesign = new RandomDesign(numericOptionsFirstSampling)
    //    rDesign.numberOfPoints = 6
    //    numericSamplings = rDesign.getPoints()
    //
    //    configurationsPreFiltered = (new Configuration).generateConfigurations(binaryConfigs, numericSamplings)
    //    println("configsBeforeFiltering " + configurationsPreFiltered.size)
    //    //       
    //    configurationsFiltered = scala.collection.mutable.Set()
    //    configurationsPreFiltered.foreach(x =>
    //      {
    //        if (dimToConsider == 2 && ccVSvc.equals("cc"))
    //          problemDefinition2D_ConstCoeff(x.partialBaseConfig)
    //        if (dimToConsider == 2 && ccVSvc.equals("vc"))
    //          problemDefinition2D_VarCoeff(x.partialBaseConfig)
    //        if (dimToConsider == 3 && ccVSvc.equals("cc"))
    //          problemDefinition3D_ConstCoeff(x.partialBaseConfig)
    //        if (dimToConsider == 3 && ccVSvc.equals("vc"))
    //          problemDefinition3D_VarCoeff(x.partialBaseConfig)
    //
    //        configurationsFiltered.add(x)
    //
    //      })
    //
    //    println("configsAfterFiltering  " + configurationsFiltered.size)
    //
    //    rDesign = new RandomDesign(numericOptionsSecondSampling)
    //    rDesign.numberOfPoints = 600
    //    numericSamplings = rDesign.getPoints()
    //    println("second sampling combinations " + numericSamplings.size)
    //
    //    var configurationsWithSecondSamplingRandom : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
    //    configurationsFiltered.foreach { x =>
    //      {
    //        numericSamplings.foreach(y => {
    //          var configCopy = x.copy();
    //          configCopy.addNumericOptions(y)
    //          if (this.derivedParameters2(configCopy))
    //            configurationsWithSecondSamplingRandom.add(configCopy)
    //        })
    //
    //      }
    //    }
    //
    //    println("random configurations: " + configurationsWithSecondSamplingRandom.size)

    // add derived features for debug perpuses 
    var featuresInOutput : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    featuresInOutput ++= (featuresToConsider)

    featuresInOutput.add("domain_fragmentLength_x")
    featuresInOutput.add("domain_fragmentLength_y")
    if (dimToConsider == 3)
      featuresInOutput.add("domain_fragmentLength_z")

    featuresInOutput.add("domain_rect_numFragsPerBlock_x")
    featuresInOutput.add("domain_rect_numFragsPerBlock_y")
    if (dimToConsider == 3)
      featuresInOutput.add("domain_rect_numFragsPerBlock_z")

    featuresInOutput.add("l3tmp_omega")
    featuresInOutput.add("domain_rect_numBlocks_x")
    featuresInOutput.add("domain_rect_numBlocks_y")
    if (dimToConsider == 3)
      featuresInOutput.add("domain_rect_numBlocks_z")

    featuresInOutput.add("mpi_numThreads")

    featuresInOutput.add("aro_x")
    featuresInOutput.add("aro_y")
    if (dimToConsider == 3)
      featuresInOutput.add("aro_z")
    featuresInOutput.add("numUnitFragsPD")

    featuresInOutput.add("domain_x")
    featuresInOutput.add("domain_y")
    if (dimToConsider == 3)
      featuresInOutput.add("domain_z")

    featuresInOutput.add("maxLevel")

    featuresInOutput.add("omp_numThreads")

    featuresInOutput.add("hw_numThreadsPerNode")
    featuresInOutput.add("omp_parallelizeLoopOverFragments")

    featuresInOutput.add("l3tmp_targetResReduction")
    featuresInOutput.add("l3tmp_maxNumCGSSteps")

    giveConfigsAName(configurationsWiththirdSampling, "")
    //    giveConfigsAName(configurationsWithSecondSamplingRandom, "_random")

    IO.printAllResultsToOneFile(configurationsWiththirdSampling.toArray, "E:/newDomainPartition.csv", featuresInOutput.toArray)
    //    IO.printAllResultsToOneFile(configurationsWithSecondSamplingRandom.toArray, "E:/newDomainPartition_random.csv", featuresInOutput.toArray)

    var blackList : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
    blackList.add("num_points_per_dim")
    blackList.add("numOMP_x")
    blackList.add("firstDim")
    blackList.add("numNodes")
    blackList.add("numUnitFragsPD")
    blackList.add("numOMP_x")
    if (dimToConsider == 3) {
      blackList.add("numOMP_y")
      blackList.add("secDim")
    }
    blackList.add("aro_x")
    blackList.add("aro_y")
    if (dimToConsider == 3)
      blackList.add("aro_z")
    blackList.add("numUnitFragsPD")

    blackList.add("domain_x")
    blackList.add("domain_y")
    if (dimToConsider == 3)
      blackList.add("domain_z")

    // print random configs
    //    writeConfigurations(configurationsWithSecondSamplingRandom, blackList, "_random")

    //     print all 
    writeConfigurations(configurationsWiththirdSampling, blackList, "")

    return true
  }

  def giveConfigsAName(configs : scala.collection.mutable.Set[Configuration], suffix : String) = {
    var configsByMpiOmp : scala.collection.mutable.Map[String, scala.collection.mutable.Set[Configuration]] = scala.collection.mutable.Map()
    configs.foreach(x => {
      var mpiOmpRanksKey = x.partialBaseConfig("mpi_numThreads").asInstanceOf[Double].toInt + "_" + x.partialBaseConfig("omp_numThreads").asInstanceOf[Double].toInt + "_" + x.numericalFeatureValues(FeatureModel.get("ranksPerNode")).toInt
      if (configsByMpiOmp.contains(mpiOmpRanksKey)) {
        configsByMpiOmp(mpiOmpRanksKey).add(x)
        x.measurementName = mpiOmpRanksKey + "_" + configsByMpiOmp(mpiOmpRanksKey).size
      } else {
        var newList : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
        newList.add(x)
        x.measurementName = mpiOmpRanksKey + "_" + "1"
        configsByMpiOmp.put(mpiOmpRanksKey, newList)
      }
    })

  }

  def writeConfigurations(configurations : scala.collection.mutable.Set[Configuration], blackList : scala.collection.mutable.Set[String], suffix : String) = {
    var configsByMpiOmp : scala.collection.mutable.Map[String, scala.collection.mutable.Set[Configuration]] = scala.collection.mutable.Map()
    configurations.foreach(x => {
      var mpiOmpRanksKey = x.partialBaseConfig("mpi_numThreads").asInstanceOf[Double].toInt + "_" + x.partialBaseConfig("omp_numThreads").asInstanceOf[Double].toInt + "_" + x.numericalFeatureValues(FeatureModel.get("ranksPerNode")).toInt
      if (configsByMpiOmp.contains(mpiOmpRanksKey)) {
        configsByMpiOmp(mpiOmpRanksKey).add(x)
      } else {
        var newList : scala.collection.mutable.Set[Configuration] = scala.collection.mutable.Set()
        newList.add(x)
        configsByMpiOmp.put(mpiOmpRanksKey, newList)
      }
    })
    println("different MPI OMP threads " + configsByMpiOmp.size)
    var newFilelocation = generationTargetDir + "script_all" + suffix + ".sh"
    var newFilelocationMake = generationTargetDir + "/makeScript" + suffix + ".sh"
    val writer = new PrintWriter(new File(newFilelocation))
    var makeWriter = new PrintWriter(new File(newFilelocationMake))
    configsByMpiOmp.foreach(x => {
      var mpi_numThreads = x._2.head.partialBaseConfig("mpi_numThreads").asInstanceOf[Double].toInt
      var omp_numThreads = x._2.head.partialBaseConfig("omp_numThreads").asInstanceOf[Double].toInt
      var ranks = x._2.head.numericalFeatureValues(FeatureModel.get("ranksPerNode")).asInstanceOf[Double].toInt

      println("NumConfigs -----------" + x._2.size)
      generateJobScript(x._1, x._2, suffix)
      var index = 1

      var scriptBuilder = new StringBuilder()

      x._2.foreach(y => {
        var configKey = y.measurementName
        configToKnowledgeFile(y, configKey, blackList, suffix)
        index += 1
        generateShFileChimaira(configKey, suffix)
        writer.append("sbatch -A spl -p chimaira -n 1 -c 1 -t 20 -o " + configKey + suffix + ".out /home/grebhahn/ScalaCodegenSISC/config/script_" + configKey + suffix + ".sh\n")

        makeWriter.append("cd config_" + configKey + suffix + "/\n")
        makeWriter.append("make -j\n")
        makeWriter.append("cd ..\n")

      })

    })

    makeWriter.flush()
    makeWriter.close()

    writer.flush()
    writer.close()
  }

  def generateShFileChimaira(configKey : String, suffix : String) {
    var newFilelocation = generationTargetDir + "script_" + configKey + suffix + ".sh"
    val writer = new PrintWriter(new File(newFilelocation))
    writer.append("#!/bin/sh\n")
    writer.append("cd /home/grebhahn/ScalaCodegenSISC/knowledgeFiles\n")
    writer.append("mkdir ../config/config_" + configKey + suffix + "/\n")
    writer.append("srun exaGen Main knowledgeFile_" + configKey + suffix + ".knowledge ../config/config_" + configKey + suffix + "/\n")
    writer.flush()
    writer.close()
  }

  def generateJobScript(mpiOmpRanksKey : String, configs : scala.collection.mutable.Set[Configuration], suffix : String) = {
    Settings.user = "alex"
    Knowledge.targetCompiler = "IBMBG"
    var mpi_numThreads = configs.head.partialBaseConfig("mpi_numThreads").asInstanceOf[Double].toInt
    var omp_numThreads = configs.head.partialBaseConfig("omp_numThreads").asInstanceOf[Double].toInt
    var ranksPerNode = configs.head.numericalFeatureValues(FeatureModel.get("ranksPerNode")).toInt
    var number = 0

    var numNodes = (mpi_numThreads * omp_numThreads) / 64

    if (numNodes <= 512) {
      // splitting in multiple jobs ( because one the max timeout of one job is 30 minutes)       
      var sourcePathes : Array[String] = new Array(20)
      var config = 0
      var index = 0

      configs.foreach { x =>
        {

          sourcePathes(index) = ("config_" + mpiOmpRanksKey + "_" + (config + 1))
          config += 1
          index += 1
          if (index == 20) {
            JobScriptGenerator.write(mpi_numThreads, omp_numThreads, ranksPerNode, sourcePathes, number, suffix)
            index = 0;
            number += 1
            sourcePathes = new Array(20)
          }
        }
      }
      JobScriptGenerator.write(mpi_numThreads, omp_numThreads, ranksPerNode, sourcePathes, number, suffix)

    } else {
      var sourcePathes : Array[String] = new Array(configs.size)
      var index = 0
      configs.foreach { x =>
        {
          sourcePathes(index) = ("config_" + mpiOmpRanksKey + "_" + (index + 1))
          index += 1
        }
      }
      JobScriptGenerator.write(mpi_numThreads, omp_numThreads, ranksPerNode, sourcePathes, number, suffix)
    }
  }

  def configToKnowledgeFile(config : Configuration, index : String, blackList : scala.collection.mutable.Set[String], suffix : String) = {

    var newFilelocation = generationTargetDir + "knowledgeFile_" + index + suffix + ".knowledge";

    val writer = new PrintWriter(new File(newFilelocation))

    var configContent = config.getKnowledgeFileContent(blackList)

    configContent.foreach { x => writer.write(x) }
    var partialBase = partialBaseConfigInformation()
    writer.write(partialBase)

    writer.write("l3tmp_timerOuputFile = \"timings_" + index + suffix + ".csv\"\n")

    writer.flush()
    writer.close()

  }

  def featureToConsider2D() = {

    featuresToConsiderDimIndependent()

    featuresToConsider.add("poly_tileSize_x")

    //    featuresToConsider.add("domain_fragmentLength_x")
    //    featuresToConsider.add("domain_fragmentLength_y")
    //    featuresToConsider.add("domain_rect_numFragsPerBlock_x")
    //    featuresToConsider.add("domain_rect_numFragsPerBlock_y")

    featuresToConsider.add("numOMP_y")
    featuresToConsider.add("numNodes")
    featuresToConsider.add("ranksPerNode")

    featuresToConsider.add("firstDim")

    featuresToConsider.add("numOMP_x")

  }

  def featureToConsider3D() = {

    featuresToConsiderDimIndependent()

    // TODO Upper bound and stepsize
    featuresToConsider.add("poly_tileSize_x")
    featuresToConsider.add("poly_tileSize_y")

    //    featuresToConsider.add("domain_fragmentLength_x")
    //    featuresToConsider.add("domain_fragmentLength_y")
    //    featuresToConsider.add("domain_fragmentLength_z")
    //    featuresToConsider.add("domain_rect_numFragsPerBlock_x")
    //    featuresToConsider.add("domain_rect_numFragsPerBlock_y")
    //    featuresToConsider.add("domain_rect_numFragsPerBlock_z")

    featuresToConsider.add("numNodes")
    featuresToConsider.add("ranksPerNode")

    featuresToConsider.add("secDim")

    featuresToConsider.add("numOMP_x")
    featuresToConsider.add("numOMP_y")
    featuresToConsider.add("numOMP_z")

  }

  def featuresToConsiderDimIndependent() = {

    //    featuresToConsider.add("maxLevel")
    featuresToConsider.add("omp_parallelizeLoopOverDimensions")

    featuresToConsider.add("minLevel")

    featuresToConsider.add("opt_useAddressPrecalc")
    featuresToConsider.add("opt_vectorize")
    featuresToConsider.add("l3tmp_smoother")
    featuresToConsider.add("l3tmp_numRecCycleCalls")
    featuresToConsider.add("l3tmp_numPre")
    featuresToConsider.add("l3tmp_numPost")

    featuresToConsider.add("l3tmp_useSlotsForJac")

    featuresToConsider.add("poly_optLevel_fine")

    featuresToConsider.add("opt_unroll")
    featuresToConsider.add("opt_unroll_interleave")
    featuresToConsider.add("comm_useFragmentLoopsForEachOp")

    featuresToConsider.add("mpi_useCustomDatatypes")
  }

  def partialBaseConfigInformation() : String = {
    var sb : StringBuilder = new StringBuilder()

    sb ++= "targetCompiler = \"IBMBG\" \n"
    sb ++= "targetCompilerVersion = 12\n"
    sb ++= "targetCompilerVersionMinor = 1\n"

    sb ++= "useDblPrecision = true\n"

    sb ++= "simd_instructionSet = \"QPX\"\n"
    sb ++= "simd_avoidUnaligned = true\n"
    sb ++= "timer_type = \"MPI_TIME\"\n"

    sb ++= "domain_readFromFile = false\n"
    sb ++= "domain_onlyRectangular = true\n"
    sb ++= "domain_rect_generate = true\n"

    sb ++= "ir_genSepLayoutsPerField = true\n"

    sb ++= "comm_sepDataByFragment = true\n"
    sb ++= "comm_sepDataByDomain = false\n"
    sb ++= "comm_sepDataByField = false\n"
    sb ++= "comm_sepDataByLevel = false\n"
    sb ++= "comm_sepDataByNeighbor = true\n"
    sb ++= "comm_useFragmentArrays = true\n"
    sb ++= "comm_useDomainArrays = true\n"
    sb ++= "comm_useFieldArrays = false\n"
    sb ++= "comm_useLevelArrays = false\n"
    sb ++= "comm_useNeighborArrays = true\n"

    sb ++= "data_initAllFieldsWithZero = true\n"
    sb ++= "data_useFieldNamesAsIdx = false\n"

    sb ++= "mpi_defaultCommunicator = \"MPI_COMM_WORLD\"\n"
    sb ++= "mpi_enabled = true\n"
    sb ++= "mpi_useLoopsWherePossible = true\n"

    sb ++= "omp_useCollapse = false\n"

    sb ++= "poly_scheduleAlgorithm = \"isl\"\n"
    sb ++= "poly_optimizeDeps = \"raw\"\n"
    sb ++= "poly_filterDeps = true\n"
    sb ++= "poly_simplifyDeps = true\n"
    sb ++= "poly_fusionStrategy = \"max\"\n"
    sb ++= "poly_maximizeBandDepth = false\n"
    sb ++= "poly_maxConstantTerm = -1\n"
    sb ++= "poly_maxCoefficient = -1\n"

    sb ++= "l3tmp_generateL4 = true\n"

    sb ++= "l3tmp_cgs = \"CG\"\n"

    sb ++= "l3tmp_useConditionsForRBGS = true\n"
    //sb ++= "l3tmp_useSlotVariables = true\n"
    sb ++= "l3tmp_genHDepStencils = true\n"

    sb ++= "l3tmp_genTimersPerFunction = true\n"
    sb ++= "l3tmp_genTimersPerLevel = false\n"
    sb ++= "l3tmp_genTimersForComm = false\n"
    sb ++= "l3tmp_genCommTimersPerLevel = false\n"

    sb ++= "l3tmp_printAllTimers = false\n"
    sb ++= "l3tmp_printTimersToFile = true\n"

    sb ++= "l3tmp_genNonZeroRhs = true\n"

    sb ++= "l3tmp_genExtFields = false\n"
    sb ++= "l3tmp_genGlobalOmega = false\n"
    sb ++= "l3tmp_genSetableStencil = false\n"
    sb ++= "l3tmp_genVectorFields = false\n"
    sb ++= "l3tmp_numVecDims = 1\n"
    sb ++= "l3tmp_genEmbeddedDomain = false\n"
    sb ++= "l3tmp_useMaxNorm = false\n"
    sb ++= "l3tmp_genCellBasedDiscr = false\n"

    sb ++= "l3tmp_printFieldAtEnd = false\n"
    sb ++= "l3tmp_initSolWithRand = false\n"
    sb ++= "l3tmp_genForAutoTests = true\n"
    sb ++= "l3tmp_printError = false\n"
    sb ++= "l3tmp_useMaxNormForError = true\n"

    sb ++= "l3tmp_sisc = true\n"
    sb ++= "l3tmp_kelvin = false\n"

    sb ++= "l3tmp_genStencilStencilConv = false\n"
    sb ++= "l3tmp_genAsyncCommunication = false\n"
    sb ++= "l3tmp_genFragLoops = false\n"

    sb ++= "experimental_useLevelIndepFcts = false\n"
    sb ++= "experimental_Neumann = false\n"
    sb ++= "experimental_timerEnableCallStacks = false\n"

    sb ++= "data_alignTmpBufferPointers = false\n"
    return sb.toString()
  }

  def problemDefinition2D_ConstCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 2)
    config.put("l3tmp_genStencilFields", false)
    config.put("l3tmp_exactSolution", "\"Kappa\"\n")
    config.put("num_points_per_dim", 32768) // 32*1024
  }

  def problemDefinition2D_VarCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 2)
    config.put("l3tmp_genStencilFields", true)
    config.put("l3tmp_exactSolution", "\"Kappa_VC\"\n")
    config.put("num_points_per_dim", 32768) // 32*1024
  }

  def problemDefinition3D_ConstCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 3)
    config.put("l3tmp_genStencilFields", false)
    config.put("l3tmp_exactSolution", "\"Kappa\"\n")
    config.put("num_points_per_dim", 1024)
  }

  def problemDefinition3D_VarCoeff(config : scala.collection.mutable.Map[String, Any]) = {
    config.put("dimensionality", 3)
    config.put("l3tmp_genStencilFields", true)
    config.put("l3tmp_exactSolution", "\"Kappa_VC\"\n")
    config.put("num_points_per_dim", 1024)
  }

  def isValidConfigDomainPartition(config : Configuration) : Boolean = {
    var num_unit_frags_x : Long = 1
    var num_unit_frags_y : Long = 1
    var num_unit_frags_z : Long = 1

    var frag_volume = 1.0

    //config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)

    if (config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int] == 3) {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      num_unit_frags_z = config.partialBaseConfig.apply("domain_rect_numBlocks_z").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z") * config.getNumericFeatureValue("domain_fragmentLength_z")).toLong
      frag_volume =
        config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y") * config.getNumericFeatureValue("domain_fragmentLength_z")
    } else {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      frag_volume =
        config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y")
    }

    if (!(Util.getNumNodes(config) > 8))
      return false

    var mem_per_node = (8.0 * 4.0 * 4.0 / 3.0 * Math.pow(config.partialBaseConfig.apply("num_points_per_dim").asInstanceOf[Int], config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int])) / Util.getNumNodes(config)
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if (!(mem_per_node <= memory))
      return false

    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3) {
      if (!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
        return false

    } else {
      if (!(num_unit_frags_x == num_unit_frags_y))
        return false
    }

    if (config.partialBaseConfig.apply("domain_numBlocks").asInstanceOf[Int] < 8)
      return false

    if (!(frag_volume <= 64.0 && config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] <= 64))
      return false

    if (!(frag_volume == 1.0 || config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] == 1.0))
      return false

    return true;
  }

  def isValidConfigDomainPartition2(config : Configuration) : Boolean = {
    var num_unit_frags_x : Long = 1
    var num_unit_frags_y : Long = 1
    var num_unit_frags_z : Long = 1

    var frag_volume = 1.0

    //config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)

    if (config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int] == 3) {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      num_unit_frags_z = config.partialBaseConfig.apply("domain_rect_numBlocks_z").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_z") * config.getNumericFeatureValue("domain_fragmentLength_z")).toLong
      frag_volume =
        config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y") * config.getNumericFeatureValue("domain_fragmentLength_z")
    } else {
      num_unit_frags_x = config.partialBaseConfig.apply("domain_rect_numBlocks_x").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_x") * config.getNumericFeatureValue("domain_fragmentLength_x")).toLong
      num_unit_frags_y = config.partialBaseConfig.apply("domain_rect_numBlocks_y").asInstanceOf[Int].toLong * (config.getNumericFeatureValue("domain_rect_numFragsPerBlock_y") * config.getNumericFeatureValue("domain_fragmentLength_y")).toLong
      frag_volume =
        config.getNumericFeatureValue("domain_fragmentLength_x") * config.getNumericFeatureValue("domain_fragmentLength_y")
    }

    if (!(Util.getNumNodes(config) > 8))
      return false

    var mem_per_node = (8.0 * 4.0 * 4.0 / 3.0 * Math.pow(config.partialBaseConfig.apply("num_points_per_dim").asInstanceOf[Int], config.partialBaseConfig.apply("dimensionality").asInstanceOf[Int])) / Util.getNumNodes(config)
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if (!(mem_per_node <= memory))
      return false

    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 3) {
      if (!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
        return false

    } else {
      if (!(num_unit_frags_x == num_unit_frags_y))
        return false
    }

    if (config.partialBaseConfig.apply("domain_numBlocks").asInstanceOf[Int] < 8)
      return false

    if (!(frag_volume <= 64.0 && config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] <= 64))
      return false

    if (!(frag_volume == 1.0 || config.partialBaseConfig.apply("domain_numFragmentsPerBlock").asInstanceOf[Int] == 1.0))
      return false

    return true;
  }

  def isValidConfigDomainPartition(config : scala.collection.mutable.Map[Feature, Double], problemDefinition : scala.collection.mutable.Map[String, Any]) : Boolean = {

    return isValidConfigDomainPartition2(config, problemDefinition)

    //    // calculation of dependent features 
    //    var domain_rect_numBlocks_x = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
    //      config(FeatureModel.get("domain_fragmentLength_x")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
    //      / config(FeatureModel.get("domain_rect_numFragsPerBlock_x")))
    //
    //    var domain_rect_numBlocks_y = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
    //      config(FeatureModel.get("domain_fragmentLength_y")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
    //      / config(FeatureModel.get("domain_rect_numFragsPerBlock_y")))
    //
    //    var domain_rect_numBlocks_z : Double = 1.0
    //
    //    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
    //      domain_rect_numBlocks_z = ((problemDefinition("num_points_per_dim").asInstanceOf[Int] / (
    //        config(FeatureModel.get("domain_fragmentLength_z")) * Math.pow(2, config(FeatureModel.get("maxLevel")))))
    //        / config(FeatureModel.get("domain_rect_numFragsPerBlock_z")))
    //    }
    //
    //    var num_frags_per_block_total : Double = 1.0
    //    var frag_volume = 1.0
    //    var num_blocks_total = 1.0
    //    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
    //      num_frags_per_block_total =
    //        config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_z"))
    //      frag_volume =
    //        config(FeatureModel.get("domain_fragmentLength_x")) * config(FeatureModel.get("domain_fragmentLength_y")) * config(FeatureModel.get("domain_fragmentLength_z"))
    //      num_blocks_total =
    //        domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z
    //    } else {
    //      num_frags_per_block_total =
    //        config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_rect_numFragsPerBlock_y"))
    //      frag_volume =
    //        config(FeatureModel.get("domain_fragmentLength_x")) * config(FeatureModel.get("domain_fragmentLength_y"))
    //      num_blocks_total =
    //        domain_rect_numBlocks_x * domain_rect_numBlocks_y
    //    }
    //
    //    var domain_numBlocks = num_blocks_total
    //    var domain_numFragmentsPerBlock = num_frags_per_block_total
    //    var mpi_numThreads = num_blocks_total
    //
    //    var omp_enabled = false
    //    var omp_numThreads = 1.0
    //    var omp_parallelizeLoopOverFragments = false
    //    var omp_parallelizeLoopOverDimensions = false
    //
    //    if (num_frags_per_block_total == 1.0 && frag_volume == 1.0) {
    //      omp_enabled = false
    //      omp_numThreads = 1.0
    //      omp_parallelizeLoopOverFragments = false
    //      omp_parallelizeLoopOverDimensions = false
    //    } else if (num_frags_per_block_total > frag_volume) {
    //      omp_enabled = true
    //      omp_numThreads = num_frags_per_block_total
    //      omp_parallelizeLoopOverFragments = true
    //      omp_parallelizeLoopOverDimensions = false
    //    } else {
    //      omp_enabled = true
    //      omp_numThreads = frag_volume
    //      omp_parallelizeLoopOverFragments = false
    //      omp_parallelizeLoopOverDimensions = true
    //    }
    //
    //    var num_unit_frags_x : Long = 1
    //    var num_unit_frags_y : Long = 1
    //    var num_unit_frags_z : Long = 1
    //
    //    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
    //      num_unit_frags_x = domain_rect_numBlocks_x.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x"))).toLong
    //      num_unit_frags_y = domain_rect_numBlocks_y.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y"))).toLong
    //      num_unit_frags_z = domain_rect_numBlocks_z.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_z")) * config(FeatureModel.get("domain_fragmentLength_z"))).toLong
    //    } else {
    //      num_unit_frags_x = domain_rect_numBlocks_x.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x"))).toLong
    //      num_unit_frags_y = domain_rect_numBlocks_y.toLong * (config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y"))).toLong
    //    }
    //
    //    var numNodes : Double = Util.getNumNodes(mpi_numThreads.toInt, omp_numThreads.toInt)
    //
    //    if (!(numNodes > 8.0))
    //      // println("Not enough nodes in use")
    //      return false
    //
    //    // new Constraint
    //    if (dimToConsider == 3)
    //      if (numNodes > 64.0)
    //        return false
    //
    //    var mem_per_node = (8.0 * 4.0 * 4.0 / (3.0 * Math.pow(problemDefinition("num_points_per_dim").asInstanceOf[Int], problemDefinition("dimensionality").asInstanceOf[Int]))) / numNodes
    //    var memory : Double = 12L * 1024L * 1024L * 1024L
    //    if (!(mem_per_node <= memory))
    //      //println("Memory requirements for each node are too high")
    //      return false
    //
    //    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
    //      if (!(num_unit_frags_x == num_unit_frags_y) || !(num_unit_frags_y == num_unit_frags_z))
    //        return false
    //
    //    } else {
    //      if (!(num_unit_frags_x == num_unit_frags_y))
    //        //println("Not square")
    //        return false
    //    }
    //
    //    if (!(domain_numBlocks >= 8))
    //      //println("Not enough blocks to distribute :%s" % num_blocks_total)
    //      return false
    //
    //    if (!(frag_volume <= 64.0 && domain_numFragmentsPerBlock <= 64))
    //      //println("Too many omp threads :%s" % self.get_value("omp_numThreads", 1))
    //      return false
    //
    //    if (!(frag_volume == 1.0 || domain_numFragmentsPerBlock == 1))
    //      //print("Two different omp parallelization strategies chosen concurrently")
    //      return false
    //
    //    if (config(FeatureModel.get("minLevel")).toInt >= config(FeatureModel.get("maxLevel")).toInt)
    //      return false
    //
    //    var domain_x = domain_rect_numBlocks_x * config(FeatureModel.get("domain_rect_numFragsPerBlock_x")) * config(FeatureModel.get("domain_fragmentLength_x")) * Math.pow(2, config(FeatureModel.get("minLevel")))
    //    if (domain_x > 64.0)
    //      return false;
    //
    //    var domain_y = domain_rect_numBlocks_y * config(FeatureModel.get("domain_rect_numFragsPerBlock_y")) * config(FeatureModel.get("domain_fragmentLength_y")) * Math.pow(2, config(FeatureModel.get("minLevel")))
    //    if (domain_y > 64.0)
    //      return false;
    //
    //    if (problemDefinition("dimensionality").asInstanceOf[Int] == 3) {
    //      var domain_z = domain_rect_numBlocks_z * config(FeatureModel.get("domain_rect_numFragsPerBlock_z")) * config(FeatureModel.get("domain_fragmentLength_z")) * Math.pow(2, config(FeatureModel.get("minLevel")))
    //      if (domain_z > 64.0)
    //        return false;
    //    }
    //
    //    return true;
  }

  def isValidConfigDomainPartition2(config : scala.collection.mutable.Map[Feature, Double], problemDefinition : scala.collection.mutable.Map[String, Any]) : Boolean = {

    var aspectRatioOffset : Double = 0

    val nodesTimesRacks : Double = config(FeatureModel.get("numNodes")) * config(FeatureModel.get("ranksPerNode"))

    val dimBase : Double = Math.pow(2, dimToConsider)

    var curr : Double = dimBase

    while (curr < nodesTimesRacks)
      curr *= 2

    aspectRatioOffset = curr / nodesTimesRacks

    return true;
  }

  def derivedParameters(config : Configuration) = {
    derivedParameters2(config)

  }

  def log(x : Double, base : Double) : Double = {
    return (Math.log(x) / Math.log(base));
  }

  // creates the derived parameters and performs a validation check
  def derivedParameters2(config : Configuration) : Boolean = {
    var aspectRatioOffset : Double = 0

    val numNodes = config.numericalFeatureValues(FeatureModel.get("numNodes"))
    val ranksPerNode = config.numericalFeatureValues(FeatureModel.get("ranksPerNode"))

    val nodesTimesRanks : Double = config.numericalFeatureValues(FeatureModel.get("numNodes")) * config.numericalFeatureValues(FeatureModel.get("ranksPerNode"))

    val dimBase : Double = Math.pow(2, dimToConsider)

    var base = 1
    var curr : Double = Math.pow(dimBase, base)

    while (curr < nodesTimesRanks) {
      base = base + 1
      curr = Math.pow(dimBase, base)
    }
    aspectRatioOffset = curr / nodesTimesRanks

    var aro_x = 1.0
    var aro_y = 1.0
    var aro_z = 1.0

    val firstDim = config.xorFeatureValues(FeatureModel.get("firstDim"))
    var secDim = ""
    if (dimToConsider == 3)
      secDim = config.xorFeatureValues(FeatureModel.get("secDim"))

    if (aspectRatioOffset >= 2) {
      if (firstDim.equals("0"))
        aro_x *= 2.0
      else if (firstDim.equals("1"))
        aro_y *= 2.0
      else
        aro_z *= 2.0
    }

    if (aspectRatioOffset >= 4) {
      if (secDim.equals("0"))
        aro_x *= 2.0
      else if (secDim.equals("1"))
        aro_y *= 2.0
      else
        aro_z *= 2.0
    }

    var numUnitFragsPD = Math.pow(nodesTimesRanks * aspectRatioOffset, (1.0 / dimToConsider))

    var maxLevel = log(num_points_per_dim / numUnitFragsPD, 2)

    val numOMP_x = config.numericalFeatureValues(FeatureModel.get("numOMP_x"))
    val numOMP_y = config.numericalFeatureValues(FeatureModel.get("numOMP_y"))
    var numOMP_z = 1.0
    if (dimToConsider == 3)
      numOMP_z = config.numericalFeatureValues(FeatureModel.get("numOMP_z"))

    if (numUnitFragsPD < numOMP_x * aro_x)
      return false;

    if (numUnitFragsPD < numOMP_y * aro_y)
      return false;

    if (dimToConsider == 3)
      if (numUnitFragsPD < numOMP_z * aro_z)
        return false;

    var ompAll : Double = 0.0
    if (dimToConsider == 2) {
      ompAll = numOMP_x * numOMP_y
      if (ompAll > ranksPerNode)
        return false
    } else {
      ompAll = numOMP_x * numOMP_y * numOMP_z
      if (ompAll > ranksPerNode)
        return false
    }

    var ompParallelizeFrags = !config.boolFeatures(FeatureModel.get("omp_parallelizeLoopOverDimensions"))

    if (ompAll <= 1) {
      if (!ompParallelizeFrags) {
        return false
      }
      ompParallelizeFrags = false
    }

    var numFragsPerBlock_x = 1.0
    var numFragsPerBlock_y = 1.0
    var numFragsPerBlock_z = 1.0
    if (ompParallelizeFrags) {
      numFragsPerBlock_x = numOMP_x
      numFragsPerBlock_y = numOMP_y
      if (dimToConsider == 3) {
        numFragsPerBlock_z = numOMP_z
      }
    }

    var fragLength_x = 1.0
    var fragLength_y = 1.0
    var fragLength_z = 1.0
    if (config.boolFeatures(FeatureModel.get("omp_parallelizeLoopOverDimensions"))) {
      fragLength_x = numOMP_x * aro_x
      fragLength_y = numOMP_y * aro_y
      if (dimToConsider == 3)
        fragLength_z = numOMP_z * aro_z

    } else {
      fragLength_x = aro_x
      fragLength_y = aro_y
      if (dimToConsider == 3)
        fragLength_z = aro_z
    }

    val numBlocks_x = numUnitFragsPD / (numFragsPerBlock_x * fragLength_x)
    val numBlocks_y = numUnitFragsPD / (numFragsPerBlock_y * fragLength_y)
    var numBlocks_z : Double = 1.0
    if (dimToConsider == 3)
      numBlocks_z = numUnitFragsPD / (numFragsPerBlock_z * fragLength_z)

    val l3tmp_useSlotsForJac = config.boolFeatures(FeatureModel.allFeatures("l3tmp_useSlotsForJac"))
    val l3tmp_smoother = config.xorFeatureValues(FeatureModel.allFeatures("l3tmp_smoother")).asInstanceOf[String]

    if (config.partialBaseConfig("dimensionality").asInstanceOf[Int] == 2) {
      if (config.xorFeatureValues.contains(FeatureModel.allFeatures("l3tmp_smoother"))) {
        if (!l3tmp_smoother.equals("Jac")) {
          config.partialBaseConfig.put("l3tmp_omega", 1.16)
        } else {
          config.partialBaseConfig.put("l3tmp_omega", 0.79)
        }
      }
    } else {
      if (config.xorFeatureValues.contains(FeatureModel.allFeatures("l3tmp_smoother"))) {
        if (!l3tmp_smoother.equals("Jac")) {
          config.partialBaseConfig.put("l3tmp_omega", 1.19)
        } else {
          config.partialBaseConfig.put("l3tmp_omega", 0.85)
        }
      }

    }

    var mem_per_node = (8.0 * 4.0 * 4.0 / (3.0 * Math.pow(num_points_per_dim, dimToConsider))) / numNodes
    var memory : Double = 12L * 1024L * 1024L * 1024L
    if (!(mem_per_node <= memory))
      return false

    var domain_rect_numBlocks_x = ((num_points_per_dim / (
      fragLength_x * Math.pow(2, maxLevel)))
      / numFragsPerBlock_x)

    var domain_rect_numBlocks_y = ((num_points_per_dim / (
      fragLength_y * Math.pow(2, maxLevel)))
      / numFragsPerBlock_y)

    var domain_rect_numBlocks_z : Double = 1.0

    val numMPI = domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z

    if (dimToConsider == 3)
      domain_rect_numBlocks_z = ((num_points_per_dim / (
        fragLength_z * Math.pow(2, maxLevel)))
        / numFragsPerBlock_z)

    var num_frags_per_block_total : Double = 1.0
    var frag_volume = 1.0
    var num_blocks_total = 1.0
    if (dimToConsider == 3) {
      num_frags_per_block_total =
        numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z
      frag_volume =
        fragLength_x * fragLength_y * fragLength_z
      num_blocks_total =
        domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z
    } else {
      num_frags_per_block_total =
        numFragsPerBlock_x * numFragsPerBlock_y
      frag_volume =
        fragLength_x * fragLength_y
      num_blocks_total =
        domain_rect_numBlocks_x * domain_rect_numBlocks_y
    }

    var domain_numBlocks = num_blocks_total
    var domain_numFragmentsPerBlock = num_frags_per_block_total
    var mpi_numThreads = num_blocks_total

    var omp_enabled = false
    var omp_numThreads = 1.0

    if (ompAll > 1)
      omp_enabled = true

    omp_numThreads = ompAll

    val minLevel = config.numericalFeatureValues(FeatureModel.get("minLevel"))

    if (config.numericalFeatureValues(FeatureModel.get("minLevel")).toInt >= maxLevel) {
      return false
    }

    var domain_x = domain_rect_numBlocks_x * numFragsPerBlock_x * fragLength_x * Math.pow(2, minLevel)
    if (domain_x > 64.0) {
      return false;
    }
    var domain_y = domain_rect_numBlocks_y * numFragsPerBlock_y * fragLength_y * Math.pow(2, minLevel)
    if (domain_y > 64.0) {
      return false;
    }
    var domain_z = 1.0
    if (dimToConsider == 3) {
      domain_z = domain_rect_numBlocks_z * numFragsPerBlock_z * fragLength_z * Math.pow(2, minLevel)
      if (domain_z > 64.0)
        return false;
    }

    val l3tmp_numPre = config.getNumericFeatureValue("l3tmp_numPre").toInt
    val l3tmp_numPost = config.getNumericFeatureValue("l3tmp_numPost").toInt
    val poly_optLevel_fine = config.xorFeatureValues(FeatureModel.allFeatures("poly_optLevel_fine"))
    var poly_tileSize_y = 0
    if (dimToConsider == 3)
      poly_tileSize_y = config.getNumericFeatureValue("poly_tileSize_y").toInt
    val poly_tileSize_x = config.getNumericFeatureValue("poly_tileSize_x").toInt
    val opt_unroll_interleave = config.boolFeatures(FeatureModel.allFeatures("opt_unroll_interleave"))
    val opt_unroll = config.getNumericFeatureValue("opt_unroll").toInt

    if (l3tmp_numPre + l3tmp_numPost < 1) {
      return false
    }

    if (l3tmp_numPre + l3tmp_numPost > 12) {
      return false

    }

    // select l3tmp_useSlotsForJac only if Jac is the Smoother
    if (l3tmp_useSlotsForJac && !l3tmp_smoother.equals("Jac"))
      return false

    // un-splotted Jac
    if (l3tmp_smoother.equals("Jac") && !l3tmp_useSlotsForJac) {

      if ((l3tmp_numPre % 2 != 0) || (l3tmp_numPost % 2 != 0))
        return false
    }

    if (opt_unroll_interleave && opt_unroll == 1) //config.xorFeatureValues(FeatureModel.allFeatures("opt_unroll")).toInt == 1)
      return false

    if (!poly_optLevel_fine.equals("3") && poly_tileSize_x != FeatureModel.get("poly_tileSize_x").minValue)
      return false

    if (dimToConsider == 3) {
      if (!poly_optLevel_fine.equals("3") && poly_tileSize_y != FeatureModel.get("poly_tileSize_y").minValue)
        return false

    }

    var poly_numFinestLevels = 2

    if (poly_optLevel_fine.equals("3") && (Math.pow(2, maxLevel - poly_numFinestLevels) * fragLength_x) + 1 < poly_tileSize_x)
      return false;

    if (dimToConsider == 3) {
      if (poly_optLevel_fine.equals("3") && (Math.pow(2, maxLevel - poly_numFinestLevels) * fragLength_y) + 1 < poly_tileSize_y)
        return false;
    }

    config.partialBaseConfig.put("maxLevel", maxLevel)

    config.partialBaseConfig.put("omp_parallelizeLoopOverFragments", ompParallelizeFrags)

    config.partialBaseConfig.put("domain_fragmentLength_x", fragLength_x)
    config.partialBaseConfig.put("domain_fragmentLength_y", fragLength_y)
    if (dimToConsider == 3)
      config.partialBaseConfig.put("domain_fragmentLength_z", fragLength_z)

    config.partialBaseConfig.put("domain_rect_numFragsPerBlock_x", numFragsPerBlock_x)
    config.partialBaseConfig.put("domain_rect_numFragsPerBlock_y", numFragsPerBlock_y)
    if (dimToConsider == 3)
      config.partialBaseConfig.put("domain_rect_numFragsPerBlock_z", numFragsPerBlock_z)

    config.partialBaseConfig.put("domain_rect_numBlocks_x", domain_rect_numBlocks_x)
    config.partialBaseConfig.put("domain_rect_numBlocks_y", domain_rect_numBlocks_y)
    if (dimToConsider == 3)
      config.partialBaseConfig.put("domain_rect_numBlocks_z", domain_rect_numBlocks_z)

    config.partialBaseConfig.put("mpi_numThreads", mpi_numThreads)

    config.partialBaseConfig.put("aro_x", aro_x)
    config.partialBaseConfig.put("aro_y", aro_y)
    if (dimToConsider == 3)
      config.partialBaseConfig.put("aro_z", aro_z)

    config.partialBaseConfig.put("numUnitFragsPD", numUnitFragsPD)

    config.partialBaseConfig.put("domain_x", domain_x)
    config.partialBaseConfig.put("domain_y", domain_y)
    if (dimToConsider == 3)
      config.partialBaseConfig.put("domain_z", domain_z)

    config.partialBaseConfig.put("omp_numThreads", omp_numThreads)

    config.partialBaseConfig.put("hw_numThreadsPerNode", ranksPerNode)

    if (config.boolFeatures(FeatureModel.get("opt_vectorize")))
      config.partialBaseConfig.put("data_alignFieldPointers", true)

    config.partialBaseConfig.put("domain_numBlocks", domain_rect_numBlocks_x * domain_rect_numBlocks_y * domain_rect_numBlocks_z)
    config.partialBaseConfig.put("domain_numFragmentsPerBlock", numFragsPerBlock_x * numFragsPerBlock_y * numFragsPerBlock_z)

    config.partialBaseConfig.put("l3tmp_maxNumCGSSteps", "1024")
    config.partialBaseConfig.put("l3tmp_targetResReduction", "1.0E-5")

    return true
  }

  def getNumIterations(config : Configuration, lfaConfigs : scala.collection.mutable.Set[LFAConfig]) : Int = {
    lfaConfigs.foreach { x =>
      if (x.numPost == config.numericalFeatureValues(FeatureModel.allFeatures("l3tmp_numPost")))
        if (x.numPre == config.numericalFeatureValues(FeatureModel.allFeatures("l3tmp_numPre")))
          if (x.Smoother == config.xorFeatureValues(FeatureModel.allFeatures("l3tmp_smoother")).replace("\"", ""))
            return x.iterationsNeeded
    }
    return 0
  }

}