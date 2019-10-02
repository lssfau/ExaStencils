
package exastencils.performance.ir

import scala.collection.mutable
import scala.collection.mutable._

import exastencils.base.ir.IR_Datatype
import exastencils.config._
import exastencils.logger.Logger
import exastencils.performance.ir.IR_EvaluatePerformanceEstimates.EvaluateSubAST.computeRelativeStencilOffsets

/// IR_DetermineBlockingFactors
object IR_DetermineBlockingFactors {
  def apply(fieldAcesses : HashMap[String, IR_Datatype], fieldSize : Array[Long], stencilOffsets : HashMap[String, ListBuffer[Long]]) : Array[Long] = {

    //Multi thread:
    var cacheSize : Double = (Platform.hw_cacheSize * Platform.hw_usableCache) / stencilOffsets.size //Bereich fuer jedes Feld
    val numberOfThreadsUsed = Knowledge.omp_numThreads
    val numberOfCaches = Platform.hw_numCacheSharingThreads / Platform.hw_cpu_numCoresPerCPU
    if (numberOfThreadsUsed > numberOfCaches) {
      cacheSize = (cacheSize / numberOfThreadsUsed) * numberOfCaches
    }
    var factors = mutable.HashMap.empty[String, Array[Long]]
    //each field:
    stencilOffsets.keys.foreach(ident => {
      var numberOfSlices = 1
      var stencil = stencilOffsets(ident).sorted.reverse
      if (stencil.nonEmpty) {
        val relO : ListBuffer[Long] = ListBuffer()
        var maxSlice : Long = 0

        //1.Version, auf komplette groesse blocken
        var cacheRequired : Double = math.abs(stencil.head)
        if (stencil(stencil.length - 1) <= 0)
          cacheRequired += math.abs(stencil(stencil.length - 1))
        cacheRequired = cacheRequired * fieldAcesses(ident).typicalByteSize
        //if (cacheRequired == 0){
        //  cacheRequired = cacheSize
        //}
        var ny = cacheSize / cacheRequired
        if (ny >= 1) {
          ny = 1
        }
        //wenn das zu kleine Bloecke werden:
        if (ny > 0.1) {
          var numberOfBlocks : Long = (1 / ny).toLong
          if (1 % ny != 0) {
            numberOfBlocks += 1
          }
          if (fieldSize.length == 3)
            factors(ident) = Array(fieldSize(0), fieldSize(1) / numberOfBlocks, fieldSize(2))
          else
            factors(ident) = Array(fieldSize(0) / numberOfBlocks, fieldSize(1))
          Logger.warn(s"Stencil: ${ stencil.head }, ${ stencil(stencil.length - 1) }")
          Logger.warn(s"Meike: cache_required1 ${ cacheRequired }, numberOfBlocks = ${ numberOfBlocks }, cacheSize = ${ cacheSize }, ny = ${ ny }")
        }
        //ansonsten 2.Version
        else {
          //3D:
          if (fieldSize.length == 3) {
            // 3D slice in +y direction
            val yOffset = fieldSize(0) * fieldSize(1)
            var stencil_biggery = stencil.filter(_ > yOffset)
            if (stencil_biggery.nonEmpty) {
              stencil = stencil.filter(_ < yOffset)
              var i = 1
              do {
                var part = stencil_biggery.filter(_ < i * yOffset)
                if (part.nonEmpty) {
                  stencil_biggery = stencil_biggery.filter(_ > i * yOffset)
                  part = computeRelativeStencilOffsets(part)
                  relO += part.sum
                  numberOfSlices += 1

                }
                i += 1
              } while (stencil_biggery.nonEmpty)

            }
            else {
              relO += 0
            }
            //3D slice in -y direction
            var stencil_smallery = stencil.filter(_ < -yOffset)
            if (stencil_smallery.nonEmpty) {
              stencil = stencil.filter(_ > -yOffset)
              var i = 1
              do {
                var part = stencil_smallery.filter(_ < i * yOffset)
                if (part.nonEmpty) {
                  stencil_smallery = stencil_smallery.filter(_ > i * yOffset)
                  part = computeRelativeStencilOffsets(part)
                  relO += part.sum
                  numberOfSlices += 1

                }
                i += 1
              } while (stencil_smallery.nonEmpty)
            }
            else {
              relO += 0
            }
            maxSlice = relO.max
          }
          //2D
          if (stencil.nonEmpty) {
            val rel_stencil = computeRelativeStencilOffsets(stencil)
            if (rel_stencil.sum > maxSlice) {
              maxSlice = rel_stencil.sum
            }
          }

          cacheRequired = maxSlice * numberOfSlices * fieldAcesses(ident).typicalByteSize
          if (cacheRequired == 0) {
            cacheRequired = cacheSize
          }
          var ny = cacheSize / cacheRequired
          if (ny > 1) {
            ny = 1
          }
          var numberOfBlocks = (1 / ny).round
          if (1 % ny != 0) {
            numberOfBlocks += 1
          }
          Logger.warn(s"Meike: cache_required2 ${ cacheRequired }, numberOfBlocks = ${ numberOfBlocks }, cacheSize = ${ cacheSize }")
          //numberOfBlocks = 1 / numberOfBlocks
          if (fieldSize.length == 3) {
            factors(ident) = Array(fieldSize(0), fieldSize(1) / numberOfBlocks, fieldSize(2))

          }
          else {
            factors(ident) = Array(fieldSize(0) / numberOfBlocks, fieldSize(1))
          }
        }
      }
      else {
        factors(ident) = Array(fieldSize(0), fieldSize(1), fieldSize(2))
      }

    })
    //minimum finden
    factors(factors.minBy(value => value._2(1))._1)

  }
}