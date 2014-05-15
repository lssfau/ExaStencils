package test.isl

import isl.BasicSet
import isl.Conversions.convertLambdaToXCallback1
import isl.Set

object Bindings {
  def main(args : Array[String]) = {

    println("start...")

    val s = new Set("[n] -> { [i] : 0<=i<=100 and i<=n }")
    println(s)
    val lm = s.lexmax
    println(lm)

    lm.foreachBasicSet((bs : BasicSet) => println(bs.samplePoint))

    println("isl works!")
  }
}
