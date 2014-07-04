package test.isl

import isl.Conversions.convertLambdaToVoidCallback1

object Bindings {
  def main(args : Array[String]) = {

    println("start...")

    val s = new isl.Set("[n] -> { [i] : 0<=i<=100 and i<=n }")
    println(s)
    val lm = s.lexmax
    println(lm)

    lm.foreachBasicSet((bs : isl.BasicSet) => println(bs.samplePoint))

    println("isl works!")
  }
}
