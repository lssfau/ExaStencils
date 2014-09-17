// A simple data class 
class MyArray[T : Manifest](xs: Int, ys: Int) { 
//   private var a = ofDim[Double](xs,ys) 
   private var a:Array[T] = new Array[T](xs*ys) 
   private var x:Int = xs 
   private var y:Int = ys 
   
   def sx() : Int = {return x} 
   def sy() : Int = {return y} 
   def set(v: T) : Unit = { 
     
     for ( i <- 0 to (sx() - 1)) 
       for ( j <- 0 to (sy() - 1)) 
         set(i,j, v) 
   }
   
   def get(i:Int, j:Int) : T = { 
     return a(i*sy()+j) 
   } 
   def set(i:Int, j:Int, value:T) : Unit = { 
     a(i*sy()+j) = value 
   } 
   def print() : Unit = { 
     
     for ( i <- 1 to (sx() - 2)) 
       for ( j <- 1 to (sy() - 2)) 
         println(get(i,j)) 
   } 
} 

// A simple Stencil class
class MyStencil(s: Int) { 
   var entries = Array[Double](s) 
   var size:Int = s 
  
   def set(e:Array[Double]) : Unit = { 
     entries = e 
   } 
   
   def diag() : Double = { 
     return entries(0) 
   } 
   
   def convolve(arr:MyArray[Double], i:Int, j:Int) : Double = { 
     // directions of stencil have to be defined dependent on type of fragments!
   return entries(0)*arr.get(i,j) + entries(4)*arr.get(i+1,j) + entries(3)*arr.get(i-1,j) + entries(2)*arr.get(i,j+1) + entries(1)*arr.get(i,j-1) 
   } 
} 

// helper functions
object helper{
def setRandom( arr:MyArray[Double]) : Unit = {
     
    var r = new scala.util.Random
    for ( i <- 1 to (arr.sx() - 2))
       for ( j <- 1 to (arr.sy() - 2))
         arr.set(i,j, r.nextDouble())
}
}
