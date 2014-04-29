// simple 2D Poisson solver with finite differences in regular domain

class mg_test {

  // global problem parameters (2D unit square)
var nrows:Int = 1024
var ncols:Int = 1024

// global solver parameters
val nlevels:Int = 9
val iters:Int = 5

val omega:Double = 1.0 
// no. of presmoothing, postsmoothing and smoothing steps on coarsest level
val nprae:Int = 2
val npost:Int = 1
val ncoarse:Int = 5

   // allocate data 
   var SolA:Array[MyArray[Double]] = new Array[MyArray[Double]](nlevels)
   var RHSA:Array[MyArray[Double]] = new Array[MyArray[Double]](nlevels)
   
   // helper Array, can be optimized, stores current residual 
   var ResA:Array[MyArray[Double]] = new Array[MyArray[Double]](nlevels)

// storage for discrete operator
var laplacian:MyStencil = new MyStencil(5)

// first only smoother is interesting 
def GaussSeidel ( lev:Int ) =
{

   //println("GS")
    for ( i <- 1 to (SolA(lev).sx() - 2))
       for ( j <- 1 to (SolA(lev).sy() - 2))
//         Sol.a(i)(j) = (1.0-omega)* Sol.a(i)(j) + 0.25*omega*(RHS.a(i)(j) + (Sol.a(i+1)(j) + Sol.a(i-1)(j) + Sol.a(i)(j+1) + Sol.a(i)(j-1)))
         SolA(lev).set(i,j, (1.0-omega)* SolA(lev).get(i,j) + 1.0/laplacian.diag() *omega*(RHSA(lev).get(i,j) - laplacian.convolve(SolA(lev),i,j) + laplacian.diag()*SolA(lev).get(i,j)))
         
}

// Functions to compute residual

def Residual ( lev:Int ) : Unit =
{

	for ( i <- 1 to (RHSA(lev).sx() - 2))
	    for ( j <- 1 to (RHSA(lev).sy() - 2))
	      // res = RHS - Stencil*Sol 
	      // where res, RHS, Sol are vectors and Stencil is a matrix 
			ResA(lev).set( i,j , RHSA(lev).get(i,j) - laplacian.convolve(SolA(lev),i,j))
//			Res.a( i)(j ) = RHS.a(i)(j) + ((Sol.a(i+1)(j) + Sol.a(i-1)(j) + Sol.a(i)(j+1) + Sol.a(i)(j-1))  - 4.0*Sol.a(i)(j))

}

def L2_Residual ( lev:Int ) : Double =
{

	var res:Double = 0.0
	var rf:Double = 0

	for ( i <- 1 to (SolA(lev).sx() - 2))
	 for ( j <- 1 to (SolA(lev).sy() - 2))
		{
			//rf = RHS.a(i)(j) + ((Sol.a(i+1)(j) + Sol.a(i-1)(j) + Sol.a(i)(j+1) + Sol.a(i)(j-1))  - 4.0*Sol.a(i)(j))
			rf = RHSA(lev).get(i,j) - laplacian.convolve(SolA(lev),i,j)
			res += rf*rf
		}
	
	return scala.math.sqrt ( res )  / ( SolA(lev).sx() *SolA(lev).sy() )
}

// interpolation/restriction are required for more than one level

def interpolate_corr(uf:MyArray[Double], uc:MyArray[Double], l:Int) : Unit = {

  var xf:Int = 0
  var yf:Int = 0
  var v:Double = 0
  var v2:Double = 0
  var v4:Double = 0

  for ( i <- 1 to (uc.sx() - 2))
  {
    xf += 2;
    yf = 0;

  for ( j <- 1 to (uc.sy() - 2))
    {
      yf += 2;

        v=uc.get(i,j)
        v2=v*0.5
        v4=v*0.25

        uf.set(xf,yf,uf.get(xf,yf)+v)

        uf.set(xf-1,yf,uf.get(xf-1,yf)+v2)
        uf.set(xf+1,yf,uf.get(xf+1,yf)+v2)
        uf.set(xf,yf-1,uf.get(xf,yf-1)+v2)
        uf.set(xf,yf+1,uf.get(xf,yf+1)+v2)

        uf.set(xf-1,yf+1,uf.get(xf-1,yf+1)+v4)
        uf.set(xf+1,yf+1,uf.get(xf+1,yf+1)+v4)
        uf.set(xf-1,yf-1,uf.get(xf-1,yf-1)+v4)
        uf.set(xf+1,yf-1,uf.get(xf+1,yf-1)+v4)
    }
  }
}

def Restrict ( fine:MyArray[Double],coarse:MyArray[Double]) : Unit =
{
    for ( i <- 1 to (coarse.sx() - 2))
	{
	   var fi:Int = 2*i;
       for ( j <- 1 to (coarse.sy() - 2))
		{
			var fj:Int = 2*j;
			coarse.set( i,j , fine.get ( fi, fj ) +  0.5*(fine.get ( fi-1, fj ) + fine.get ( fi, fj-1 ) + fine.get ( fi+1, fj ) + fine.get ( fi, fj+1 )) +  0.25 *(fine.get ( fi-1, fj-1 ) 
			                                      + fine.get ( fi+1, fj-1 ) + fine.get ( fi-1, fj+1 ) + fine.get ( fi+1, fj+1 )))
		}
	}
}


// multigrid algorithm

def VCycle ( lev:Int  ) : Unit =
{

	if ( lev == nlevels-1 ) {
	  for ( i <- 1 to ncoarse)
			GaussSeidel ( lev)
	} else
	{
	  for ( i <- 1 to nprae)
			GaussSeidel( lev)

		Residual ( lev )
		Restrict ( ResA(lev),RHSA(lev+1))

		SolA(lev+1).set(0)
		VCycle ( lev+1 )

		interpolate_corr ( SolA(lev),SolA(lev+1),lev+1 )

	  for ( i <- 1 to npost)
			GaussSeidel ( lev )
	}
}

def Solver ( ) : Double =
{

	// solve
	var res_0:Double = L2_Residual ( 0 )
	var res:Double = res_0
	println("Starting residual: " + res )	
	var res_old:Double = 0	
	
    //ncoarse = scala.math.min(Sol(nlevels-1).sx()*Sol(nlevels-1).sy(),5000)

     for ( i <- 0 to (iters-1))
		{
			res_old = res;
			VCycle ( 0 )
			res = L2_Residual ( 0 )
			println("Cycle: " + i + " Residual: " + res + " residual reduction: " + res_0/res + " convergence factor: " + res/res_old)
			//Sol(lev).print()
		}

	return res
}


}

// Code that will be reused first

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

object helper {
def setRandom( arr:MyArray[Double]) : Unit = {
     
    var r = new scala.util.Random
    for ( i <- 1 to (arr.sx() - 2))
       for ( j <- 1 to (arr.sy() - 2))
         arr.set(i,j, r.nextDouble())
}
}

	// Code next should be generated
		
	/*	

	// node-based grid
	var sizeadd:Int = 1

	for ( i <- 0 to (nlevels - 1) ) 
	{
		SolA(i) = new MyArray(nrows+sizeadd,ncols+sizeadd )
		RHSA(i) = new MyArray(nrows+sizeadd,ncols+sizeadd )
		ResA(i) = new MyArray(nrows+sizeadd,ncols+sizeadd )

		println("lev: " + i + " " + nrows + " " + ncols)
		nrows= ( nrows/2 );
		ncols= ( ncols/2 );
	}

   // initialize solution and RHS
   SolA(0).set(0)
   helper.setRandom(SolA(0))
  
   // set stencil for Poisson problem, our PDE is Laplacian(Sol) = RHS
   laplacian.set(Array[Double](4,-1,-1,-1,-1))
   
   RHSA(0).set(0)

    println( "Residual after FMG iteration: " + Solver ( ))
  */

