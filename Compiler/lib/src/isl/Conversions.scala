import isl._

package isl {
  object Conversions {
    implicit def convertLambdaToXCallback1[ArgTy1,ExTy <: Exception](f: ArgTy1 => Unit): XCallback1[ArgTy1,ExTy] = {
      new XCallback1[ArgTy1,ExTy]() {
	def apply(arg1: ArgTy1) = f(arg1)
      }
    }

    implicit def convertLambdaToXCallback2[ArgTy1,ArgTy2,ExTy <: Exception](f: (ArgTy1,ArgTy2) => Unit): XCallback2[ArgTy1,ArgTy2,ExTy] = {
      new XCallback2[ArgTy1,ArgTy2,ExTy]() {
	def apply(arg1: ArgTy1, arg2: ArgTy2) = f(arg1,arg2)
      }
    }

    implicit def convertLambdaToXCallback3[ArgTy1,ArgTy2,ArgTy3,ExTy <: Exception](f: (ArgTy1,ArgTy2,ArgTy3) => Unit): XCallback3[ArgTy1,ArgTy2,ArgTy3,ExTy] = {
      new XCallback3[ArgTy1,ArgTy2,ArgTy3,ExTy]() {
	def apply(arg1: ArgTy1, arg2: ArgTy2, arg3: ArgTy3) = f(arg1,arg2,arg3)
      }
    }

    implicit def convertValToBigInt(v: Val): BigInt = {
      assert(v.getDen() == 1)
      new BigInt(v.getNum())
    }
    implicit def convertIntToVal(i: Int): Val = new Val(i.toString())
    implicit def convertBigIntToVal(i: BigInt): Val = new Val(i.toString())
    implicit def convertBigIntegerToBigInt(i: java.math.BigInteger): BigInt = new BigInt(i)
    implicit def convertBigIntegerToVal(i: java.math.BigInteger): BigInt = new Val(i.toString())
  }
}

