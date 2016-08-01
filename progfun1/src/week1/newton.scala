
def abs(x: Double) = if (x < 0) -x else x

def isGoodEnough(guess: Double, x: Double) = {
  abs(guess * guess - x) < (x * 0.0001)
}

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)

def sqrt(n: Double) = sqrtIter(1, n)

def runTest = 
  List(0.001, 0.1e-20, 1.0e20, 1.0e50).foreach( n => {
    val actualSqrt = Math.sqrt(n)
    val mySqrt = sqrt(n)
    println(s"${n} -> ${actualSqrt} -> ${mySqrt}")
  })
