
def abs(x: Double) = if (x < 0) -x else x

def isGoodEnough(guess: Double, x: Double) = {
  println(guess + " -> " + (abs(guess * guess - x) ))
  abs(guess * guess - x) < (x * 0.0001)
}

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)

def sqrt(n: Double) = sqrtIter(1, n)
