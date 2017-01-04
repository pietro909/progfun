
object HOF {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else go(n+1)
    go(0)
  }
  
  def lt(a: Int, b: Int) = a < b
  val a1 = Array(1,2,3,4)
  println(isSorted[Int](a1, lt))
  val a2 = Array(2,3,1,4)
  println(isSorted[Int](a2, lt))
  
  def partial2[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)
  
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)
  
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)
  
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a) => f(g(a))
  
  def sum(a: Int, b: Int): Int = a + b
  val curriedSum = curry(sum)
  curriedSum(1)(2)
  val uncurriedSum = uncurry(curriedSum)
  uncurriedSum(1, 2)
  
  def intToFloat(i: Int): Float = i.toFloat
  def floatToString(f: Float): String = f.toString
  val intAsFloatToString = compose(floatToString, intToFloat)
  intAsFloatToString(1)

}
