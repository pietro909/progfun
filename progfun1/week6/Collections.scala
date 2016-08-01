object Collections {

  def main(args: Array[String]): Unit = {
    val action = if (args.size > 0) args(0) else ""
    println(forScalarProduct(List(1,2,3), List(3,4,5)))
    action match {
      case "combine" =>
        combine(args(1).toInt, args(2).toInt)
      case "scalar" =>
        // scalarProduct(
        println(args(1))
      case "prime" =>
        val n = args(1).toInt
        val r = isPrime(n)
        println(s"is $n prime? $r")
      case "prime-pairs" =>
        val n = args(1).toInt
        val pairs = primePairs(n)
        println(pairs)
      case _ =>
        println(s"can't resolve $action")
    }
  }

  def combine(n: Int, m: Int): List[(Int,Int)] = {
    val result = (1 to n) flatMap (x => (1 to m) map (y => (x, y)))
    println(s"combine 1 to $n with 1 to $m gives $result")
    result.toList
  }

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    val result = (xs zip ys).map(xy => xy._1 * xy._2).sum
    println(s"$xs x $ys = $result")
    result
  }

  def forScalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ( (x, y) <- xs zip ys ) yield x * y).sum

  def isPrime(n: Int): Boolean =
    (2 until n) forall { d => n % d != 0 }

  // with higher order functions
  def findPrimePairs(n: Int): IndexedSeq[(Int, Int)] =
    (1 until n) flatMap ( i =>
        (1 until n) map ( j => (i, j) )) filter { case(i, j) => isPrime(i+j) }

  def primePairs(n: Int): IndexedSeq[(Int, Int)] =
    for {
      i <- 1 until n
      j <- 1 until n
      if isPrime(i+j)
    } yield (i, j)
}
