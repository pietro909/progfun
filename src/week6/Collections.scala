object Collections {

  def main(args: Array[String]): Unit = {
    val action = args(0)
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
    // { case (x, y) => x * y }
    println(s"$xs x $ys = $result")
    result
  }

  def isPrime(n: Int): Boolean =
    (2 until n) forall { d => n % d != 0 }
}
