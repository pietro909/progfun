object MergeSort {

  /*
  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    xs match {
      case List() => ys
      case x::xst =>
        ys match {
          case List() => xs
          case y::yst =>
            if (x <= y) x :: merge(xst, ys)
            else y :: merge(xs, yst)
        }
    }
  */

  def merge(xs: List[Int], ys: List[Int]): List[Int] =
    (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x::xst, y::yst) =>
        if (x < y) x::merge(xst, ys)
        else y::merge(xs, yst)
    }

  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

}

val l = List(5,1,7,3,88,0)
val r = MergeSort.msort(l)
println(l)
println(r)
