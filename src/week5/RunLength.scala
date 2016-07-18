def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs.span(y => y == x)
    (x, first.length) :: encode(rest)
  }
}

val r = encode(List("a","a","a","b","c","c","a"))
println(r)
