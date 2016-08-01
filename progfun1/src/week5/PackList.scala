def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
  }
}

val r = pack(List("a","a","a","b","c","c","a"))
println(r)
