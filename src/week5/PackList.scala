def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val r = xs1.span(y => y == x)
    (x::r._1) :: pack(r._2)
  }
}

val r = pack(List("a","a","a","b","c","c","a"))
println(r)
