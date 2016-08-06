object Lists {
  def removeAt[T](n: Int, xs: List[T]) = {
    def loop(counter: Int, ys: List[T], result: List[T]): List[T] =
      ys match {
        case List() => result
        case h::t =>
          if (counter == n) result ++ t
          else loop(counter + 1, t, h::result)
      }
    loop(0, xs, List())
  }
}

val res = Lists.removeAt[Char](1, List('a','b','c','d'))
println(res)
