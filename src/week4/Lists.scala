object Lists {
  
  def apply[T]() = new List()

  def apply[T](a: T) = new List(a)

  def apply[T](a: T, b: T) = new List(a, b)

}
