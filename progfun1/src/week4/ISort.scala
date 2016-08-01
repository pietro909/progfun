
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

def isort(l: List[Int]): List[Int] = l match {
  case List() => l
  case x :: xs => insert(x, isort(xs))
}
