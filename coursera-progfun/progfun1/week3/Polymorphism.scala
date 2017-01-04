//package week3

trait MyList[T] {
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]
}

class Cons[T]( val head: T,
               val tail: MyList[T]
             ) extends MyList[T] {
  def isEmpty = false
}
class Nil[T] extends MyList[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

object Polymorphism {
  def main(args: Array[String]): Unit = {
    val l = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil)))
    //val e = find(7, l)
    val e = nth(22, l)
    println(e)
  }
  def find[T](n: Int, list: MyList[T]): T = {
    def loop(now: Int, list: MyList[T]): T = {
      if (list.isEmpty) throw new IndexOutOfBoundsException("too long")
      else if (now == n) list.head
      else loop(now + 1, list.tail)
    }
    loop(0, list)
  }
  // better solution with tail recursion
  def nth[T](n: Int, list: MyList[T]): T = 
    if (list.isEmpty) throw new IndexOutOfBoundsException("too long")
    else if (n == 0) list.head
    else nth(n - 1, list.tail)
}
