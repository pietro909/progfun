/**
 * Binary tree:
 * a set can be empty or not
 * the right node will be > of the parent
 **/

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x < elem) new NonEmpty(elem, left, right incl x)
    else this
  }

}

object Program {

  def main(args: Array[String]): Unit = {
    println(s"hello!")
  }

}