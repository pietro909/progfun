/**
 * Binary tree:
 * a set can be empty or not
 * the right node will be > of the parent
 **/

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  // union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
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

  override def toString = s"{${left} ${elem} ${right}}"

}

object BinaryTree {

  def main(args: Array[String]): Unit = {

    val empty = Empty
    println(s"Empty: ${empty}")

    val nonEmpty = new NonEmpty(3, empty, empty)
    println(s"NonEmpty: ${nonEmpty}")

    val multipleSets = nonEmpty.incl(7).incl(15).incl(4).incl(100)
    println(s"MultipleSets: ${multipleSets}")

  }
}
