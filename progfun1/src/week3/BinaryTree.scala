/**
 * Binary tree:
 * a set can be empty or not
 * the right node will be > of the parent
 **/

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString = "."
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  override def toString = s"{${left} ${elem} ${right}}"

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

object BinaryTree {

  def main(args: Array[String]): Unit = {

    val empty = Empty
    println(s"Empty: ${empty}")

    val nonEmpty = new NonEmpty(3, empty, empty)
    println(s"NonEmpty: ${nonEmpty}")

    val multipleSets = nonEmpty.incl(7).incl(15).incl(4).incl(100)
    println(s"MultipleSets: ${multipleSets}")

    val unionSet = multipleSets.incl(10)//.union(nonEmpty)
    println(s"unionset: ${unionSet}")

  }
}
