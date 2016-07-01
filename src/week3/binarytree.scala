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

object EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
  override def toString = "."
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x < elem) new NonEmptySet(elem, left, right incl x)
    else this
  }

  override def toString = s"{${left} ${elem} ${right}}"

}

object Program {

  def main(args: Seq[String]) = {

    val empty = EmptySet
    println(s"Empty: ${empty}")

    val nonEmpty = new NonEmptySet(3, empty, empty)
    println(s"NonEmpty: ${nonEmpty}")
  }
}
