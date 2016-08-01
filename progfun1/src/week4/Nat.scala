abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new NoSuchElementException("can't go below zero")
  def +(that: Nat) = that
  def -(that) = if (that.isZero) this else throw new Error("can't go -0")
}

class Succ(nat: Nat) extends Nat {
  def isZero = false
  def predecessor = nat
  def +(that: Nat) = new Succ(nat + that)
  def -(that: Nat) = nat - that.predecessor
}
