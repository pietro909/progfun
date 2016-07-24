package week6

object polynomials {

  class Poly(_terms: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = _terms withDefaultValue 0.0

//    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

    private def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
      terms + (term._1 -> (term._2 + terms(term._1)))


    private def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield s"${coeff}x^$exp") mkString(" + ")

  }

  def test = {
    val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
    val r = p1 + p2
    println(r)
  }

}
