package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map { case (name: String, expr: Signal[Expr]) =>
      (name, Signal(eval(expr(), namedExpressions.filterNot(_._1 == name))))
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v: Double) => v match {
      case Double.NaN => throw new Error("NaN")
      case _ => v
    }
    case Ref(name: String) => eval(getReferenceExpr(name, references), references)
    case Plus(a: Expr, b: Expr) => eval(a, references)+eval(b, references)
    case Minus(a: Expr, b: Expr) => eval(a, references)-eval(b, references)
    case Times(a: Expr, b: Expr) => eval(a, references)*eval(b, references)
    case Divide(a: Expr, b: Expr) => eval(a, references)/eval(b, references)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
