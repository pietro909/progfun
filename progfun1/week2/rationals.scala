
class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be non-zero")

    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int =
        if (b == 0) a else gcd(b, a % b)

    private val g = gcd(x, y)

    def numerator = x / g
    def denominator = y / g

    def + (r: Rational) =
        new Rational(
            numerator * r.denominator + r.numerator * denominator,
            denominator * r.denominator
        )

    def unary_- =
        new Rational(-numerator, denominator)

    def - (r: Rational) =
        this.+(-r)

    def < (r: Rational) =
        r.numerator * denominator > numerator * r.denominator

    def > (r: Rational) = ! this.<(r)

    def max(r: Rational) = if (this.<(r)) r else this

    override def toString =
        numerator + "/" + denominator
}

val x = new Rational(1, 3)
val y = new Rational(1, 7)
val z = new Rational(3, 2)

// x.+(y).mul(z)

x - y - z
x < y
x > z
x max z

