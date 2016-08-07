object Primes {
    def from(n: Int): Stream[Int] = n #:: from(n + 1)

    val nats = from(0)

    val m4s = nats map (_ * 4)

    def sieve(s: Stream[Int]): Stream[Int] =
        s.head #:: sieve(s.tail filter (_ % s.head != 0))

    val primes = sieve(from(2))

    def sqrtStream(x: Double): Stream[Double] = {
        def improve(guess: Double): Double = (guess + x/guess) / 2
        lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
        guesses
    }

    def isGoodEnough(x: Double)(guess: Double): Boolean =
        math.abs((guess * guess - x) / x) < 0.0000001

    def sqrt(x: Double, tolerance: Double = 0.01): Double =
        sqrtStream(x) filter (isGoodEnough(x)) head

}
