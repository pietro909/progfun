def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)

def recFactorial(n: Int): Int = {
  def recur(n: Int, acc: Int): Int =
    if (n == 0) acc else recur(n - 1, n * acc)

  recur(n, 1)
}
