// package week7

import scala.io.Source

object phoneNumbers {

  def main(args: Array[String]): Unit = {
    val number = if (args.size > 0) args(0) else ""
    val results = translate(number)
    println(s"number $number can be encoded as")
    results map println
    // println( wordsForNum )
  }


  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

    val words = in.getLines.toList filter (word => word forall (_.isLetter))

    val mnemonics = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

    val charCode: Map[Char, Char] =
      for {
        (number, v) <- mnemonics
        letter <- v
      } yield letter -> number

    def wordCode(word: String): String =
      word.toUpperCase map charCode

    def wordsForNum: Map[String, Seq[String]] =
       words groupBy wordCode withDefaultValue(Seq())

    /**
     * given "123"
     *  calculate 1 23 12 3
     *  return a set with all the result in one seq
     **/
    def encode(number: String): Set[List[String]] =
        if (number.isEmpty) Set(List())
        else
            (for {
                i <- 1 to number.length
                word <- wordsForNum(number take i)
                rest <- encode(number drop i)
            } yield word :: rest).toSet

    def translate(number: String): Set[String] =
      encode(number) map { _ mkString " " }

}
