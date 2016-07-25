package week6

import scala.io.Source

object phoneNumbers {

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
        words groupBy wordCode

    def encode(number: String): Set[Seq[String]] =
        if (number.isEmpty) Set(Seq())
        else
            for {
                i <- 1 to numer.length
                word <- number take i
                rest <- encode(number drop i)
            } yield word::rest

}
