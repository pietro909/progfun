package week6

import scala.io.Source

object phoneNumbers {

//  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")

//    val words = in.getLines

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



}
