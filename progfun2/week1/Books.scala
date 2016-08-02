
case class Book(title: String, authors: List[String])

object Books {

  val books = List(
    Book( title = "Elena Elena, amore mio", authors = List("Luciano de Crescenzo")),
    Book( title = "Il Dubbio", authors = List("Luciano de Crescenzo")),
    Book( title = "La Sacra Bibbia", authors = List("DIO")),
    Book( title = "Sticazzi", authors = List("Io")),
    Book( title = "Vecchio Testamento", authors = List("DIO")),
    Book( title = "Nuovo Testamento", authors = List("DIO"))
  )

  def find =
    (for {
      b1 <- books
      b2 <- books
      if b1.title < b2.title
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1).toSet

}
