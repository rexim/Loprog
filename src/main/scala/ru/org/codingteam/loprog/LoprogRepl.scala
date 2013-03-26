package ru.org.codingteam.loprog

object LoprogRepl {
  def start(fileName: String) = {
    val predicates =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        scala.io.Source.fromFile(fileName).mkString
      ).get.map(Loprog.scopePredicate(_))

    while(true) {
      print("?- ")

      val query = LoprogParsers.parse(LoprogParsers.query, readLine)

      if(query.successful) {
        Loprog.visitSolutions(predicates, query.get, {
          m => {
            print(m)
            readLine
          }
        }, Map())
      } else {
        println(query)
      }
    }
  }
}
