package ru.org.codingteam.loprog

object LoprogRepl {
  def start(fileName: String) = {
    val predicates =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        scala.io.Source.fromFile(fileName).mkString
      ).get

    while(true) {
      print("?- ")

      val query = LoprogParsers.parse(LoprogParsers.query, readLine)

      if(query.successful) {
        Loprog.visitSolutions(predicates, query.get, {
          bindings => {
            bindings.foreach {
              case (varName, term) =>
                println(s"$varName = $term")
            }

            readLine
          }
        }, Map())
      } else {
        println(query)
      }
    }
  }
}
