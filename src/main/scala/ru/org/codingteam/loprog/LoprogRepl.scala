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
        val vars = Utils.collectVars(query.get)

        Loprog.visitSolutions(predicates, query.get, {
          bindings => {
            for(varName <- vars)
              bindings.get(varName) match {
                case Some(term) => println(s"$varName = $term")
                case None => // skip the varName
              }

            readLine
          }
        })
      } else {
        println(query)
      }
    }
  }
}
