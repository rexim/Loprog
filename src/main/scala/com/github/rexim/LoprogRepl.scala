package com.github.rexim

object LoprogRepl {
  def start(fileName: String) = {
    val predicates =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        scala.io.Source.fromFile(fileName).mkString
      ).get.map(Loprog.scopePredicate(_))

    while(true) {
      print("?- ")

      val query =
        LoprogParsers.parse(
          LoprogParsers.query,
          readLine
        ).get

      Loprog.visitSolutions(predicates, query, {
        m => {
          println(m)
          readLine
        }
      }, Map())
    }
  }
}
