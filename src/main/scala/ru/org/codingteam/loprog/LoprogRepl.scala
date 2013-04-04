package ru.org.codingteam.loprog

import scala.io.Source

object LoprogRepl {
  def start(fileName: String) = {
    val predicates =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        LoprogParsers.removeComments(Source.fromFile(fileName).mkString)
      )

    if(predicates.successful) {
      while(true) {
        print("?- ")

        val query = LoprogParsers.parse(
          LoprogParsers.query,
          LoprogParsers.removeComments(readLine)
        )

        if(query.successful) {
          val vars = Utils.collectVars(query.get)

          Loprog.visitSolutions(predicates.get, query.get, {
            bindings => {
              val answer = vars.filter(bindings.contains(_)).map({
                varName => s"$varName = ${Loprog.showValue(varName, bindings)}"
              }).mkString(", ")

              print(answer + " ")

              if(readLine == ";")
                Next
              else
                Abort
            }
          })
        } else {
          println(query)
        }
      }
    } else {
      println(predicates)
    }
  }
}
