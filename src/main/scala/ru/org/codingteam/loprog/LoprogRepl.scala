package ru.org.codingteam.loprog

import scala.io.Source
import scala.collection.mutable.ListBuffer

object LoprogRepl {
  def readQuery: String = {
    print("?- ")
    var result = new ListBuffer[String]

    var piece = readLine.trim
    while(piece.last != '.') {
      result.append(piece)
      print("|  ")
      piece = readLine.trim
    }
    result.append(piece)

    result.mkString("\n")
  }

  def start(fileName: String) = {
    val predicates =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        LoprogParsers.removeComments(Source.fromFile(fileName).mkString)
      )

    if(predicates.successful) {
      while(true) {
        val query = LoprogParsers.parse(
          LoprogParsers.query,
          LoprogParsers.removeComments(readQuery)
        )

        if(query.successful) {
          val vars = Utils.collectVars(query.get)

          Loprog.visitSolutions(predicates.get, query.get, {
            bindings => {
              val answer = vars.filter(bindings.contains(_)).map({
                varName => s"$varName = ${Loprog.showValue(varName, bindings)}"
              }).mkString(",\n")

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
