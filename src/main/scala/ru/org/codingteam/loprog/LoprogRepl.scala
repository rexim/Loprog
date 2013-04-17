package ru.org.codingteam.loprog

import scala.io.Source
import scala.collection.mutable.ListBuffer

object LoprogRepl {
  def readQuery: String = {
    def read: List[String] => List[String] = _ match {
        case Nil => read(readLine :: Nil)
        case ls @ x :: xs => val trmd = x.trim()
          if (trmd == "" || trmd.last == '.') ls.reverse
          else { print("|  "); read(readLine :: ls) }
      }
    read(List.empty[String]) mkString ("\n")
  }

  def launch(fileName: String) {
    start(fileName)
  }

  private def start(fileName: String) = {
    val predicatesParseResult =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        LoprogParsers.removeComments(Source.fromFile(fileName).mkString)
      )

    if(predicatesParseResult.successful) {
      val predicates = predicatesParseResult.get

      while(true) {
        print("?- ");

        val queryParseResult = LoprogParsers.parse(
          LoprogParsers.query,
          LoprogParsers.removeComments(readQuery)
        )

        if(queryParseResult.successful) {
          val query = queryParseResult.get
          val vars = Utils.collectVariables(query)

          Loprog.visitSolutions(predicates, query, {
            bindings => {
              val answer = vars.filter(bindings.contains(_)).map({
                variable => s"${variable.name} = ${Loprog.showValue(variable, bindings)}"
              }).mkString(",\n")

              print(answer + " ");
              if(readLine == ";") {
                Next
              }
              else {
                Abort
              }
            }
          })
        } else {
          println(queryParseResult toString())
        }
      }
    } else {
      println(predicatesParseResult toString())
    }
  }
}
