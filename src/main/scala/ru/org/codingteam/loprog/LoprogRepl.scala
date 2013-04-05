package ru.org.codingteam.loprog

import scala.io.Source
import scala.collection.mutable.ListBuffer
import jline.console.ConsoleReader
import jline.TerminalFactory

object LoprogRepl {
  def readQuery(con: ConsoleReader): String = {
    var result = new ListBuffer[String]

    var piece = con.readLine().trim
    while(piece.length() != 0 && piece.last != '.') {
      result.append(piece)
      con.setPrompt("")
      con.print("|  "); con.flush()
      piece = con.readLine().trim
    }
    result.append(piece)

    result.mkString("\n")
  }

  def launch(fileName: String) {
    try {
      start(fileName)
    } finally {  TerminalFactory.get().restore(); }
  }

  private def start(fileName: String) = {
    val con = new ConsoleReader()
    val predicates =
      LoprogParsers.parse(
        LoprogParsers.sourceCode,
        LoprogParsers.removeComments(Source.fromFile(fileName).mkString)
      )

    if(predicates.successful) {
      while(true) {
        con.setPrompt("?- ")
        val query = LoprogParsers.parse(
          LoprogParsers.query,
          LoprogParsers.removeComments(readQuery(con))
        )

        if(query.successful) {
          val vars = Utils.collectVars(query.get)

          Loprog.visitSolutions(predicates.get, query.get, {
            bindings => {
              val answer = vars.filter(bindings.contains(_)).map({
                varName => s"$varName = ${Loprog.showValue(varName, bindings)}"
              }).mkString(",\n")

              con.print(answer + " "); con.flush()
              con.setPrompt("")
              if(con.readCharacter() == ';') {
                con.println(";");
                Next
              }
              else {
                con.println("")
                Abort
              }
            }
          })
        } else {
          con.println(query toString())
        }
      }
    } else {
      con.println(predicates toString())
    }
  }
}
