package ru.org.codingteam.loprog

object Main {
  def main(args: Array[String]): Unit =
    if(args.size >= 1)
      LoprogRepl.launch(args(0))
    else
      println("Usage: loprog <source-code-file>")
}
