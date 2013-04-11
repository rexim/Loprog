package ru.org.codingteam.loprog

import scala.collection.mutable.HashSet

object Utils {
  def collectVariables(terms: List[Term]): List[Variable] = {
    val result = new HashSet[Variable]
    terms.foreach(foreachVariable(_, result.add(_)))
    result.toList
  }

  def mapVarName(term: Term, f: String => String): Term =
    term match {
      case Variable(name) => Variable(f(name))
      case Functor(name, args) =>
        Functor(name, args.map(mapVarName(_, f)))
    }

  def createGenerator: () => String = {
    var index = 0
    () => {
      index += 1
      s"_G$index"
    }
  }

  def foreachVariable(term: Term, action: Variable => Unit): Unit =
    term match {
      case variable: Variable => action(variable)
      case Functor(_, args) => args.foreach(foreachVariable(_, action))
    }
}
