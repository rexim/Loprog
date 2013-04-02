package ru.org.codingteam.loprog

object Utils {
  def collectVars(terms: List[Term]): Set[String] =
    terms.foldLeft(Set[String]()) {
      case (acc, t) => t match {
        case Variable(varName) => acc ++ Set(varName)
        case Functor(_, args) => acc ++ collectVars(args)
      }
    }

  def mapVarName(term: Term, f: (String) => String): Term =
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
}
