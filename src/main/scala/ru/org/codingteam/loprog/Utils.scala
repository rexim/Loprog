package ru.org.codingteam.loprog

object Utils {
  def addPrefixToVars(prefix: String, term: Term): Term =
    term match {
      case Functor(name, args) =>
        Functor(name, args.map(addPrefixToVars(prefix, _)))
      case Variable(varName) =>
        Variable(s"$prefix::$varName")
    }

  def collectVars(terms: List[Term]): Set[String] =
    terms.foldLeft(Set[String]()) {
      case (acc, t) => t match {
        case Variable(varName) => acc ++ Set(varName)
        case Functor(_, args) => acc ++ collectVars(args)
      }
    }
}
