package ru.org.codingteam.loprog

object Utils {
  def addPrefixToVars(prefix: String, term: Term): Term =
    term match {
      case Functor(name, args) =>
        Functor(name, args.map(addPrefixToVars(prefix, _)))
      case Variable(varName) =>
        Variable(s"$prefix::$varName")
    }

  def collectVars(term: Term): Set[String] = term match {
    case Variable(varName) => Set(varName)
    case Functor(_, terms) =>
      terms.foldLeft(Set[String]())({
        case (acc, t) => acc ++ collectVars(t)
      })
  }
}
