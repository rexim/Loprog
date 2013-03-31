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

  def showValue(varName: String, bindings: Map[String, Term]): String =
    bindings.get(varName) match {
      case Some(term) => showValue(varName, term, bindings)
      case None => varName
    }

  private def showValue(
    varName: String,
    term: Term,
    bindings: Map[String, Term]
  ): String = term match {
    case Variable(nextVarName) =>
      if(nextVarName == varName) {
        "**"
      } else {
        bindings.get(nextVarName) match {
          case Some(nextTerm) =>
            showValue(varName, nextTerm, bindings)

          case None => nextVarName
        }
      }

    case Functor(name, args) =>
      if(args.isEmpty) {
        name
      } else {
        val showedArgs =
          args.map(showValue(varName, _, bindings)).mkString(", ")
        name + "(" + showedArgs + ")"
      }
  }
}
