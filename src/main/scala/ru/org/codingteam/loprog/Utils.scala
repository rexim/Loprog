package ru.org.codingteam.loprog

import scala.collection.mutable.HashMap

object Utils {
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

  def scopePredicate(predicate: Predicate, generator: () => String) = {
    val scope = new HashMap[String, String]()

    val varScoper: (String) => String =
      (name) => {
        scope.get(name) match {
          case Some(scopedName) => scopedName
          case None => {
            val scopedName = generator()
            scope.put(name, scopedName)
            scopedName
          }
        }
      }


    predicate match {
      case Predicate(Functor(headName, headArgs), body) =>
        Predicate(Functor(headName, headArgs.map(mapVarName(_, varScoper))),
          body.map({
            case Functor(bodyName, bodyArgs) =>
              Functor(bodyName, bodyArgs.map(mapVarName(_, varScoper)))
          })
        )
    }
  }
}
