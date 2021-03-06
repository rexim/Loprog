package ru.org.codingteam.loprog

import scala.collection.mutable.HashMap

abstract class Term
case class Functor(name: String, args: List[Term]) extends Term
case class Variable(name: String) extends Term

case class Predicate(head: Functor, body: List[Functor])

abstract class VisitStatus
case object Abort extends VisitStatus
case object Next extends VisitStatus

object Loprog {
  type Bindings = Map[Variable, Term]
  type VisitFunction = Bindings => VisitStatus

  def unify(left: Term, right: Term, bindings: Bindings): Option[Bindings] =
    (left, right) match {
      case (variable: Variable, right) =>
        bindings.get(variable) match {
          case Some(left) =>
            unify(left, right, bindings)
          case None =>
            Some(bindings + (variable -> right))
        }

      case (left, variable: Variable) =>
        unify(variable, left, bindings)

      case (Functor(leftName, leftArgs), Functor(rightName, rightArgs))
          if leftName == rightName && leftArgs.size == rightArgs.size => {
            var result = bindings

            for((left, right) <- leftArgs.zip(rightArgs))
              unify(left, right, result) match {
                case Some(newBindings) =>
                  result = result ++ newBindings
                case None =>
                  return None
              }

            Some(result)
          }

    case _ => None
  }

  def visitSolutions(
    predicates: List[Predicate],
    query: List[Functor],
    visit: VisitFunction
  ): Unit = {
    val generator = Utils.createGenerator

    visitSolutions(
      predicates,
      query,
      Map(),
      visit,
      scopePredicate(_, generator)
    )
  }

  private def visitSolutions(
    predicates: List[Predicate],
    query: List[Functor],
    bindings: Bindings,
    visit: VisitFunction,
    scope: Predicate => Predicate
  ): VisitStatus = query match {

    case Functor("halt", List()) :: _ =>
      System.exit(0)
      Abort // actually, it doesn't matter

    case functor :: restOfQuery => {
      for(Predicate(head, body) <- predicates.map(scope))
        unify(head, functor, bindings) match {
          case Some(nextBindings) => {
            val nextQuery = body ++ restOfQuery
            val status = visitSolutions(
              predicates,
              nextQuery,
              nextBindings,
              visit,
              scope
            )

            if(status == Abort)
              return Abort
          }

          case None => // skip the predicate
        }
      Next
    }

    case List() => visit(bindings)
  }

  def showValue(variable: Variable, bindings: Bindings): String =
    bindings.get(variable) match {
      case Some(term) => showValue(variable, term, bindings)
      case None => variable.name
    }

  private def showValue(
    variable: Variable,
    term: Term,
    bindings: Bindings
  ): String = term match {
    case nextVariable: Variable =>
      if(nextVariable == variable) {
        "**"
      } else {
        bindings.get(nextVariable) match {
          case Some(nextTerm) =>
            showValue(variable, nextTerm, bindings)

          case None => nextVariable.name
        }
      }

    case Functor(name, args) =>
      if(args.isEmpty) {
        name
      } else {
        val showedArgs =
          args.map(showValue(variable, _, bindings)).mkString(", ")
        name + "(" + showedArgs + ")"
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
        Predicate(Functor(headName, headArgs.map(Utils.mapVarName(_, varScoper))),
          body.map({
            case Functor(bodyName, bodyArgs) =>
              Functor(bodyName, bodyArgs.map(Utils.mapVarName(_, varScoper)))
          })
        )
    }
  }
}
