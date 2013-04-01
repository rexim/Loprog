package ru.org.codingteam.loprog

abstract class Term

case class Functor(name: String, args: List[Term]) extends Term
case class Variable(name: String) extends Term

case class Predicate(head: Functor, body: List[Functor])

object Loprog {
  type Bindings = Map[String, Term]
  type VisitFunction = Bindings => Unit

  def unify(
    left: Term,
    right: Term,
    bindings: Bindings
  ): Option[Bindings] = (left, right) match {
    case (Variable(varName), right) =>
      bindings.get(varName) match {
        case Some(left) =>
          unify(left, right, bindings)
        case None =>
          Some(bindings + (varName -> right))
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
      Utils.scopePredicate(_, generator)
    )
  }

  private def visitSolutions(
    predicates: List[Predicate],
    query: List[Functor],
    bindings: Bindings,
    visit: VisitFunction,
    scope: Predicate => Predicate
  ): Unit = query match {
    case functor :: restOfQuery =>
      for(predicate <- predicates)
        scope(predicate) match {
          case Predicate(head, body) => 
            unify(head, functor, bindings) match {
              case Some(nextBindings) => {
                val nextQuery = body ++ restOfQuery
                visitSolutions(predicates, nextQuery, nextBindings, visit, scope)
              }

              case None => // skip the predicate
            }
        }

    case List() => visit(bindings)
  }
}
