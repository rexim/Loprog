package com.github.rexim

abstract class Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Functor(name: String, args: List[Term]) extends Term

case class Predicate(head: Functor, body: List[Functor])

object Loprog {
  type VisitFunction = Map[String, Term] => Unit

  def unify(
    left: Term,
    right: Term,
    bindings: Map[String, Term]
  ): Option[Map[String, Term]] = (left, right) match {
    case (Atom(leftName), Atom(rightName)) if leftName == rightName =>
      Some(bindings)

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
    visit: VisitFunction,
    bindings: Map[String, Term]
  ): Unit = {
    // ...
  }
}
