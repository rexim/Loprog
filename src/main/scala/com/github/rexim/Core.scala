package com.github.rexim

abstract class Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Functor(name: String, args: List[Term]) extends Term

case class Predicate(head: Functor, body: List[Functor])

object LoprogContext {
  def unify(
    left: Term,
    right: Term,
    bindings: Map[String, Term]
  ): Boolean = (left, right) match {
    case (Atom(leftName), Atom(rightName)) => leftName == rightName

    case (atom: Atom, Variable(varName)) =>
      if(bindings.contains(varName)) unify(atom, bindings.get(varName))
      else true

    case (variable: Variable, atom: Atom) => unify(atom, variable)

    case (Variable(leftName), Variable(rightName)) =>
      if(bindings.contains(leftName) && bindings.contains(rightName)) {
        unify(bindings(leftName), bindings(rightName))
      } else {
        true
      }

    // ...

    case _ => false
  }
}

class LoprogContext(predicates: List[Predicate]) {
  type VisitFunction = Map[String, Term] => Boolean

  def visitSolutions(
    query: List[Functor],
    visit: VisitFunction
  ): Unit = {}
}
