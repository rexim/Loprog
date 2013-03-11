package com.github.rexim

abstract class Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Functor(name: String, args: List[Term]) extends Term

case class Predicate(head: Functor, body: List[Functor])

object LoprogContext {
  private var varNameCount = 0

  def generateVarName = {
    varNameCount += 1
    s"_G$varNameCount"
  }

  def unify(left: Term, right: Term): Map[String, Term] =
    (left, right) match {
      case (Atom(leftName), Atom(rightName))
          if leftName == rightName => Map()

      case (Variable(varName), something) => Map(varName -> something)

      // FIXME: unimplemented yet
      case (Functor(leftName, leftArgs), Functor(rightName, rightArgs))
          if leftName == rightName && leftArgs.size == rightArgs.size => null

      case _ => null
    }
}

class LoprogContext(predicates: List[Predicate]) {
  def performQuery(query: List[Functor]): List[Option[Map[String, Term]]] = null
}
