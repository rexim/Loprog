package com.github.rexim

abstract class Term
case class Atom(name: String) extends Term
case class Variable(name: String) extends Term
case class Functor(name: String, args: List[Term]) extends Term

case class Predicate(head: Functor, body: List[Functor])

class LoprogContext(predicates: List[Predicate]) {
  def performQuery(query: List[Functor]): List[Option[Map[String, Term]]] = null
}
