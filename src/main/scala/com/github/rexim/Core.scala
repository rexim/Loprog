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

      case (Variable(leftVarName), Variable(rightVarName)) => {
        val sharedVarName = generateVarName
        Map(
          leftVarName -> Variable(sharedVarName),
          rightVarName -> Variable(sharedVarName)
        )
      }

      case (Variable(varName), something) => Map(varName -> something)

      case (something, Variable(varName)) => Map(varName -> something)

      // FIXME: do it functional way
      case (Functor(leftName, leftArgs), Functor(rightName, rightArgs))
          if leftName == rightName && leftArgs.size == rightArgs.size => {
            var result = Map[String, Term]()

            for((left, right) <- leftArgs.zip(rightArgs)) {
              val bindings = unify(left, right)
              if(bindings != null) {
                for((key, value) <- bindings) {
                  if(!result.contains(key)) {
                    result = result + (key -> value)
                  } else if(value != result(key)) {
                    return null
                  }
                }
              } else {
                return null
              }
            }

            result
          }

      case _ => null
    }
}

class LoprogContext(predicates: List[Predicate]) {
  type VisitFunction = Map[String, Term] => Boolean

  def visitSolutions(
    query: List[Functor],
    visit: VisitFunction
  ): Unit = {}
}
