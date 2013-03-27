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
    visit: VisitFunction,
    bindings: Bindings
  ): Unit = query match {
    case functor :: restOfQuery =>
      for(Predicate(head, body) <- predicates.map(scopePredicate(_)))
        unify(head, functor, bindings) match {
          case Some(nextBindings) => {
            val nextQuery = collectGarbage(query, body ++ restOfQuery)
            visitSolutions(predicates, nextQuery, visit, nextBindings)
          }
          case None => // skip the predicate
        }

    case List() => visit(bindings)
  }

  def collectGarbage(query: List[Functor], bindings: Bindings) =
    // FIXME(rexim): implement this. Just remove from the bindings
    // variables which are not used in the query.
    bindings

  def addPrefixToVars(prefix: String, term: Term): Term =
    term match {
      case Functor(name, args) =>
        Functor(name, args.map(addPrefixToVars(prefix, _)))
      case Variable(varName) =>
        Variable(s"$prefix::$varName")
    }

  // FIXME(rexim): omg, what is that? (refactoring is required)
  var varPrefixNumber = 0
  def scopePredicate(predicate: Predicate): Predicate = {
    varPrefixNumber += 1
    val hashCode = predicate.hashCode
    val prefix = s"$varPrefixNumber::$hashCode"

    predicate match {
      case Predicate(Functor(headName, headArgs), body) =>
        Predicate(
          Functor(headName, headArgs.map(addPrefixToVars(prefix, _))),
          body.map({
            case Functor(bodyName, bodyArgs) =>
              Functor(bodyName, bodyArgs.map(addPrefixToVars(prefix, _)))
          })
        )
    }
  }
}
