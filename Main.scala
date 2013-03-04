
abstract class Term;
case class Variable(name: String) extends Term
case class Functor(name: String, args: List[Term]) extends Term

case class Predicate(head: Functor, body: List[Functor])

object Main {
  val predicates = List(
    Predicate(Functor("human", List(Functor("john", List()))),   List()),
    Predicate(Functor("human", List(Functor("lisa", List()))),   List()),
    Predicate(Functor("human", List(Functor("silver", List()))), List()),
    Predicate(Functor("human", List(Functor("toby", List()))),   List()),
    Predicate(Functor("human", List(Functor("diana", List()))),  List()),
    Predicate(Functor("human", List(Functor("anny", List()))),   List()),

    Predicate(Functor("man", List(Functor("john", List()))),   List()),
    Predicate(Functor("man", List(Functor("silver", List()))), List()),
    Predicate(Functor("man", List(Functor("toby", List()))),   List()),

    Predicate(Functor("woman", List(Functor("lisa", List()))),   List()),
    Predicate(Functor("woman", List(Functor("diana", List()))),  List()),
    Predicate(Functor("woman", List(Functor("anny", List()))),   List()),
  )

  def query(q: Functor) = {
    predicate.filter(_ match {
      case Predicate(Functor(q.name, _), _) => true
      case _ => false
    })
  }
}
