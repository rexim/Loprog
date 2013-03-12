import org.scalatest._
import org.scalatest.matchers._
import com.github.rexim.{
  LoprogContext,
  Predicate,
  Functor,
  Atom,
  Variable
}

class LoprogContextSpec extends FlatSpec with ShouldMatchers {
  "A LoprogContext" should "perform simple queries on facts" in {
    val predicates = List(
      Predicate(Functor("human", List(Atom("john"))), List()),
      Predicate(Functor("human", List(Atom("lisa"))), List()),
      Predicate(Functor("cat", List(Atom("vasya"))), List()),
      Predicate(Functor("cat", List(Atom("toma"))), List()),
      Predicate(Functor("male", List(Atom("john"))), List()),
      Predicate(Functor("male", List(Atom("vasya"))), List()),
      Predicate(Functor("female", List(Atom("lisa"))), List()),
      Predicate(Functor("female", List(Atom("toma"))), List())
    )

    val queryCatFemale = List(
      Functor("cat", List(Variable("X"))),
      Functor("female", List(Variable("X")))
    )

    val queryHumanMale = List(
      Functor("human", List(Variable("X"))),
      Functor("male", List(Variable("X")))
    )

    val context = new LoprogContext(predicates)

    context.performQuery(queryCatFemale) should equal (List(Some(Map("X" -> Atom("toma")))))
    context.performQuery(queryHumanMale) should equal (List(Some(Map("X" -> Atom("john")))))
  }

  it should "support operation of unification" in {
    // p(X, f(Y), a)
    val left = Functor("p", List(
      Variable("X"),
      Functor("f", List(Variable("Y"))),
      Atom("a")
    ))

    // p(a, f(a), Y)
    val right = Functor("p", List(
      Atom("a"),
      Functor("f", List(Atom("a"))),
      Variable("Y")
    ))

    // ?- p(X, f(Y), a) = p(a, f(a), Y).
    // X = a, Y = a.
    LoprogContext.unify(left, right) should equal (Map("X" -> Atom("a"), "Y" -> Atom("a")))
  }
}
