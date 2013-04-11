import org.scalatest.FunSuite
import ru.org.codingteam.loprog._
import scala.collection.mutable.ListBuffer

class UtilsSuite extends FunSuite {
  test("collectVars") {
    val terms = List(
      // foo(X, a, bar(Y))
      Functor("foo",
        List(
          Variable("X"),
          Functor("a", List()),
          Functor("bar", List(Variable("Y")))
        )
      ),

      // baz(X, Z)
      Functor("baz",
        List(
          Variable("X"),
          Variable("Z")
        )
      )
    )

    val answer = List(
      Variable("X"),
      Variable("Y"),
      Variable("Z")
    ).sortWith(_.name < _.name)

    val result = Utils.collectVars(terms)

    assert(result.sortWith(_.name < _.name) === answer)
  }

  test("mapVarName") {
    // foo(X, bar(Y), baz)
    val term = Functor("foo",
      List(
        Variable("X"),
        Functor("bar", List(Variable("Y"))),
        Functor("baz", List())
      )
    )

    // foo(X::derp, bar(Y::derp), baz)
    val answer = Functor("foo",
      List(
        Variable("X::derp"),
        Functor("bar", List(Variable("Y::derp"))),
        Functor("baz", List())
      )
    )

    val result = Utils.mapVarName(term, _ + "::derp")

    assert(answer === result)
  }

  test("createGenerator") {
    val generator = Utils.createGenerator
    assert(generator() === "_G1")
    assert(generator() === "_G2")
    assert(generator() === "_G3")
  }

  test("foreachVariable") {
    // foo(X, bar(Y), baz)
    val term = Functor("foo",
      List(
        Variable("X"),
        Functor("bar", List(Variable("Y"))),
        Variable("X")
      )
    )

    val answer = List(
      Variable("X"),
      Variable("Y"),
      Variable("X")
    ).sortWith(_.name < _.name)

    val result = new ListBuffer[Variable]
    Utils.foreachVariable(term, result.append(_))

    assert(result.sortWith(_.name < _.name) === answer)
  }
}
