package ru.org.codingteam.loprog

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.language.postfixOps // the compiler asked me nicely for that import

object LoprogParsers extends RegexParsers {
  def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
        (r findPrefixMatchOf (source.subSequence(start, source.length))) match {
        case Some(matched) =>
          Success(matched, in.drop(start + matched.end - offset))
        case None =>
          Failure(
            s"string matching regex `$r' expected but `${in.first}' found",
            in.drop(start - offset)
          )
      }
    }
  }

  def variable: Parser[Variable] =
    regexMatch("([_A-Z][_a-zA-Z0-9]*)".r) ^^ {
      m => Variable(m.group(1))
    }

  def functor: Parser[Functor] =
    functorName ~ (functorArguments ?) ^^ {
      case name ~ Some(args) => Functor(name, args)
      case name ~ None => Functor(name, List())
    }

  private def functorName: Parser[String] =
    regexMatch("([a-z][_a-zA-Z0-9]*)".r) ^^ { _.group(1) }

  private def functorArguments: Parser[List[Term]] =
    "(" ~ term ~ rep("," ~ term) ~ ")" ^^ {
      case _ ~ x ~ xs ~ _ =>
        x :: (xs.map({case _ ~ y => y}))
    }

  def term: Parser[Term] =
    functor | variable

  def predicate: Parser[Predicate] =
    functor ~ (predicateBody ?) ~ "." ^^ {
      case head ~ Some(body) ~ _ => Predicate(head, body)
      case head ~ None ~ _ => Predicate(head, List())
    }

  private def predicateBody: Parser[List[Functor]] =
    ":-" ~ functor ~ rep("," ~ functor) ^^ {
      case _ ~ x ~ xs =>
        x :: (xs.map({case _ ~ y => y}))
    }

  def sourceCode: Parser[List[Predicate]] = rep(predicate)

  def query: Parser[List[Functor]] =
    functor ~ rep("," ~ functor) ~ "." ^^ {
      case x ~ xs ~ _ =>
        x :: (xs.map({case _ ~ y => y}))
    }

  def removeComments(code: String): String =
    code.split("\n").map(_.takeWhile(_ != '%')).mkString("\n")
}
