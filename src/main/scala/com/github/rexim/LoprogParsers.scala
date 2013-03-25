package com.github.rexim

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
          Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
      }
    }
  }

  def atom: Parser[Atom] =
    regexMatch("([a-z][a-zA-Z0-9]*)".r) ^^ {
      m => Atom(m.group(1))
    }

  def variable: Parser[Variable] =
    regexMatch("([A-Z][a-zA-Z0-9]*)".r) ^^ {
      m => Variable(m.group(1))
    }

  def functorName: Parser[String] =
    regexMatch("([a-z][a-zA-Z0-9]*)".r) ^^ { _.group(1) }

  def functorArguments: Parser[List[Term]] =
    "(" ~ term ~ rep("," ~ term) ~ ")" ^^ {
      case _ ~ x ~ xs ~ _ =>
        x :: (xs.map({case _ ~ y => y}))
    }

  def functor: Parser[Functor] =
    functorName ~ (functorArguments ?) ^^ {
      case name ~ Some(args) => Functor(name, args)
      case name ~ None => Functor(name, List())
    }

  def term: Parser[Term] =
    atom | variable | functor

  def predicate: Parser[Predicate] =
    "" ^^ {_ => Predicate(Functor("", List()), List())} // TODO: implement
}
