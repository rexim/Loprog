package com.github.rexim

import scala.util.parsing.combinator._
import scala.util.matching.Regex

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

  def atom = regexMatch("([a-z][a-zA-Z0-9]*)".r) ^^ {
    m => Atom(m.group(1))
  }

  def variable = regexMatch("([A-Z][a-zA-Z0-9]*)".r) ^^ {
    m => Variable(m.group(1))
  }

  def functor = "" // TODO: implement

  def predicate = "" // TODO: implement
}
