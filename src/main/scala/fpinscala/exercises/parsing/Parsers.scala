package fpinscala.exercises.parsing

import scala.util.matching.Regex

trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  case class ParserOps[A](p: Parser[A])

  object Laws

trait CpParsers[Parser[+_]]:

  type Result[A]
  def succeed[A](a: A): Parser[A]
  def string(s: String): Parser[String]
  def regex(r: Regex): Parser[String]

  final val whitespace: Parser[String] = regex("\\s*".r)

  extension [A](p: Parser[A])
    def run(input: String): Result[A]
    def slice: Parser[String]
    final def map[B](f: A => B): Parser[B] = flatMap(a => succeed(f(a)))
    def flatMap[B](f: A => Parser[B]): Parser[B]

    def or[B >: A](p2: => Parser[B]): Parser[B]
    final def | [B >: A](p2: => Parser[B]): Parser[B] = or(p2)

    final def product[B](p2: => Parser[B]): Parser[(A, B)] = flatMap(a => p2.map(b => (a, b)))
    final def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def many: Parser[List[A]]
    def many1: Parser[List[A]]

    final def keepLeft[B](p2: => Parser[B]): Parser[A] = p.flatMap(a => p2.slice.map(_ => a))
    final def <*[B](p2: => Parser[B]): Parser[A] = p.keepLeft(p2)

    final def keepRight[B](p2: => Parser[B]): Parser[B] = p.slice.flatMap(_ => p2)
    final def *>[B](p2: => Parser[B]): Parser[B] = p.keepRight(p2)

    final def token: Parser[A] = p <* whitespace
  end extension

end CpParsers

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
