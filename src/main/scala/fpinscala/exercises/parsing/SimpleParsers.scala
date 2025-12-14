package fpinscala.exercises.parsing

import scala.util.matching.Regex

object SimpleParsers extends CpParsers[SimpleParsers.Parser] {

  type Parser[+A] = String => Either[String, (A, String)]

  override def succeed[A](a: A): Parser[A] = ???

  override def string(s: String): Parser[String] = ???

  override def regex(r: Regex): Parser[String] = ???

  extension [A](p: Parser[A])
    override def run(input: String): Either[String, A] = ???
    override def slice: Parser[String] = ???
    override def flatMap[B](f: A => Parser[B]): Parser[B] = ???
    override def or[B >: A](p2: => Parser[B]): Parser[B] = ???
    override def many: Parser[List[A]] = ???
    override def many1: Parser[List[A]] = ???

}
