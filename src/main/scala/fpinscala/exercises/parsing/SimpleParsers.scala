package fpinscala.exercises.parsing

import scala.util.matching.Regex

object SimpleParsers extends CpParsers[SimpleParsers.Parser] {

  sealed trait Result[+A]
  case class Error(msg: String) extends Result[Nothing]
  case class Success[A](value: A, remainingInput: String) extends Result[A]

  type Parser[+A] = String => Result[A]

  override def succeed[A](a: A): Parser[A] = input => Success(a, input)

  override def string(s: String): Parser[String] = input =>
    if input.startsWith(s) then Success(s, s.drop(s.length))
    else
      val safeInputPeek = peekInfo(input, s.length)
      Error(s"Input does not start with $s, it $safeInputPeek")

  override def regex(r: Regex): Parser[String] = input =>
    r.findPrefixOf(input) match
      case Some(res) => Success(res, input.drop(res.length))
      case None => Error(s"Input did not start with Regex $r , it ${peekInfo(input, 10)}")

  private def peekInfo(input: String, size: Int): String =
    val safePeekLength = Math.min(size, input.length)
    if safePeekLength == 0 then "is empty" else s"starts with ${input.take(safePeekLength)}"


  extension [A](p: Parser[A])
    override def run(input: String): Result[A] = p(input)
    override def slice: Parser[String] = ???
    override def flatMap[B](f: A => Parser[B]): Parser[B] = ???
    override def or[B >: A](p2: => Parser[B]): Parser[B] = ???
    override def many: Parser[List[A]] = ???
    override def many1: Parser[List[A]] = ???

}
