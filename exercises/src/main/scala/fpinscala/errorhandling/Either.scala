package fpinscala.errorhandling

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match{
	case Right(a) => Right(f(a))
	case _ => this.asInstanceOf[Either[E, B]]
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
	case Right(a) => f(a)
	case _ => this.asInstanceOf[Either[EE, B]]
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
	case Left(_) => b
	case _ => this
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match{
	case Right(aa) => b.map(f(aa,_))
	case _ => this.asInstanceOf[Either[EE, C]]
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!") 
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] = 
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }

  def sequence[E,A](seq: List[Either[E, A]]): Either[E, List[A]] = seq match {
	case Nil => Right(Nil)
	case h :: rest => h.flatMap(a => sequence(rest).map(a :: _))
  }
  
  def traverse[E,A,B](seq: List[A])(f: A => Either[E, B]): Either[E, List[B]] = seq match {
	case Nil => Right(Nil)
	case h :: tail => f(h).map2(traverse(tail)(f))(_ :: _)
  }
  
}