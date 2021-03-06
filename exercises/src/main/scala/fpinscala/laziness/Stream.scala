package fpinscala.laziness

import Stream._

trait Stream[+A] {
	def uncons: Option[(A, Stream[A])]
	
	def isEmpty: Boolean = uncons.isEmpty
	
	def foldRight[B](z: => B)(f: (A, => B) => B): B = uncons match {
		case Some((h, t)) => f(h, t.foldRight(z)(f))
		case None => z
    }

	def toList: List[A] = uncons match{
		case None => Nil
		case Some((h, t)) => h :: t.toList
	}

	def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

	def take(n: Int): Stream[A] = 
		if (n <= 0) empty
		else uncons match {
      case Some((h, t)) => cons(h, t.take(n - 1))
      case None => sys.error("nothing to take from an empty stream!")
    }

	def takeWhile(p: A => Boolean): Stream[A] = uncons match {
		case None => empty
		case Some((h, t)) => if(p(h)) cons(h, t.takeWhile(p)) else empty
	}
	
	def takeWhile_foldright(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty){(a, s) => 
		if(p(a)) cons(a, s) else empty
	}

	def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

	def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, s) => cons(f(a), s))

	def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,s) => {
		if(p(a)) cons(a,s) else s
	})
	
	def collect[B](f: PartialFunction[A,B]): Stream[B] = foldRight(empty[B])((a,s) => {
		if(f.isDefinedAt(a)) cons(f(a),s) else s
	})

	def append[B >: A](bs: Stream[B]): Stream[B] = foldRight(bs)(cons(_,_))

	def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,s) => f(a) append s)

	def map_unfold[B](f: A => B): Stream[B] = unfold(this)(
		_.uncons.map{case (ahead, atail) => (f(ahead), atail)}
	)

	def take_unfold(n: Int): Stream[A] = unfold((this,n)){
		case (s,k) => if(k <= 0) None else s.uncons match{
			case None => sys.error("nothing to take from an empty stream!")
			case Some((h,t)) => Some(h, (t, k - 1))
		}
	}

	def takeWhile_unfold(p: A => Boolean): Stream[A] = unfold(this)(
		_.uncons.flatMap{case (ahead, atail) => if(p(ahead)) Some(ahead, atail) else None}
	)
	
	def tails: Stream[Stream[A]] = cons(this, unfold(this)(s => s.uncons map{
		case (_, tail) => (tail, tail)
	}))
	
	def head: A = uncons.map(_._1).getOrElse(throw new Exception("no head in an empty stream!"))
	
	def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = uncons match {
		case Some((h, t)) =>
			val tb = t.scanRight(z)(f) 
			val hb = f(h, tb.head)
			cons(hb, tb)
		case None => Stream(z)
	}
	
}

object Stream {

	def empty[A]: Stream[A] = 
		new Stream[A] { def uncons = None }
  
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
		new Stream[A] {
			lazy val uncons = Some((hd, tl)) 
		}
  
	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))

	val ones: Stream[Int] = cons(1, ones)
	
	def constant[A](a: A): Stream[A] = cons(a, constant(a))
	
	def from(n: Int): Stream[Int] = cons(n, from(n + 1))

	val fibs: Stream[Long] = {
		def fibsFrom(f1: Long, f2: Long): Stream[Long] = cons(f1, fibsFrom(f2, f1 + f2))
		fibsFrom(0, 1)
	}

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match{
		case None => empty[A]
		case Some((a,s)) => cons(a, unfold(s)(f))
	}

	val fibs_unfold: Stream[Long] = unfold((0,1)){case (f1,f2) => Some((f1, (f2, f1 + f2)))}

	def from_unfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

	def constant_unfold[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

	val ones_unfold = constant_unfold(1)
	
	def zip[A,B](s1: Stream[A], s2: Stream[B]): Stream[(A, B)] = unfold((s1.uncons,s2.uncons)){
		case ( Some((h1,t1)), Some((h2,t2)) ) => Some( ((h1,h2), (t1.uncons,t2.uncons)) )
		case _ => None
	}
	
	def zipAll[A,B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((s1,s2))(s => {
		def optionizeTuple[T](opt: Option[(T, Stream[T])]): (Option[T], Stream[T]) = opt.map{
			case(t,s) => (Some(t),s)
		}.getOrElse((None, empty))
		
		val (h1,t1) = optionizeTuple(s._1.uncons)
		val (h2,t2) = optionizeTuple(s._2.uncons)
		
		if (h1 == None && h2 == None) None else Some( (h1,h2), (t1,t2) )
	})
	
	def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean =
		zipAll(s,s2).collect{
			case (Some(a1),Some(a2)) if !a1.equals(a2) => false
			case (None, Some(a2)) => false
			case (Some(a1), None) => true
			case (None, None) => true
		}.uncons.map(_._1).getOrElse(true)
}
