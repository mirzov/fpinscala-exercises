package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  def tail[A](l: List[A]): List[A] = l match{
		case Nil => Nil
		case Cons(h, tail) => tail
	}

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) l else drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match{
    case Nil => Nil
    case Cons(h, tail) =>
      if(f(h)) dropWhile(tail)(f) else l
  }

  def setHead[A](l: List[A])(h: A): List[A] = Cons(h, tail(l))

  def init[A](l: List[A]): List[A] = l match{
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
  }

  def length[A](l: List[A]): Int = {
    @tailrec
    def lengthAcc(acc: Int, l: List[A]): Int = l match{
      case Nil => acc
      case Cons(h, tail) => lengthAcc(acc+1, tail)
    }
    lengthAcc(0, l)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
		case Nil => z
		case Cons(h,t) => foldLeft(t, f(z,h))(f)
	}

	def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((rl,a) => Cons(a, rl))

	def reverse_viaFoldRight[A](l: List[A]): List[A] = l match{
		case Nil => Nil
		case Cons(h, t) => foldRight(reverse_viaFoldRight(t), Cons(h, Nil))(Cons(_,_))
	}

	def foldLeft_viaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
		foldRight(reverse_viaFoldRight(l), z)((a,b) => f(b,a))

	def foldRight_viaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
		foldLeft(reverse(l), z)((b,a) => f(a,b))

	def append_viaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

	def flattenList[A](ll: List[List[A]]): List[A] = foldRight(ll, Nil:List[A])(append(_,_))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match{
		case Nil => Nil
		case Cons(h,t) => Cons(f(h), map(t)(f))
	}

	def filter[A](l: List[A])(f: A => Boolean): List[A] = l match{
		case Nil => Nil
		case Cons(h,t) => if(f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
	}

	def filterOdd(l: List[Int]): List[Int] = filter(l)(_ % 2 == 0)

	def flatMap[A,B](l: List[A])(f: A => List[B]) = flattenList(map(l)(f))

	def filter_viaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if(f(a)) List(a) else Nil)

	def zip[A,B,C](la: List[A], lb: List[B])(f: (A,B) => C): List[C] = la match{
		case Nil => Nil
		case Cons(ha,ta) => lb match{
			case Nil => Nil
			case Cons(hb, tb) => Cons(f(ha,hb), zip(ta,tb)(f))
		}
	}

	@tailrec
	def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
		@tailrec
		def startsFrom[A](l: List[A], sub: List[A], maySucceed: Boolean): Boolean = maySucceed && (sub match {
			case Nil => true
			case Cons(sh, st) => l match{
				case Nil => false
				case Cons(lh, lt) => startsFrom(lt, st, sh == lh)
			}
		})
		startsFrom(l, sub, true) || (l match{
			case Nil => false
			case Cons(lh, lt) => hasSubsequence(lt,sub)
		})
	}

}
