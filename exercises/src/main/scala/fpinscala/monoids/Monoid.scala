package fpinscala.monoids

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int]{
		def op(i1: Int, i2: Int): Int = i1 + i2
		def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
		def op(i1: Int, i2: Int): Int = i1 * i2
		def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean]{
		def op(b1: Boolean, b2: Boolean): Boolean = b1 || b2
		def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean]{
		def op(b1: Boolean, b2: Boolean): Boolean = b1 && b2
		def zero: Boolean = true
  }
  
  val booleanXor: Monoid[Boolean] = new Monoid[Boolean]{
		def op(b1: Boolean, b2: Boolean): Boolean = b1 ^ b2
		def zero: Boolean = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
		def op(o1: Option[A], o2: Option[A]): Option[A] = o1 orElse o2
		def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]{
		def op(f1: A => A, f2: A => A): A => A = f1 compose f2
		def zero: A => A = a => a
  }

  import org.scalacheck.{Prop,Gen}

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
	Prop.forAll(gen, gen, gen)( (a1,a2,a3) =>
		m.op(a1,m.op(a2,a3)) == m.op(m.op(a1,a2),a3)
	) &&
	Prop.forAll(gen)(a =>
		m.op(m.zero,a) == a && m.op(a, m.zero) == a
	)
  
  def optMonoidProp = monoidLaws(optionMonoid[Int], Gen.posNum[Int].map{n => if(n % 2 == 0) Some(n) else None})

  def wordsMonoid: Monoid[String] = new Monoid[String]{
		def op(s1: String, s2: String) = opInv(s2, s1)
		def opInv(s1: String, s2: String): String = 
			if(s1 == zero) s2
			else if(s2 == zero) s1
			else (s1.trim + " " + s2.trim).trim
		val zero: String = " "
  }
	
  def wordsMonoidProp = monoidLaws(wordsMonoid, Gen.oneOf("a", " a", "a ", "", " ", "\t", "\ta"))

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    sys.error("todo")

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    sys.error("todo")

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

