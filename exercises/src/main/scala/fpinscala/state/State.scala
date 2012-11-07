package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & // `&` is bitwise AND
                  ((1L << 48) - 1) // `<<` is left binary shift
      ((seed2 >>> 16).asInstanceOf[Int], // `>>>` is right binary shift with zero fill
       simple(seed2))
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveInt(rng: RNG): (Int, RNG) = {
		val (i,r) = rng.nextInt
		if(i == Int.MinValue) positiveInt(r) else (i.abs,r)
	}

  def double(rng: RNG): (Double, RNG) = {
		val (i,r) = positiveInt(rng)
		(i.toDouble / Int.MaxValue, r)
	}

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
		val (i,r1) = rng.nextInt
		val (d, r2) = double(r1)
		((i,d),r2)
	}

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
		intDouble(rng) match {
			case ((i,d),r) => ((d,i),r)
		}
	}

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
		val (d1,r1) = double(rng)
		val (d2,r2) = double(r1)
		val (d3,r3) = double(r2)
		((d1, d2, d3),r3)
	}

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
		if(count <= 0) (Nil, rng)
		else{
			val (inext, r1) = rng.nextInt
			val (ilist, rnext) = ints(count - 1)(r1)
			(inext :: ilist, rnext)
		}
	}

  def positiveMax(n: Int): Rand[Int] = map(int)(i => i % (n + 1))

	val double_elegant: Rand[Double] = map(int)(i => i.toDouble / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rnd =>
		val (a, r1) = ra(rnd)
		val (b, r2) = rb(r1)
		(f(a,b), r2)
	}

	val intDouble_elegant: Rand[(Int,Double)] = map2(int, double_elegant)((i,d) => (i,d))

	val doubleInt_elegant: Rand[(Double,Int)] = map2(int, double_elegant)((i,d) => (d,i))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
		fs match {
			case Nil => rng => (Nil, rng)
			case fhead :: frest => rng => {
				val (ahead, r1) = fhead(rng)
				val (arest, rfinal) = sequence(frest)(r1)
				(ahead :: arest, rfinal)
			}
		}		
	}

  def ints_elegant(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rnd =>
	val (a, r1) = f(rnd)
	g(a)(r1)
  }
  
  val positiveInt_viaFlatMap: Rand[Int] = flatMap(int){i =>
	if(i == Int.MinValue) int else unit(i)
  }
  
  def map_viaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)((unit[B] _).compose(f))
  
  def map2_viaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
	flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
  
}

case class State[S,+A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap((State.unit[S, B] _).compose(f))
//  State(s => {
//	val (a, s1) = run(s)
//	(f(a), s1)
//  })
  
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
	val (a, s1) = run(s)
	val (b, s2) = sb.run(s1)
	(f(a,b), s2)
  })
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
	val (a, s1) = run(s)
	f(a).run(s1)
  })
  
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  def unit[S,A](a: A):State[S, A] = State(s => (a, s))
  
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, Int] = sys.error("todo")
}
