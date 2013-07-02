package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
//import Status._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (TestCases,RNG) => Result){
	
	def &&(p: Prop): Prop = Prop( (tcases, rng) => {
		this.run(tcases,rng) match{
			case f @ Left(failCase1) => p.run(tcases, rng) match{
				case Left(failCase2) => Left(failCase1 + ", " + failCase2)
				case _ => f 
			}
			case Right((stat1, count1)) => p.run(tcases, rng) match{
				case f @ Left(failCase) => f
				case Right((stat2, count2)) => Right((stat1 && stat2, count1 + count2)) 
			}
		}
	})
	
	def ||(p: Prop): Prop = Prop( (tcases, rng) => {
		this.run(tcases,rng) match{
			case Left(failCase1) => p.run(tcases, rng) match{
				case Left(failCase2) => Left(failCase1 + ", " + failCase2)
				case r @ Right(_) => r 
			}
			case r @ Right((stat1, count1)) => p.run(tcases, rng) match{
				case Left(_) => r
				case Right((stat2, count2)) => Right((stat1 || stat2, count1 + count2)) 
			}
		}
	})
}

object Prop {
	type FailedCase = String
	type SuccessCount = Int
	type TestCases = Int
	type Result = Either[FailedCase, (Status,SuccessCount)]
	
	def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

sealed trait Status{
	def &&(s: Status): Status = (this, s) match{
		case (Proven, Proven) => Proven
		case _ => Unfalsified
	}
	def ||(s: Status): Status = (this, s) match{
		case (Unfalsified, Unfalsified) => Unfalsified
		case _ => Proven
	}
}
case object Proven extends Status
case object Unfalsified extends Status

object Gen {

//	type Gen[A] = State[RNG,A]

	def chooseRNG(start: Int, stopExclusive: Int) = State[RNG,Int]{r: RNG =>
		val (i, r1) = r.nextInt
		val sample: Int = start + math.abs(i % (stopExclusive - start))
		(sample, r1)
	}

	def unit[A](a: => A): Gen[A] = Gen(State.unit(a), Stream(Some(a)))

	def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0), Stream(Some(false),Some(true)))

	def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
		chooseRNG(start, stopExclusive),
		Stream.from(start).take(stopExclusive - start).map(Some(_))
	)

	def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
		val listSample = State.sequence(List.fill(n)(g.sample))
		
		def getStreamOfLists(len: Int): Stream[Option[List[A]]] =
			if(len <= 0) g.exhaustive.map(_ => Some(Nil))
			else if(len == 1) g.exhaustive.map(intOp => intOp.map(List(_)))
			else for(
			    aopt <- g.exhaustive;
				lopt <- getStreamOfLists(len - 1)
			) yield for(a <- aopt; l <- lopt) yield a :: l
			
		Gen(listSample, getStreamOfLists(n))
	}
	
	def listOfN_viaPrimitives[A](n: Int, g: Gen[A]): Gen[List[A]] =	sequence(List.fill(n)(g))
	
	def sequence[A](seq: List[Gen[A]]): Gen[List[A]] = seq match {
		case Nil => unit(Nil)
		case h :: tail => h.flatMap(a => sequence(tail).map(l => a :: l))
	}
	
	def uniform: Gen[Double] = Gen(
	    chooseRNG(0, Int.MaxValue).map(_.asInstanceOf[Double] / Int.MaxValue),
	    Stream(None)
	)

	def choose(i: Double, j: Double): Gen[Double] = uniform.map(u => i + (j - i) * u)

	def map2[A,B,C](g1: Gen[A], g2: Gen[B], f: (A,B) => C): Gen[C] = {
		val exhaustive = for(o1 <- g1.exhaustive; o2 <- g2.exhaustive)
							yield for(a <- o1; b <- o2)
								yield f(a,b)
		val sample = for(a <- g1.sample; b <- g2.sample)
						yield f(a,b)
		Gen(sample, exhaustive)
	}
	
	def sameParity(from: Int, to: Int): Gen[(Int,Int)] = choose(from,to).flatMap{a =>
		choose(from, to).map( b =>
			if( (a + b) % 2 == 0) (a, b)
			else if(b - 1 >= from) (a, b - 1)
			else if(b + 1 < to) (a, b + 1)
			else throw new Exception(s"Cannot locate a same-parity pair between $from and $to")
		)
	}
	
	def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
	  
		val pick: State[RNG, Boolean] = State(r => {val (i, r1) = r.nextInt; (i % 2 == 0, r)})
	  
		def mixStream[T](stream1: Stream[T], stream2: Stream[T], r: RNG): Stream[T] = new Stream[T]{
		  	val (b, r1) = pick.run(r)
			def uncons: Option[(T, Stream[T])] = for(
			    (a1, s1) <- stream1.uncons;
			    (a2, s2) <- stream2.uncons
			) yield if(b) (a1, mixStream(s1, stream2, r1)) else (a2, mixStream(stream1, s2, r1))
		}
		
		val sample = pick.flatMap(b => if(b) g1.sample else g2.sample)
		val exhaustive = mixStream(g1.exhaustive, g2.exhaustive, RNG.simple(42))
		
		Gen(sample, exhaustive)
	}

}

case class Gen[+A](sample: State[RNG,A], exhaustive: Stream[Option[A]]){
	
	def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))
	
	def flatMap[B](f: A => Gen[B]): Gen[B] = {
		val bSample = sample.flatMap(a => f(a).sample)
		val bExhaustive = exhaustive.flatMap[Option[B]]{aopt => aopt match {
				case None => Stream(None)
				case Some(a) => f(a).exhaustive
			}
		}
		Gen(bSample, bExhaustive)
	}
	
	def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOfN(n, this))
	
	def unsized: SGen[A] = SGen(size => 
		Gen(sample, exhaustive.take(size))
	)
}

case class SGen[+A](forSize: Int => Gen[A]){
	def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))
	def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))
}

