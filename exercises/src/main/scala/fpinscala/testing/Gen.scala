package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import Status._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
	self =>
	def check: Boolean
	def &&(p: Prop): Prop = new Prop{
		def check: Boolean = self.check && p.check
	}
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

trait Status {

}

object Status {

}

object Gen {

//	type Gen[A] = State[RNG,A]

	def chooseRNG(start: Int, stopExclusive: Int) = State[RNG,Int]{r: RNG =>
		val (i,r1) = r.nextInt
		val sample: Int = start + math.abs(i % (stopExclusive - start))
		(sample, r1)
	}

	def unit[A](a: => A): Gen[A] = Gen(State.unit(a), Stream(a))

	def boolean: Gen[Boolean] = Gen(State(RNG.int).map(_ % 2 == 0), Stream(false,true))

	def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
		chooseRNG(start, stopExclusive),
		Stream.from(start).take(stopExclusive - start)
	)

	def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
		val listSample = State.sequence(List.fill(n)(g.sample))
		def getStreamOfLists(len: Int): Stream[List[A]] =
			if(len <= 0) g.exhaustive.map(_ => Nil)
			else if(len == 1) g.exhaustive.map(List(_))
			else for(a <- g.exhaustive; l <- getStreamOfLists(len - 1)) yield a :: l
		Gen(listSample, getStreamOfLists(n))
	}

}

case class Gen[+A](sample: State[RNG,A], exhaustive: Stream[A]){
	def map[A,B](f: A => B): Gen[B] = sys.error("placeholder")
	def flatMap[A,B](f: A => Gen[B]): Gen[B] = sys.error("placeholder")
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = sys.error("placeholder")
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = sys.error("placeholder")
//}

trait SGen[+A] {

}

