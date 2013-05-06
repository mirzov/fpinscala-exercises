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

	type Gen[A] = State[RNG,A]

	def choose(start: Int, stopExclusive: Int): Gen[Int] = State[RNG,Int]{r: RNG =>
		val (i,r1) = r.nextInt
		val sample: Int = start + math.abs(i % (stopExclusive - start))
		(sample, r1)
	}

}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

