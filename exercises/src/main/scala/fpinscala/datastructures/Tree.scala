package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](tree: Tree[A]): Int = tree match{
		case Leaf(_) => 1
		case Branch(left, right) => 1 + size(left) + size(right)
	}

	def maximum(tree: Tree[Int]): Int = tree match{
		case Leaf(v) => v
		case Branch(left, right) => maximum(left) max maximum(right)
	}

	def depth[A](tree: Tree[A]): Int = tree match{
		case Leaf(_) => 0
		case Branch(left, right) => 1 + depth(left).max(depth(right))
	}

	def map[A,B](tree: Tree[A], f: A => B): Tree[B] = tree match{
		case Leaf(v) => Leaf(f(v))
		case Branch(left, right) => Branch(map(left,f), map(right,f))
	}

	def fold[A,B](tree: Tree[A], seed: A => B, join: (B,B) => B): B = tree match{
		case Leaf(v) => seed(v)
		case Branch(left, right) => join(fold(left, seed, join), fold(right, seed, join))
	}

	def sizeFold[A](tree: Tree[A]): Int = fold[A,Int](tree, _ => 1, 1 + _ + _)

	def maximumFold(tree: Tree[Int]): Int = fold[Int,Int](tree, v => v, _.max(_))

	def depthFold[A](tree: Tree[A]): Int = fold[A,Int](tree, _ => 0, 1 + _.max(_))

	def mapFold[A,B](tree: Tree[A], f: A => B): Tree[B] = fold[A,Tree[B]](tree, v => Leaf(f(v)), Branch(_,_))
}
