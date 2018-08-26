package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](ta: Tree[A]): Int = ta match {
    case Leaf(a) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](ta: Tree[A])(f: A => B): Tree[B] = ta match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](ta: Tree[A])(f: A => B, merge: (B, B) => B): B = ta match {
    case Leaf(a) => f(a)
    case Branch(left, right) => merge(fold(left)(f, merge), fold(right)(f, merge))
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    val size = (a:A) => 1
    val merge = (leftSize:Int, rightSize:Int) => 1 + leftSize + rightSize

    fold(t)(size, merge)
  }

  def depthViaFold[A](t: Tree[A]): Int = {
    val depth = (a: A) => 1
    val merge = (dl: Int, dr: Int) => dl max dr

    fold(t)(depth, merge)
  }

  def mapViaFold[A, B](ta: Tree[A])(f: A => B): Tree[B] = {

    def transform(a: A): Leaf[B] = Leaf(f(a))

    def merge(left: Tree[B], right: Tree[B]): Tree[B] = {
      Branch(left, right)
    }

    fold(ta)(transform, merge)
  }
}