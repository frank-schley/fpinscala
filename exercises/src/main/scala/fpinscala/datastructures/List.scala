package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def product3(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

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

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }
  def append3[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((acc:List[A],a:A) => Cons(a, acc))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b:B, a:A) => f(a, b))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Can't call tail on an empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("Can't replace the head on an empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def setHead2[A](l: List[A], h: A): List[A] = Cons(h, List.tail(l))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Can't init on an empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = {
    def go(as: List[A], count: Int): Int = as match {
      case Nil => count
      case Cons(h, t) => go(t, count + 1)
    }
    go(l, 0)
  }

  def length2[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, t) => 1 + length(t)

  }

  def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc:Int, a) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }


  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc: List[A], a:A) => Cons(a, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a:A, acc:List[B]) => Cons(f(a), acc))


  def concat[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, List[A]())((l:List[A], as:List[A]) => append(l, as))
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List[B]())((a:A, bs:List[B]) => append(f(a), bs))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {

    def to_list_with_filter(a: A): List[A] = {
      if(f(a)) List[A](a)
      else List[A]()
    }

    flatMap(as)(to_list_with_filter)
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  }

  def startsWith[A](sub: List[A], as: List[A]): Boolean = (sub, as) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (Cons(hsub, tsub), Cons(has, tas)) => {
      if(hsub == has) startsWith(tsub, tas)
      else false
    }
  }

  def hasSubSequence[A](sub: List[A], as: List[A]): Boolean = {
    if(startsWith(sub, as)) true
    else hasSubSequence(sub, tail(as))
  }
}
