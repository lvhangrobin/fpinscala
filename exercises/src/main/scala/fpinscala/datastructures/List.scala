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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => sys.error("calling tail on empty List")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => sys.error("calling setHead on empty List")
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => sys.error("calling init on empty List")
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, e) => Cons(e, acc))
//  {
//    def go(lst: List[A], acc: B): B = lst match {
//      case Nil => acc
//      case Cons(h, t) => go(t, f(acc, h))
//    }
//
//    go(l, z)
//  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z) //stack function calls, amazing!

  def append2[A](list1: List[A], list2: List[A]): List[A] =
    foldRight2(list1, list2)(Cons(_, _))

  def concat[A](lsts: List[List[A]]): List[A] =
    foldLeft(lsts, List[A]())(append2)

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight2(l, List[B]())((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight2(l, List[A]()){ (h, t) =>
      if (f(h)) Cons(h, t)
      else t
    }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight2(l, List[B]())((h, t) => append2(f(h), t))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = {
    @tailrec
    def helper(l1: List[A], l2: List[B], acc: List[C]): List[C] = {
      (l1, l2) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(h1, t1), Cons(h2, t2)) => helper(t1, t2, Cons(f(h1, h2), acc))
      }
    }

    reverse(helper(a, b, List[C]()))
  }

  def startsWith[A](l: List[A], sub: List[A]): Boolean = (l, sub) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h1, t1)) if h == h1 => startsWith(t, t1)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }
}
