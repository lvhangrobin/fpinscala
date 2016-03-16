package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(acc: List[A], stream: Stream[A]): List[A] = this match {
      case Cons(h, t) => go(acc :+ h(), t())
      case _ => acc
    }

    go(List[A](), this)
  }

  def take(n: Int): Stream[A] = this match{
    case Cons(h, t) if n >= 1 => cons(h(), t().take(n-1))
    case _ => Empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold(this -> n){
    case (Cons(h, t), n) if n >= 1 => Some(h() -> (t(), (n - 1)))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => t().drop(n - 1)
    case Cons(h, t) => this
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A]){case (a, acc) => if (p(a)) cons(a, acc) else empty }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h,t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if(f(a)) cons(a, acc) else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))
  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold(this -> s2){
    case (Cons(h, t), Cons(h1, t1)) => Some(f(h(), h1()), t() -> t1())
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> s2){
    case (Empty, Empty) => None
    case (Cons(h, t), Cons(h1, t1)) => Some(Some(h()) -> Some(h1()), t() -> t1())
    case (Cons(h, t), Empty) => Some(Some(h()) -> None, t() -> empty)
    case (Empty, Cons(h1, t1)) => Some(None -> Some(h1()), empty -> t1())
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile(_._2.isDefined)
      .forAll { case (s1, s2) => s1 == s2}

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h, t) => Some(this, t())
    case _ => None
  } append empty[Stream[A]]

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(z -> empty[B]){ case (a, (ac, acc)) =>
      lazy val ac1 = ac
      lazy val acc1 = acc
      (f(a, ac1) -> cons(f(a, ac1), acc1))
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val as: Stream[A] = Cons(() => a, () => as)
    as
  }

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  val fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  val fibViaUnfold: Stream[Int] = unfold((0,1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(n, n+1))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty[A]
  }
}