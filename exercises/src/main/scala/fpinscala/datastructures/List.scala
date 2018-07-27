package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => List(h)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil                  => Nil
    case Cons(_, xs) if n > 0 => drop(xs, n - 1)
    case _                    => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _                   => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, xs) if xs != Nil => Cons(x, init(xs))
    case _                        => Nil
  }

  def length[A](l: List[A]): Int = l match {
    case Nil         => 0
    case Cons(x, xs) => 1 + length(xs)
  }

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ints: List[Int]) = {
    foldLeft(ints, 0)(_ + _)
  }

  def product3(ints: List[Int]) = {
    foldLeft(ints, 1)(_ * _)
  }

  def length3[A](l: List[A]) = {
    foldLeft(l, 0) { case (acc, _) => acc + 1 }
  }

  def reverse[A](l: List[A]) = {
    foldLeft(l, Nil: List[A]) { case (acc, x) => Cons(x, acc) }
  }

  def foldLeftWithFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???
  def foldRightWithFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = ???

  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def addOne(l: List[Int]) = foldRight(l, Nil: List[Int]) {
    case (x, acc) => Cons(x + 1, acc)
  }

  def doublesToString(l: List[Double]) = foldRight(l, Nil: List[String]) {
    case (x, acc) => Cons(x.toString, acc)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B]) {
    case (a, acc) => Cons(f(a), acc)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A]) {
      case (a, acc) =>
        if (f(a)) Cons(a, acc) else acc
    }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addInts[Int](as: List[Int], bs: List[Int]): List[Int] =

}
