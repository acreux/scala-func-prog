
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = {
     l match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, tail) => Cons(h, tail)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, tail) if f(h) => dropWhile(tail, f)
      case _ => _
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(h, Nil) => Nil
      case Nil => Nil
      case Cons(h, tail) => Cons(h, init(tail))
    }
  }


}
