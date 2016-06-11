sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
   def sum(ints: List[Int]): Int = ints match {
      case Nil          => 0
      case Cons(x, xs)  => x + sum(xs)
   }
   def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(x, xs)  => x * product(xs)
   }

   def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

   def product2(ns: List[Double]) = foldRight(ns, 1.0)((x, y) => x * y)

   def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x,foldRight(xs, z)(f))
   }

   def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
   }

   def length2[A](as:List[A]): Int = as match {
        case Nil => 0
        case Cons(x, xs) => 1 + length2(xs)
    }
   def length(as:List[_]): Int = foldRight(as, 0)((_, y) => 1 + y)


   def apply[A](as: A*) : List[A] =
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
   
    def tail[A](l:List[A]): List[A] = l match {
        case Cons(_,xs) => xs
        case Nil => Nil
   }
   def setHead[A](l:List[A], e:A): List[A] = l match {
        case Cons(_,xs) => Cons(e,xs)
        case Nil => Nil
   }

  def drop4[A](l: List[A], n: Int): List[A] = {
     if (n <= 0) l
     else if (l == Nil) Nil
     else List.drop4(tail(l), n-1)
  }

   def drop2[A](l:List[A], n:Int): List[A] = l match {
        case _ if n == 0 => l
        case Cons(_, x) => List.drop2(x, n-1)

   }

   def dropWhile[A](l:List[A])(f: A => Boolean): List[A] = l match {
       case Cons(x, xs) if f(x) => dropWhile(xs)(f)
       case _ => l
   }

   def init[A](l: List[A]): List[A] = l match {
       case Cons(_, Nil) => Nil
       case Cons(x,xs) => Cons(x, List.init(xs))
       case Nil => Nil
   }





}