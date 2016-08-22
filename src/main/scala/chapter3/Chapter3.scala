package chapter3

object Chapter3 {
    def tail[A](list: List[A]): Option[List[A]] =
        list match {
            case Nil => None
            case _ :: xs => Some(xs)
        }

    def setHead[A](a: A, list: List[A]): Option[List[A]] =
        list match {
            case Nil => None
            case _ :: xs => Some(a :: xs)
        }

    def drop[A](list: List[A], n: Int): List[A] =
        if (n <= 0)
            list
        else
            list match {
                case Nil => Nil
                case _ :: xs => drop(xs, n - 1)
            }

    def dropWhile[A](list: List[A])(f: A => Boolean): List[A] =
        list match {
            case Nil => Nil
            case x :: xs =>
                if (f(x))
                    dropWhile(xs)(f)
                else
                    list
        }

    def init[A](l: List[A]): List[A] =
        l match {
            case Nil => Nil
            case x :: Nil => Nil
            case x :: xs => x :: init(xs)
        }

    def length[A](as: List[A]): Int =
        as.foldRight(0)((_, acc) => 1 + acc)

    /// def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B

    @annotation.tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case x :: xs => foldLeft(xs, f(z, x))(f)
        }

    def sumL(l: List[Int]): Int =
        foldLeft(l, 0)(_ + _)

    def productL(l: List[Int]): Int =
        foldLeft(l, 1)(_ * _)

    def lengthL[A](as: List[A]): Int =
        foldLeft(as, 0)((acc, _) => acc + 1)

    def reverse[A](as: List[A]) =
        foldLeft(as, Nil: List[A])((b, a) => a :: b)

    @annotation.tailrec
    def foldLeftViaRight[A,B](as: List[A], z: B)(f: (B, A) => B): B =
        

}
