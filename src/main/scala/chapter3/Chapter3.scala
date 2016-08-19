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

    def dropWhile[A](list: List[A], f: A => Boolean): List[A] =
        list match {
            case Nil => Nil
            case x :: xs =>
                if (f(x))
                    dropWhile(xs, f)
                else
                    x :: dropWhile(xs, f)
//                if (f(x))
//                    Nil // list //x :: dropWhile(xs, f)
//            }
        }

}
