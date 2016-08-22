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
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
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

    // 3.13
    // @annotation.tailrec
    def foldLeftViaRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

    // 3.14
    def appendL[A](l1: List[A], l2: List[A]): List[A] =
        foldLeft(l1.reverse, l2)((b, a) => a :: b)

    def appendR[A](l1: List[A], l2: List[A]): List[A] =
        l1.foldRight(l2)((a, b) => a :: b)

    // 3.15
    def flatten[A](ll: List[List[A]]): List[A] =
        ll.foldRight(Nil: List[A])((a, b) => appendR(a, b))

    // 3.16
    def addOne(is: List[Int]): List[Int] =
        is.foldRight(Nil: List[Int])((a, b) => (a + 1) :: b)

    // 3.17
    def doublesToString(ds: List[Double]): List[String] =
        ds.foldRight(Nil: List[String])((a, b) => a.toString :: b)

    // 3.18
    def map[A, B](as: List[A])(f: A => B) =
        as.foldRight(Nil: List[B])((a, b) => f(a) :: b)

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
        as.foldRight(Nil: List[A])((a, b) => if (f(a)) a :: b else b)

    // 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
        as.foldRight(Nil: List[B])((a, b) => f(a) ::: b)

    // 3.21
    def filterFm[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)(a => if (f(a)) List(a) else Nil)

    // 3.22
    def addIntLists(xs: List[Int], ys: List[Int]): List[Int] =
        xs match {
            case Nil => Nil
            case a :: as => ys match {
                case Nil => Nil
                case b :: bs => (a + b) :: addIntLists(as, bs)
            }
        }

    // 3.23
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
        (as, bs) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
        }


    // 3.24
    @annotation.tailrec
    def startsWith[A](as: List[A], prefix: List[A]): Boolean =
        (as, prefix) match {
            case (_, Nil) => true
            case (x :: xs, y :: ys) if x == y => startsWith(xs, ys)
            case _ => false
        }

    def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {

        @annotation.tailrec
        def proc[A](as: List[A], sub: List[A], sequenceHasStarted: Boolean): Boolean =
            (as, sub) match {
                case (Nil, _ :: _) => false
                case (_, Nil) => true
                case (x :: xs, s@(y :: ys)) =>
                    if (x == y) proc(xs, ys, true)
                    else if (!sequenceHasStarted)
                        proc(xs, s, false)
                    else false
            }

        proc(as, sub, false)
    }
}