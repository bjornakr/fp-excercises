package chapter2

object MyModule {

    def isSorted[A](as: Array[A], greaterThan: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def proc(i: Int): Boolean =
            if (i+1 >= as.length)
                true
            else
                greaterThan(as(i+1), as(i)) && proc(i+1)

        proc(0)
    }


    def fib(n: Int): Int = {
        @annotation.tailrec
        def tail(cur: Int, next: Int, i: Int): Int =
        if (i == n)
            cur
        else
            tail(next, cur+next, i+1)

        tail(0, 1, 0)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) =
        a => b => f(a, b)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
        (a, b) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C =
        a => f(g(a))
}
