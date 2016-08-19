package chapter2

import org.scalatest.WordSpec

class MyModuleSpec extends WordSpec {
    "fib" when {
        "n = 0" should {
            "be 0" in {
                assert(MyModule.fib(0) == 0)
            }
        }

        "n = 1" should {
            "be 1" in {
                assert(MyModule.fib(1) == 1)
            }
        }

        "n = 2" should {
            "be 1" in {
                assert(MyModule.fib(2) == 1)
            }
        }

        "n = 3" should {
            "be 2" in {
                assert(MyModule.fib(3) == 2)
            }
        }

        "n = 4" should {
            "be 3" in {
                assert(MyModule.fib(4) == 3)
            }
        }

        "n = [5, 6, 7, 8]" should {
            "be [5, 8, 13, 21]" in {
                assert((5 to 8).map(MyModule.fib) == Seq(5, 8, 13, 21))
            }
        }
    }

    "isSorted" when {
        def intGreaterThan(a: Int, b: Int) = a > b
        def stringGreaterThan(a: String, b: String) = a > b
        //def greaterThan[A <: Ordered](a: A, b: A): Int = a.compare(b)

        "empty" should {
            "be sorted" in {
                assert(MyModule.isSorted(Array(), intGreaterThan))
            }
        }

        "one element" should {
            "be sorted" in {
                assert(MyModule.isSorted(Array(1), intGreaterThan))
            }
        }

        "two sorted elements" should {
            "be sorted" in {
                assert(MyModule.isSorted(Array("A", "B"), stringGreaterThan))
            }
        }

        "two unsorted elements" should {
            "not be sorted" in {
                assert(!MyModule.isSorted(Array(2, 1), intGreaterThan))
            }
        }

        "multiple sorted elements" should {
            "be sorted" in {
                assert(MyModule.isSorted((1 to 10).toArray, intGreaterThan))
            }
        }

        "multiple unsorted elements" should {
            "not be sorted" in {
                assert(!MyModule.isSorted(Array(1, 2, 4, 3, 5), intGreaterThan))
            }
        }
    }

    "curry" when {
        "some function" should {
            "return curried function" in {
                def f(x: Int, y: Int) = x + y
                val g = MyModule.curry(f)
                assert(g(1)(2) == 3)
            }
        }
    }

    "uncurry" when {
        "some curried function" should {
            "return uncurried function" in {
                def f(x: Int)(y: Int) = x + y
                val g = MyModule.uncurry(f)
                assert(g(1, 2) == 3)
            }
        }
    }

    "compose" when {
        "two functions" should {
            "return composed function" in {
                def f(x: Int) = x + 1
                def g(x: Int) = x * 10
                val h = MyModule.compose(g, f)
                assert(h(1) == 20)
            }
        }
    }
}
