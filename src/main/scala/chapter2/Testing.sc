import chapter2.MyModule

val a = "test"

MyModule.fib(2)

3


List(1, 2, 3).foldRight(Nil:List[Int])(_ :: _)

List(1, 2, 3).foldLeft(0)(_ + _)
