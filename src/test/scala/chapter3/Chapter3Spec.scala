package chapter3

import org.scalatest.WordSpec

class Chapter3Spec extends WordSpec {
    "tail" when {
        "[]" should {
            "be None" in {
                assert(Chapter3.tail(List()) == None)
            }
        }

        "[x]" should {
            "be Some []" in {
                assert(Chapter3.tail(List(1)) == Some(List()))
            }
        }

        "[1, 2]" should {
            "be Some [2]" in {
                assert(Chapter3.tail(List(1, 2)) == Some(List(2)))
            }
        }

        "[1, 2, 3, 4, 5]" should {
            "be Some [2, 3, 4, 5]" in {
                assert(Chapter3.tail(List(1, 2, 3, 4, 5)) == Some(List(2, 3, 4, 5)))
            }
        }
    }

    "setHead" when {
        "[]" should {
            "be None" in {
                assert(Chapter3.setHead(1, List()) == None)
            }
        }

        "new head = 2, list = [1]" should {
            "be Some [2]" in {
                assert(Chapter3.setHead(2, List(1)) == Some(List(2)))
            }
        }

        "new head = B, list = [A, C, D, E]" should {
            "be Some [B, C, D, E]" in {
                assert(Chapter3.setHead("B", List("A", "C", "D", "E")) == Some(List("B", "C", "D", "E")))
            }
        }
    }

    "drop" when {
        "[]" when {
            "drop 1" should {
                "be []" in {
                    assert(Chapter3.drop(List(), 1) == List())
                }
            }

            "drop 5" should {
                "be []" in {
                    assert(Chapter3.drop(List(), 5) == List())
                }
            }
        }

        "[x]" when {
            "drop 1" should {
                "be []" in {
                    assert(Chapter3.drop(List("A"), 1) == List())
                }
            }

            "drop 5" should {
                "be []" in {
                    assert(Chapter3.drop(List("A"), 5) == List())
                }
            }
        }

        "[x, y]" when {
            "drop 1" should {
                "be [y]" in {
                    assert(Chapter3.drop(List("A", "B"), 1) == List("B"))
                }
            }
        }


        "[x, y, z]" when {

            "drop 1" should {
                "be [y, z]" in {
                    assert(Chapter3.drop(List("A", "B", "C"), 1) == List("B", "C"))
                }
            }

            "drop 2" should {
                "be [z]" in {
                    assert(Chapter3.drop(List("A", "B", "C"), 2) == List("C"))
                }
            }

            "drop 3" should {
                "be []" in {
                    assert(Chapter3.drop(List("A", "B", "C"), 3) == List())
                }
            }
        }
    }

    "dropWhile" when {

        "drop all" when {
            "[]" should {
                "be []" in {
                    assert(Chapter3.dropWhile(List())(_ => true) == List())
                }
            }

            "[a, b, c]" should {
                "be []" in {
                    assert(Chapter3.dropWhile(List(1, 2, 3))(_ => true) == List())
                }
            }
        }

        "drop none" when {
            "[]" should {
                "be []" in {
                    assert(Chapter3.dropWhile(List())(_ => false) == List())
                }
            }

            "[a, b, c]" should {
                "be [a, b, c]" in {
                    assert(Chapter3.dropWhile(List(1, 2, 3))(_ => false) == List(1, 2, 3))
                }
            }
        }

        "drop even from [2, 4, 6, 7, 8, 9]" should {
            "be [7, 8, 9]" in {
                assert(Chapter3.dropWhile(List(2, 4, 6, 7, 8, 9))(_ % 2 == 0) == List(7, 8, 9))
            }
        }

        "drop x < 4 from [1, 2, 3, 4, 5, 6]" should {
            "be [4, 5, 6]" in {
                assert(Chapter3.dropWhile(List(1, 2, 3, 4, 5, 6))(_ < 4) == List(4, 5, 6))
            }
        }
    }

    "init" when {
        "[]" should {
            "be []" in {
                assert(Chapter3.init(List()) == List())
            }
        }

        "[x]" should {
            "be []" in {
                assert(Chapter3.init(List(1)) == List())
            }
        }

        "[a, b]" should {
            "be [a]" in {
                assert(Chapter3.init(List(1, 2)) == List(1))
            }
        }

        "[a, b, c, d]" should {
            "be [a, b, c]" in {
                assert(Chapter3.init(List(1, 2, 3, 4)) == List(1, 2, 3))
            }
        }
    }

    "length" when {
        "[]" should {
            "be 0" in {
                assert(Chapter3.length(List()) == 0)
            }
        }

        "[x]" should {
            "be 1" in {
                assert(Chapter3.length(List(1)) == 1)
            }
        }

        "[a, b, c, d, e]" should {
            "be 5" in {
                assert(Chapter3.length(List(1, 2, 3, 4, 5)) == 5)
            }
        }
    }

    "sumL" when {
        "[1..10]" should {
            "be 55" in {
                assert(Chapter3.sumL((1 to 10).toList) == 55)
            }
        }
    }

    "productL" when {
        "[]" should {
            "be 1" in {
                assert(Chapter3.productL(List()) == 1)
            }
        }

        "[1..5]" should {
            "be 120" in {
                assert(Chapter3.productL((1 to 5).toList) == 120)
            }
        }
    }

    "lengthL" when {
        "[1..10]" should {
            "be 10" in {
                assert(Chapter3.lengthL((1 to 10).toList) == 10)
            }
        }
    }

    "reverse" when {
        "[]" should {
            "be []" in {
                assert(Chapter3.reverse(List()) == List())
            }
        }

        "[1, 2, 3]" should {
            "be [3, 2, 1]" in {
                assert(Chapter3.reverse(List(1, 2, 3)) == List(3, 2, 1))
            }
        }
    }

    "appendl" when {
        "first = []" when {
            "second = []" should {
                "be []" in {
                    assert(Chapter3.appendL(List(), List()) == List())
                }
            }

            "second = [4, 5, 6]" should {
                "be [4, 5, 6]" in {
                    assert(Chapter3.appendL(List(), List(4, 5, 6)) == List(4, 5, 6))
                }
            }
        }

        "first = [1, 2, 3]" when {
            "second = []" should {
                "be [1, 2, 3]" in {
                    assert(Chapter3.appendL(List(1, 2, 3), List()) == List(1, 2, 3))
                }
            }

            "second = [4, 5, 6]" should {
                "be [1, 2, 3, 4, 5, 6]" in {
                    assert(Chapter3.appendL(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
                }
            }
        }
    }

    "appendR" when {
        "first = []" when {
            "second = []" should {
                "be []" in {
                    assert(Chapter3.appendR(List(), List()) == List())
                }
            }

            "second = [4, 5, 6]" should {
                "be [4, 5, 6]" in {
                    assert(Chapter3.appendR(List(), List(4, 5, 6)) == List(4, 5, 6))
                }
            }
        }

        "first = [1, 2, 3]" when {
            "second = []" should {
                "be [1, 2, 3]" in {
                    assert(Chapter3.appendR(List(1, 2, 3), List()) == List(1, 2, 3))
                }
            }

            "second = [4, 5, 6]" should {
                "be [1, 2, 3, 4, 5, 6]" in {
                    assert(Chapter3.appendR(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
                }
            }
        }
    }

    "flatten" when {
        "[]" should {
            "be []" in {
                assert(Chapter3.flatten(List()) == List())
            }
        }

        "[[]]" should {
            "be []" in {
                assert(Chapter3.flatten(List(List())) == List())
            }
        }

        "[[x]]" should {
            "be [x]" in {
                assert(Chapter3.flatten(List(List(1))) == List(1))
            }
        }

        "[[x], []]" should {
            "be [x]" in {
                assert(Chapter3.flatten(List(List(1), List())) == List(1))
            }
        }

        "[[a, b], [c, d]]" should {
            "be [a, b, c, d]" in {
                assert(Chapter3.flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
            }
        }
    }

    "addOne" when {
        "[]" should {
            "be []" in {
                assert(Chapter3.addOne(List()) == List())
            }
        }

        "[1]" should {
            "be [2]" in {
                assert(Chapter3.addOne(List(1)) == List(2))
            }
        }

        "[2, 3, 4]" should {
            "be [3, 4, 5]" in {
                assert(Chapter3.addOne(List(2, 3, 4)) == List(3, 4, 5))
            }
        }
    }

    "doublesToString" when {
        "[]" should {
            "be []" in {
                assert(Chapter3.doublesToString(List()) == List())
            }
        }

        "[x]" should {
            "be ['x']" in {
                assert(Chapter3.doublesToString(List(1.0)) == List("1.0"))
            }
        }

        "[x, y]" should {
            "be ['x', 'y']" in {
                assert(Chapter3.doublesToString(List(1.0, 2.5)) == List("1.0", "2.5"))
            }
        }
    }

    "map" when {
        "mapping function f(x) = x + 1" when {
            def plusOne(x: Int) = x + 1

            "[]" should {
                "be []" in {
                    assert(Chapter3.map(List())(plusOne) == List())
                }
            }

            "[1, 2, 3]" should {
                "be [2, 3, 4]" in {
                    assert(Chapter3.map(List(1, 2, 3))(plusOne) == List(2, 3, 4))
                }
            }
        }
    }

    "filter" when {
        "filter is even numbers" should {
            def isEven(x: Int) = x % 2 == 0

            "[]" should {
                "be []" in {
                    assert(Chapter3.filter(List())(isEven) == List())
                }
            }

            "[1, 2, 3, 4]" should {
                "be [2, 4]" in {
                    assert(Chapter3.filter(List(1, 2, 3, 4))(isEven) == List(2, 4))
                }
            }
        }
    }

    "flatMap" when {
        "function is f(x) = [x, x]" when {
            def dup[A](a: A) = List(a, a)

            "[]" should {
                "be []" in {
                    assert(Chapter3.flatMap(List())(dup) == List())
                }
            }

            "[a, b, c]" should {
                "be [a, a, b, b, c, c]" in {
                    assert(Chapter3.flatMap(List(1, 2, 3))(dup) == List(1, 1, 2, 2, 3, 3))
                }
            }
        }

        "function is f(x) = [x + 1]" when {
            def addOne(x: Int) = List(x + 1)

            "[1, 2, 3]" should {
                "be [2, 3, 4]" in {
                    assert(Chapter3.flatMap(List(1, 2, 3))(addOne) == List(2, 3, 4))
                }
            }
        }
    }

    "filterFm" when {
        "filter is even numbers" should {
            def isEven(x: Int) = x % 2 == 0

            "[]" should {
                "be []" in {
                    assert(Chapter3.filterFm(List())(isEven) == List())
                }
            }

            "[1, 2, 3, 4]" should {
                "be [2, 4]" in {
                    assert(Chapter3.filterFm(List(1, 2, 3, 4))(isEven) == List(2, 4))
                }
            }
        }
    }

    "addIntLists" when {
        "left = [1, 2, 3], right = [1, 2, 3]" should {
            "be [2, 4, 6]" in {
                assert(Chapter3.addIntLists(List(1, 2, 3), List(1, 2, 3)) == List(2, 4, 6))
            }
        }
    }

    "zipWith" when {
        "function is f(x, y) = (x, y)" when {
            def tuplify(a: Any, b: Any) = (a, b)
            "left = []" when {
                "right = []" should {
                    "be []" in {
                        assert(Chapter3.zipWith(List(), List())(tuplify) == List())
                    }
                }

                "right = [1, 2, 3]" should {
                    "be []" in {
                        assert(Chapter3.zipWith(List(), List(1, 2, 3))(tuplify) == List())
                    }
                }
            }

            "left = [1, 2, 3]" when {
                "right = []" should {
                    "be []" in {
                        assert(Chapter3.zipWith(List(1, 2, 3), List())(tuplify) == List())
                    }
                }
            }

            "left = [1, 2, 3]" when {
                "right = ['a', 'b', 'c']" should {
                    "be [(1, 'a'), (2, 'b'), (3, 'c')]" in {
                        assert(Chapter3.zipWith(List(1, 2, 3), List("a", "b", "c"))(tuplify) == List((1, "a"), (2, "b"), (3, "c")))
                    }
                }
            }
        }
    }

    "hasSubsequence" when {
        "source = []" when {
            "sub = []" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(List(), List()))
                }
            }

            "sub = [x]"  should {
                "be false" in {
                    assert(!Chapter3.hasSubsequence(List(), List(1)))
                }
            }
        }

        "source = [x]" when {
            "sub = []" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(List(1), List()))
                }
            }

            "sub = [x]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(List(1), List(1)))
                }
            }

            "sub = [y]" should {
                "be false" in {
                    assert(!Chapter3.hasSubsequence(List(1), List(2)))
                }
            }
        }

        "source = [a, b, c]" when {
            val source = List(1, 2, 3)
            "sub = [a, b, c]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(source, source))
                }
            }

            "sub = [b, c]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(source, List(2, 3)))
                }
            }

            "sub = [a, b]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(source, List(1, 2)))
                }
            }

            "sub = [a]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(source, List(1)))
                }
            }

            "sub = [b]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(source, List(2)))
                }
            }

            "sub = [c]" should {
                "be true" in {
                    assert(Chapter3.hasSubsequence(source, List(3)))
                }
            }

            "sub = [a, c]" should {
                "be false" in {
                    assert(!Chapter3.hasSubsequence(source, List(1, 3)))
                }
            }

            "sub = [a, b, c, d]" should {
                "be false" in {
                    assert(!Chapter3.hasSubsequence(source, List(1, 2, 3, 4)))
                }
            }
        }
    }
}
