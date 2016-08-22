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
}
