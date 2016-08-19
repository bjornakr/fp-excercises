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
        val all = (a: Any) => true

        "drop all" when {
            "[]" should {
                "be []" in {
                    assert(Chapter3.dropWhile(List(), all) == List())
                }
            }

            "[a, b, c]" should {
                "be []" in {
                    assert(Chapter3.dropWhile(List(1, 2, 3), all) == List())
                }
            }
        }

        val none = (a: Any) => false

        "drop none" when {
            "[]" should {
                "be []" in {
                    assert(Chapter3.dropWhile(List(), none) == List())
                }
            }

            "[a, b, c]" should {
                "be [a, b, c]" in {
                    assert(Chapter3.dropWhile(List(1, 2, 3), none) == List(1, 2, 3))
                }
            }
        }

        val even = (x: Int) => x % 2 == 0

        "drop even from [1, 2, 3, 4, 5]" should {
            "be [1, 3, 5]" in {
                assert(Chapter3.dropWhile(List(1, 2, 3, 4, 5), even) == List(1, 3, 5))
            }
        }
    }
}
