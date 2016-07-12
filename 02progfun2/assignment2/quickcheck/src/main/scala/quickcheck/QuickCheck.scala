package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbInt.arbitrary
    m <- oneOf(const(empty), genHeap)
  } yield insert(x, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  private def isSorted(h: H): Boolean = {
    def isSorted(sorted: Boolean, lastElement: Int, heap: H): Boolean =
      if (!sorted || isEmpty(heap)) sorted
      else {
        val currentMin = findMin(heap)
        isSorted(currentMin >= lastElement, currentMin, deleteMin(heap))
      }

    isSorted(sorted = true, Int.MinValue, h)
  }

  private def containsAll(heap: H, subset: H): Boolean = {
    if (isEmpty(subset)) true
    else if (isEmpty(heap)) false
    else if (findMin(heap) == findMin(subset)) containsAll(deleteMin(heap), deleteMin(subset))
    else containsAll(deleteMin(heap), subset)
  }

  property("Adding an element to an empty heap should result in a heap containing that one element at its min position") = forAll { (value: Int) =>
    val heap = insert(value, empty)
    !isEmpty(heap)
    findMin(heap) == value
  }

  property("Adding a minimum value to a heap should result in a heap containing the value at its Min position") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("Adding two values to an empty heap, findMin should return the min value between the two") = forAll { (val1: Int, val2: Int) =>
    findMin(insert(val1, insert(val2, empty))) == Math.min(val1, val2)
  }

  property("Adding an element to an empty heap, and then deleting it should result an empty heap") = forAll { (element: Int) =>
    val h = insert(element, empty)
    findMin(h) == element
    isEmpty(deleteMin(h))
  }

  property("Given any heap, the elements inside the heap should be sorted") = forAll { (h: H) =>
    isSorted(h)
  }

  property("Melding two heaps should result in one heap containing all elements") = forAll { (h1: H, h2: H) =>
    containsAll(meld(h1, h2), h1)
    containsAll(meld(h1, h2), h2)
    containsAll(meld(h2, h1), h1)
    containsAll(meld(h2, h1), h2)
  }

  property("Given a melding of any two heaps, the elements inside the resulting heap should be sorted") = forAll { (h1: H, h2: H) =>
    isSorted(meld(h1, h2))
  }

  property("The minimum value after melding two heaps should be the minimum value of either the one or the other") = forAll { (h1: H, h2: H) =>
    val expected = Math.min(findMin(h1), findMin(h2))
    findMin(meld(h1, h2)) == expected
    findMin(meld(h2, h1)) == expected
  }
}
