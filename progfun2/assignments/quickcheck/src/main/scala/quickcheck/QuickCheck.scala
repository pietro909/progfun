package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    lazy val genHeap: Gen[H] = for {
        element <- arbitrary[Int]
        heap <- oneOf(const(empty), genHeap)
    } yield insert(element, heap)

    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

    property("gen1") = forAll { (h: H) =>
        val m = if (isEmpty(h)) 0 else findMin(h)
        findMin(insert(m, h)) == m
    }

    property("If you insert an element into an empty heap, finding the minimum of the resulting heap should get the element.") = forAll { (a: Int) =>
        val heap = insert(a, empty)
        findMin(heap) == a
    }

    property("If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.") = forAll { (a: Int) =>
        val heap = deleteMin(insert(a, empty))
        isEmpty(heap)
    }

    property("If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.") = forAll { (a: Int, b: Int) =>
        val heap = insert(a, insert(b, empty))
        if (a < b) findMin(heap) == a
        else findMin(heap) == b
    }

    def flushHeap(heap: H, queue: List[Int] = List()): List[Int] =
        if (isEmpty(heap)) queue
        else flushHeap(deleteMin(heap), findMin(heap)::queue)

    property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.") = forAll { (h: H) =>
        val elements = flushHeap(h).reverse
        elements == elements.sorted
    }

    property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll { (h1: H, h2: H) =>
        val h3 = meld(h1, h2)
        val min3 = findMin(h3)
        min3 == findMin(h1) || min3 == findMin(h2)
    }

    property("Meld two heaps, then remove min from both and meld again: the ranks' list must equal") = forAll { (h1: H, h2: H) =>
        val h3 = meld(h1, h2)
        val h4 = meld(deleteMin(h1), insert(findMin(h1), h2))
        flushHeap(h3) == flushHeap(h4)
    }

}
