package net.pragyah.scalgorithms.sorting

import junit.framework.TestCase
import junit.framework.Assert._

class TestQuickSort extends TestCase{

  def testIntSorting = {
    assert(true)
    val sorter = new QuickSort[int]()
    val input:List[int] = 10 :: 98 :: 5 :: 123 :: 3 :: 52 :: 52 :: 52 :: 34 :: 1 :: 22 :: 4 :: 6 ::Nil
    val output = sorter.sort(input)
    assert(output.length == input.length)
    input.foreach(i => {assert(input.count(_ == i) == output.count(_ == i))})
    output.foldLeft[int](output.head)((a,b) => {assert(a <= b);b})
    assert(true)
  }
  
}
