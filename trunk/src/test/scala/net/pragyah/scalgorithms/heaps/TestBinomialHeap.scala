package net.pragyah.scalgorithms.heaps

import junit.framework.TestCase
import junit.framework.Assert._

class TestBinomialHeap extends TestCase{
  
  def testExtractMinInt = {
	val heap = new BinomialHeap[int](Int.MinValue);
    
     heap ++= (10 :: 98 :: 5 :: 123 :: 3 :: 52 :: 52 :: 52 :: 34 :: 1 :: 22 :: 4 :: 6 ::Nil)
     assert(heap.minimum.get == 1)
     assert(heap.extractMin.get == 1)
     assert(heap.extractMin.get == 3)
     assert(heap.extractMin.get == 4)
     assert(heap.extractMin.get == 5)
     assert(heap.extractMin.get == 6)
     assert(heap.extractMin.get == 10)
     assert(heap.extractMin.get == 22)
     assert(heap.extractMin.get == 34)
     assert(heap.extractMin.get == 52)
     assert(heap.extractMin.get == 52)
     assert(heap.extractMin.get == 52)
     assert(heap.extractMin.get == 98)
     assert(heap.extractMin.get == 123)
     assert(heap.extractMin == None)
     assert(heap.extractMin == None)
     
  }
  
   def testDecreaseKeyInt = {
	val heap = new BinomialHeap[int](Int.MinValue);
    heap ++= (10 :: 98 :: 5 :: 123 :: 3 :: 52 :: 52 ::Nil)
     assert(heap.extractMin.get == 3)
     assert(heap.extractMin.get == 5)
     
     heap.decreaseKey(123,4)
     assert(heap.extractMin.get == 4)

     
     heap.decreaseKey(52,11)
     assert(heap.extractMin.get == 10)
     assert(heap.extractMin.get == 11)
     
     try{
       heap.decreaseKey(98,100)
     }catch{
	     case ex:java.lang.IllegalArgumentException => assert(true) 
      case ex : java.lang.AssertionError => assert(true)
	     case ex:java.lang.Exception  => assert(false," Some Exception : "+ex.getMessage())
     }
     
     heap.decreaseKey(98,0)
     assert(heap.extractMin.get == 0)
     
     assert(heap.minimum.get == 52)
   }


}
