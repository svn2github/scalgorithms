package net.pragyah.scalgorithms.heaps

object Main {
  
  def main(args : Array[String]) : Unit = {
     //val heap = new FibonacciHeap[int](Int.MinValue);
     val heap = new BinomialHeap[int](Int.MinValue);
     heap.insert(10)
     heap.insert(98)
     heap.insert(5)
     heap.insert(123)
     heap.insert(3)
     heap.insert(52)
     heap.insert(52)
     heap.insert(52)
     heap.insert(34)
     heap.insert(1)
     heap.insert(22)
     heap.insert(4)
     heap.insert(6)
     
     println(heap)
     
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
    heap.decreaseKey(123,29)
          println(heap)
          
     println(heap.extractMin)
          println(heap)
     heap.delete(98)
          println(heap)

     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
     println(heap.extractMin)
          println(heap)
        
  }
  
}
