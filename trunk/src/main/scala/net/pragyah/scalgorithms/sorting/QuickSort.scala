package net.pragyah.scalgorithms.sorting

class QuickSort[A <% Ordered[A]] extends SortingAlgorithm[A] {
  
    def sort(l:List[A]): List[A] = {
      if(l.size <= 1) return l
      val pivot = l.head
      val (lt,gt) = l.tail.partition( _ <= pivot)
      sort(lt) ::: List(pivot) ::: sort(gt)
    }

}
