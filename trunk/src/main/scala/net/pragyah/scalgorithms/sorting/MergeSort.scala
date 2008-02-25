package net.pragyah.scalgorithms.sorting

class MergeSort[A <% Ordered[A]] extends SortingAlgorithm[A] {

  def sort(l:List[A]): List[A] = {
    if( l.length <= 1) return l
    merge(sort(l.dropRight(l.length/2)),sort(l.drop(l.length - l.length/2)))
  }
      
  def merge(l:List[A],r:List[A]) : List[A] = {
    var left = l
    var right = r
    var ret = List[A]()

    while(left != Nil || right != Nil){
      if(left == Nil){ ret = ret ::: right; right = Nil}
      else if(right == Nil){ret = ret ::: left; left = Nil}
      else if(left.head < right.head) {ret = ret :::left.takeWhile(_ <= right.head); left = left.dropWhile(_ <= right.head)}
      else {ret = ret :::right.takeWhile(_ <= left.head); right = right.dropWhile(_ <= left.head)}
    }
    ret
  }
}
