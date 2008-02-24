package net.pragyah.scalgorithms.heaps

import scala.collection.mutable.{ListBuffer,HashMap}

/*
 * A binomial heap is a collection of binomial trees,
 * The binomial tree Bk consists of two binomial trees Bk-1 that are linked together: 
 * the root of one is the leftmost child of the root of the other
 *  
 */
class BinomialHeap[A <% Ordered[A]](val aMin:A) extends Heap[A] {

 
  var head:BHNode[A] = null
  
  val keyNodeMap = new HashMap[A,List[BHNode[A]]]()

  /*
   * min key should reside in a root node
   * since this method checks all roots ... which numbers at most lg n + 1 .... 
   * running time is O(lg n)
   */
  def minimum() : Option[A] = {
    var y:BHNode[A] = null
    var x = head
    if(x == null)
      None
    
    var min = x.key
    y = x
    while (x != null) {
      if(x.key < min) {
        min = x.key
        y = x
      }
      x = x ->
    }
    Some(y.key)
  }

  /*
   * this takes O(lg n) time as it makes a call to the union method that takes O(lg n) time
   * rest everhthing in this method is a constant time operation
   */
  def insert(a:A){
    val H_ = new BinomialHeap[A](aMin)
    val x = BHNode[A](a)
    x ^ null
    x.child = null
    x -> null
    x.degree = 0
    H_.head = x
    
    var e:List[BHNode[A]] = Nil
    if(keyNodeMap.contains(a)) e = keyNodeMap(a)
    keyNodeMap(a) = x :: e
    
    head = union(this,H_).head
  }
  
  /*
   * - find the minimum in the root list and remove it from the root list (searching for min aekst O(lg n) time
   * - get its children and add them to the root list of a new Heap after reversing the order 
   * - merge the two heaps (this and the new/temporary one)
   */
  def extractMin() : Option[A] = {
    if (head == null) return None
    
    //find the root x with the minimum key
    var x = head
    var x_prev:BHNode[A] = null
    
    var c = x
    var c_prev:BHNode[A] = null
    while(c != null){ // this takes log(n) time as there are as many root nodes
      if(c.key < x.key ){
        x = c
        x_prev = c_prev
      }
      c_prev = c
      c = c->
    }
    
    //remove x from the root list
    if(x_prev == null)
      head = x->
    else
      x_prev -> (x->)

    val H_ = new BinomialHeap[A](aMin)
    
    //get the children in reverse order ... 
    // Children of a node are in decreasing order of degree - from left to right
    // but the root list of the heap is in increasing order of degree .. remember? :)
    val xcr = x.children.reverse
    
    if(xcr != Nil){
	    H_.head = xcr.head
	    var y = xcr.head
        y ^ null
	    xcr.tail.foreach(z => {z ^ null; y -> z; y =z})
	    y -> null 
    }
    head = union(this,H_).head

    var l = keyNodeMap(x.key)
    l = l.remove(_ == x)
    if(l.length == 0) keyNodeMap -= x.key
    else keyNodeMap(x.key) = l
    
    Some(x.key)
  }
  
  /*
   * Running time O(lg n)
   */
  def delete(a:A){
    decreaseKey(a,aMin)
    extractMin()
  }
  
  /*    this method decreases the key from a to k ...
  *  Running time : O(lg n)
  */ 
  def decreaseKey(a:A,k:A) = {
    assume(keyNodeMap.contains(a)," Key does not exist in the heap : "+a)
    assume(k < a," New key is greater than the existing key")
    
    val x = keyNodeMap(a).head
    
    keyNodeMap(a) = keyNodeMap(a).tail // remove x from the list of nodes with key = a 
    if(keyNodeMap(a) == Nil) keyNodeMap -= a // if list contained only x ..then remove a from map altogether
    x.key = k

    var e:List[BHNode[A]] = Nil
    if(keyNodeMap.contains(k)) e = keyNodeMap(k)
    keyNodeMap(k) = x::e

    
    var y = x
    var z = y^
      
    while(z != null && y.key < z.key){
     val temp = y.key
     keyNodeMap(temp) = keyNodeMap(temp).remove(_ == y) // remove y from the list of y.key as its key is going to change 
     if(keyNodeMap(temp) == Nil) keyNodeMap -= temp 
     
     y.key = z.key
     keyNodeMap(z.key) = y::keyNodeMap(z.key).remove(_ == z) // remove z  and add y for z.key
     
     z.key = temp
     if(!keyNodeMap.contains(temp) || keyNodeMap.contains(temp) == Nil) keyNodeMap(temp) = z::Nil
     else keyNodeMap(temp) = z::keyNodeMap(temp)
     
     
     
     y = z
     z = y^
    } 
  }

  	/*   merges the rootlist of the two binomialHeaps ... if the root lists of H1 and H2 have m roots altogether ... 
	 * this method runs in 0(m) time by repeately examining the roots at the heads of the two root list and appending 
     * the root with the lower degree to the output root list, removing it from its input root list in the process
     */
   def merge(H1:BinomialHeap[A],H2:BinomialHeap[A]) : BHNode[A] = {
     var h:BHNode[A] = null
     var p:BHNode[A] = null
     
     var p1 = H1.head
     var p2 = H2.head
     
     //root list of H1 and H2 are sorted by strictly increasing degree .... 
     while(p1 != null || p2 != null){
       
       if(p1 == null) { if(h == null) h = p2 else p -> p2 ;p = p2; p2 = p2->} // if p1 is null or its degree is greater than p2's
       else if(p2 == null || p1.degree <= p2.degree) { if(h == null) h = p1 else p -> p1 ;p = p1; p1 = p1->} // if p2 is null or its degree is greater than p1's
       else { if(h == null) h = p2 else p -> p2 ;p = p2; p2 = p2-> }

     }
     
 
     H1.head = null
     H2.head = null
     h
   }
  /*
    * unites two heaps and consolidates them
    * running time is O(lg n) .. where n is the total number of nodes in H1 and H2
    * say H1 contains n1 nodes and H2 contains n2 nodes .. so n = n1+n2
    * H1 contains at most lg n1 +1 roots and H2 contains at most lg n2 + 1 roots
    * merge operation takes lag n1 + lg n2 time
    * so does the 'consolidation' code hence total running time is O(lg n)
    */ 
   def union(H1:BinomialHeap[A],H2:BinomialHeap[A]) : BinomialHeap[A] = {
     val H = new BinomialHeap[A](H1.aMin)
     H.head = merge(H1,H2)
     if(H.head == null) return H
     
     var prev_x:BHNode[A] = null
     var x = H.head
     var next_x = x->;
     
     while(next_x != null){
       if((x.degree != next_x.degree) || (next_x.sibling != null && next_x.sibling.degree == x.degree)){ // case 1 and 2 (CLRS) .. 
         // where either the degree are different for x and next_x ... OR ... the degree is same for x, next_x and next_x.sibling
         // in which case .. just shift right .. ignoring x .... marking next_x as new x and then dealing with the equality 
           // of the degree of the new x and new next_x 
         prev_x = x
         x = next_x
       }else if(x.key <= next_x.key){ // if  both x and next_x have the same degree ..AND x's key is lesser than next_x's 
         x -> next_x.sibling // skip next_x by joining x to next_x's sibling
         link(next_x,x) // make next_x a child of x
       }else { // if  both x and next_x have the same degree ..AND x's key is greater than next_x's
         if(prev_x == null){ // if x is the head ... may next_X as the new head as x is going to move under next-x now
           H.head = next_x
         }else{
           prev_x -> next_x // if x has a node left of it ... remove x from in-between prev_x and next_x
         }
         link(x,next_x) // make x a child of next_x
         x = next_x
       }
       next_x = x->
     }
     H
   }
   
   // makes y a child of z ... works in O(1) time
  def link[A <% Ordered[A]](y:BHNode[A],z:BHNode[A]) = {
    y ^ z
    y -> z.child
    z.child = y
    z.degree = z.degree+1
  }
     

  def mergeKeyNodeMap(keyNodeMap1:HashMap[A,List[BHNode[A]]],keyNodeMap2:HashMap[A,List[BHNode[A]]]): HashMap[A,List[BHNode[A]]] ={
    val knm = new HashMap[A,List[BHNode[A]]]()
    knm ++= keyNodeMap1
    
    keyNodeMap2.keys.foreach(k => {
                               if(knm(k) != null){
                                 knm(k) =knm(k):::keyNodeMap2(k)  
                               }else{
                                 knm(k) = keyNodeMap2(k)
                               }
                             })
    keyNodeMap1.clear
    keyNodeMap2.clear
    knm
  }
  

  override def toString() : String = {
    var str = "HEAP"
    if(head == null){
      str = str.concat(" EMPTY "); return str
    }
    var c = head
    str =str.concat("\n-").concat(c.toTreeString("\t",1))
    c = head->
      
    while(c != null){
      str =str.concat("\n-").concat(c.toTreeString("\t",1))
      c = c->
    }
    
    str
    
  }

}
 
//////////// NODE
object BHNode{
  def apply[A](key:A) = new BHNode(key)
}

class BHNode[A](var key:A){
   var degree = 0
   var parent:BHNode[A] = null
   var child:BHNode[A] = null
   var sibling:BHNode[A] = null

   override def toString = key.toString
   
   def -> = sibling
   def ->(s:BHNode[A]) = sibling = s
   def ^ = parent
   def ^(p:BHNode[A]) = parent = p
 
 
  def children() : List[BHNode[A]] = {
    var children = new ListBuffer[BHNode[A]]()
    
    var c =  child
    while(c != null){
      children += c
      c = c->
    }

    children.toList
  }

 def toTreeString(indent:String,indentLevel:int) : String = {
    var indentStr = indent
    for(i <- 1 to indentLevel) indentStr = indentStr.concat(indent)
    
    var str = indentStr.concat(key.toString)
    children.foreach(child => str = str.concat("\n").concat(child.toTreeString(indent,indentLevel+1)))
    str
  }

  
}
