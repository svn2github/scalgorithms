package net.pragyah.scalgorithms.dynamicprogramming

import scala.collection.mutable.{Set,HashSet,HashMap};

class LongestCommonSubsequence {
  
  def findSubsequence(str1:String, str2:String) : Set[String] = {
    
    val x = str1.toCharArray
    val y = str2.toCharArray
    
    val c = new Array[Array[int]](x.length+1,y.length+1)

    for(i <- 1 to x.length){
      for(j <- 1 to y.length){
        if(x(i-1) == y(j-1)){
          c(i)(j) = c(i-1)(j-1)+1
        }else{
          if(c(i-1)(j) >= c(i)(j-1)){
	          c(i)(j) = c(i-1)(j)
          }else{
	          c(i)(j) = c(i)(j-1)
          }
        }
      }
    }
    val lcs:Set[String] = new HashSet[String]()
    retrace(c,x.length,y.length,x,y).foreach(ll => lcs += new String(ll.reverse.toArray))
    lcs
  }
  
  val memo = new HashMap[(int,int),List[List[char]]]()
  
  private def retrace(c:Array[Array[int]],i:int,j:int,x:Array[char],y:Array[char]): List[List[char]] = {
    if(memo.contains(i,j)) {
      return memo(i,j)
    }
    
    
    if(i == 0 && j == 0) return Nil
    if(i == 0){
      return retrace(c,i,j-1,x,y)
    }else if(j == 0){
      return retrace(c,i-1,j,x,y)
    }
    
    if(c(i)(j) == c(i)(j-1) && c(i)(j) == c(i-1)(j)){
      val l = retrace(c,i-1,j,x,y) ::: retrace(c,i,j-1,x,y)
      memo += ((i,j)) -> l 
      return   l
    }else if (c(i)(j) == c(i-1)(j-1)+1 && c(i-1)(j-1) == c(i-1)(j) && c(i-1)(j-1) == c(i)(j-1)){
      var l =  retrace(c,i-1,j-1,x,y)
      if(l == Nil) 
        l = (x(i-1) :: Nil)::Nil
      else 
        l = l.flatMap(l => (x(i-1)::l)::Nil ) 
      memo += ((i,j)) -> l 
      return l
    }else if(c(i)(j) == c(i-1)(j) && c(i)(j) != c(i)(j-1)){
      val l =  retrace(c,i-1,j,x,y)
      memo += ((i,j)) -> l 
      return   l
    }else if(c(i)(j) == c(i)(j-1) && c(i)(j) != c(i-1)(j)){
      val l =  retrace(c,i,j-1,x,y)
      memo += ((i,j)) -> l 
      return   l
    }
    Nil
  }
  
}
